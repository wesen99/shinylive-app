# ============================================================
# Shinydashboard Dashboard (2 pages)
# 1) Map View: hover mini-card tooltip on points + top 4 KPIs
# 2) Portfolio Summary: achievements + key metrics/charts/tables
# ============================================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(readxl)
library(stringr)
library(htmltools)
library(ggplot2)
library(scales)
library(DT)

# -----------------------------
# Helpers
# -----------------------------
to_num <- function(x) suppressWarnings(as.numeric(str_trim(as.character(x))))
`%||%` <- function(a, b) if (!is.null(a)) a else b

norm_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- str_replace_all(x, "\\s+", " ")
  x
}

extract_year <- function(x) {
  y <- str_extract(as.character(x), "\\b(19|20)\\d{2}\\b")
  suppressWarnings(as.integer(y))
}

# Robust column finder (case/underscore/space insensitive)
find_col <- function(nms, candidates) {
  norm <- function(s) tolower(gsub("[^a-z0-9]", "", s))
  nms_n <- norm(nms)
  cand_n <- norm(candidates)
  hit <- match(cand_n, nms_n)
  if (all(is.na(hit))) return(NULL)
  nms[hit[which(!is.na(hit))[1]]]
}

# disability normalization
dis_bucket <- function(x) {
  v <- tolower(str_trim(as.character(x %||% "")))
  ifelse(v %in% c("yes","y","true","1"), "Yes",
         ifelse(v %in% c("no","n","false","0"), "No", "N/A"))
}

# handover normalization
handover_bucket <- function(x) {
  v <- tolower(str_trim(as.character(x %||% "")))
  ifelse(v %in% c("handed over", "handover", "yes", "y", "true", "1"),
         "Handed Over",
         ifelse(v %in% c("not handed over", "no", "n", "false", "0"),
                "Not Handed Over", "Unknown"))
}

# Extract first numeric from beneficiary text ("9,935 PSNP Clients" -> 9935)
extract_first_number <- function(x) {
  s <- as.character(x)
  s <- str_replace_all(s, ",", "")
  m <- str_extract(s, "\\d+(?:\\.\\d+)?")
  suppressWarnings(as.numeric(m))
}

# ---- Small SVG icon (shape + color) for markers
svg_icon_url <- function(shape = "circle", fill = "#2563eb") {
  svg <- switch(
    shape,
    circle = sprintf("<svg xmlns='http://www.w3.org/2000/svg' width='18' height='18' viewBox='0 0 18 18'><circle cx='9' cy='9' r='6.2' fill='%s' stroke='white' stroke-width='2'/></svg>", fill),
    square = sprintf("<svg xmlns='http://www.w3.org/2000/svg' width='18' height='18' viewBox='0 0 18 18'><rect x='3.2' y='3.2' width='11.6' height='11.6' rx='2' fill='%s' stroke='white' stroke-width='2'/></svg>", fill),
    triangle = sprintf("<svg xmlns='http://www.w3.org/2000/svg' width='18' height='18' viewBox='0 0 18 18'><polygon points='9,2.8 15.4,14.8 2.6,14.8' fill='%s' stroke='white' stroke-width='2'/></svg>", fill),
    diamond = sprintf("<svg xmlns='http://www.w3.org/2000/svg' width='18' height='18' viewBox='0 0 18 18'><polygon points='9,2.6 15.4,9 9,15.4 2.6,9' fill='%s' stroke='white' stroke-width='2'/></svg>", fill),
    star = sprintf("<svg xmlns='http://www.w3.org/2000/svg' width='18' height='18' viewBox='0 0 18 18'><polygon points='9,2.2 11.1,6.8 16.2,7.2 12.4,10.4 13.6,15.4 9,12.8 4.4,15.4 5.6,10.4 1.8,7.2 6.9,6.8' fill='%s' stroke='white' stroke-width='1.8'/></svg>", fill),
    hex = sprintf("<svg xmlns='http://www.w3.org/2000/svg' width='18' height='18' viewBox='0 0 18 18'><polygon points='9,2.4 14.6,5.8 14.6,12.2 9,15.6 3.4,12.2 3.4,5.8' fill='%s' stroke='white' stroke-width='2'/></svg>", fill),
    sprintf("<svg xmlns='http://www.w3.org/2000/svg' width='18' height='18' viewBox='0 0 18 18'><circle cx='9' cy='9' r='6.2' fill='%s' stroke='white' stroke-width='2'/></svg>", fill)
  )
  paste0("data:image/svg+xml;charset=UTF-8,", URLencode(svg, reserved = TRUE))
}

# Create deterministic mapping of types -> shape+color
build_type_style_map <- function(types) {
  shape_pool <- c("circle", "square", "triangle", "diamond", "star", "hex")
  color_pool <- c("#2563eb", "#16a34a", "#f59e0b", "#dc2626", "#7c3aed", "#0891b2", "#0ea5e9", "#64748b")
  types <- sort(unique(na.omit(types)))
  data.frame(
    Infrastructure_Type = types,
    Shape = shape_pool[(seq_along(types) - 1) %% length(shape_pool) + 1],
    Color = color_pool[(seq_along(types) - 1) %% length(color_pool) + 1],
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# LOAD DATA (Excel or CSV)
# -----------------------------
# Create sample data if file doesn't exist
if (!file.exists("data.csv")) {
  # Generate sample data for testing
  set.seed(123)
  sample_data <- data.frame(
    Latitude = runif(50, 8, 12),
    Longitude = runif(50, 36, 40),
    Infrastructure_Type = sample(c("Health Center", "School", "Water Point", "Road"), 50, replace = TRUE),
    Name_of_Infrastructure = paste("Infrastructure", 1:50),
    Woreda = sample(c("Woreda A", "Woreda B", "Woreda C", "Woreda D"), 50, replace = TRUE),
    Functionality_Status = sample(c("Functional", "Not Functional"), 50, replace = TRUE, prob = c(0.8, 0.2)),
    Status = sample(c("Completed", "Ongoing", "Planned"), 50, replace = TRUE),
    Handover_Status = sample(c("Handed Over", "Not Handed Over"), 50, replace = TRUE, prob = c(0.7, 0.3)),
    Disability_Access = sample(c("Yes", "No"), 50, replace = TRUE, prob = c(0.6, 0.4)),
    Beneficiaries = paste(sample(100:5000, 50, replace = TRUE), "people"),
    Budget_Utilized_ETB = sample(10000:500000, 50, replace = TRUE),
    Year_Constructed = sample(2010:2024, 50, replace = TRUE)
  )
  write.csv(sample_data, "data.csv", row.names = FALSE)
}

file_path <- "data.csv"

raw <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)

names(raw) <- str_replace_all(names(raw), "\\s+", "_")

# detect optional columns
status_col <- find_col(names(raw), c("Status", "Completion_Status", "Implementation_Status", "Progress_Status"))
handover_col <- find_col(names(raw), c("Handover_Status", "Handed_Over", "HandedOver", "Handover", "HandoverStatus"))
benef_col <- find_col(names(raw), c("Beneficiaries", "Beneficiaries_Reached", "Beneficiaries_Served", "People_Served", "HHs_Served", "Number_of_Beneficiaries"))
budget_col <- find_col(names(raw), c("Budget_Utilized_(ETB)", "Budget_Utilized_ETB", "Budget_Utilized", "Budget_ETB", "Budget_(ETB)"))
dis_col <- find_col(names(raw), c("Disability_Access","Disability_Access_(No/Yes)","PWD_Access","Disability_Friendly","Accessible","Accessibility"))

build_col <- find_col(names(raw), c("Year_Constructed","Year_Built","Date_Built","Construction_Date","Built_Year","Year_of_Construction"))

df <- raw %>%
  mutate(
    id = row_number(),
    Latitude = to_num(.data[["Latitude"]]),
    Longitude = to_num(.data[["Longitude"]]),
    Infrastructure_Type = str_squish(as.character(.data[["Infrastructure_Type"]])),
    Name_of_Infrastructure = str_squish(as.character(.data[["Name_of_Infrastructure"]])),
    Woreda = str_squish(as.character(.data[["Woreda"]])),
    Woreda_key = norm_name(Woreda),
    
    Func_Class = ifelse(
      tolower(str_trim(as.character(.data[["Functionality_Status"]]))) %in% c("functional", "yes"),
      "Functional", "Not Functional"
    ),
    
    Status = if (!is.null(status_col)) str_squish(as.character(.data[[status_col]])) else NA_character_,
    Handover_Status = if (!is.null(handover_col)) str_squish(as.character(.data[[handover_col]])) else NA_character_,
    Handover_Class = handover_bucket(Handover_Status),
    
    Disability_Access = if (!is.null(dis_col)) dis_bucket(.data[[dis_col]]) else "N/A",
    
    Beneficiary_Raw = if (!is.null(benef_col)) str_squish(as.character(.data[[benef_col]])) else NA_character_,
    Beneficiary_Num = extract_first_number(Beneficiary_Raw),
    Beneficiary_IsNumeric = !is.na(Beneficiary_Num),
    
    Budget_ETB = if (!is.null(budget_col)) to_num(.data[[budget_col]]) else NA_real_,
    
    Built_Raw = if (!is.null(build_col)) str_squish(as.character(.data[[build_col]])) else NA_character_,
    Built_Year = extract_year(Built_Raw),
    Built_Display = ifelse(!is.na(Built_Year), as.character(Built_Year),
                           ifelse(!is.na(Built_Raw) & nzchar(Built_Raw), Built_Raw, "—")),
    
    Image_URL = paste0("https://picsum.photos/seed/", id + 100, "/500/300")
  ) %>%
  filter(!is.na(Latitude), !is.na(Longitude))

types_all <- sort(unique(df$Infrastructure_Type))
woredas  <- sort(unique(df$Woreda))

# attach shape/color mapping
type_style_map <- build_type_style_map(df$Infrastructure_Type)
df <- df %>%
  left_join(type_style_map, by = "Infrastructure_Type") %>%
  mutate(
    Shape = ifelse(is.na(Shape), "circle", Shape),
    Color = ifelse(is.na(Color), "#2563eb", Color)
  )

# zoom
bb_all <- df %>%
  summarise(
    xmin = min(Longitude, na.rm = TRUE),
    xmax = max(Longitude, na.rm = TRUE),
    ymin = min(Latitude, na.rm = TRUE),
    ymax = max(Latitude, na.rm = TRUE)
  )

# -----------------------------
# UI (shinydashboard with 2 pages)
# -----------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Ifaa SEI Infrastructure Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map View", tabName = "maptab", icon = icon("map-marked-alt")),
      menuItem("Portfolio Summary", tabName = "summarytab", icon = icon("chart-bar")),
      br(),
      selectInput("type_filter", "Infrastructure Type", c("All", types_all)),
      selectInput("woreda_filter", "Woreda", c("All", woredas)),
      hr(),
      tags$small(class = "text-muted",
                 "Filters affect both pages. Hover a point on the map to see details.")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
    .content-wrapper, .right-side { background: #f4f6f9; }
    
    /* =========================
       COMPACT HOVER CARD
       ========================= */
    
    .leaflet-tooltip.mapHoverCard {
      background: rgba(255,255,255,0.98);
      border: 1px solid rgba(0,0,0,.08);
      border-radius: 10px;
      box-shadow: 0 6px 16px rgba(0,0,0,.12);
      padding: 0;
      max-width: 220px;
      min-width: 200px;
      white-space: normal;
      overflow-wrap: anywhere;
    }
    
    /* image - SMALLER */
    .hoverImg {
      width: 100%;
      height: 80px;
      object-fit: cover;
      border-radius: 10px 10px 0 0;
      border: 1px solid rgba(0,0,0,.06);
    }
    
    /* content - COMPACT */
    .hoverContent {
      background: #ffffff;
      padding: 6px 8px;
    }
    
    .hoverCardTitle {
      font-weight: 700;
      font-size: 11px;
      color: #0f172a;
      margin-bottom: 2px;
      line-height: 1.2;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    
    .hoverCardType {
      font-size: 9px;
      color: #64748b;
      margin-bottom: 4px;
    }
    
    /* compact meta grid - TIGHTER */
    .hoverMeta {
      font-size: 9px;
      color: #334155;
      line-height: 1.3;
    }
    
    .hoverMeta div {
      margin-bottom: 2px;
    }
    
    .hoverMeta span {
      font-weight: 600;
      color: #64748b;
    }
    
    /* remove extra spacing */
    .hoverRow {
      display: flex;
      flex-direction: column;
      gap: 0;
    }
    
    /* box styling */
    .box {
      border-radius: 10px;
    }
  "))
    ),
    
    tabItems(
      # ======================
      # TAB 1: MAP VIEW
      # ======================
      tabItem(
        tabName = "maptab",
        
        # top KPIs (4)
        fluidRow(
          valueBoxOutput("vb_total", width = 3),
          valueBoxOutput("vb_func", width = 3),
          valueBoxOutput("vb_dis", width = 3),
          valueBoxOutput("vb_types", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Infrastructure Locations (Hover a point for details)",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            height = "680px",
            leafletOutput("map", height = "620px")
          )
        )
      ),
      
      # ======================
      # TAB 2: PORTFOLIO SUMMARY
      # ======================
      tabItem(
        tabName = "summarytab",
        
        fluidRow(
          valueBoxOutput("kpi_infra_total", width = 3),
          valueBoxOutput("kpi_completion_rate", width = 3),
          valueBoxOutput("kpi_functionality_rate", width = 3),
          valueBoxOutput("kpi_budget_total", width = 3)
        ),
        fluidRow(
          valueBoxOutput("kpi_handover_not", width = 3),
          valueBoxOutput("kpi_benef_total", width = 3),
          valueBoxOutput("kpi_benef_missing", width = 3),
          valueBoxOutput("kpi_avg_cost", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Infrastructure Type Distribution",
            width = 6, status = "info", solidHeader = TRUE,
            plotOutput("plt_type_dist", height = "320px")
          ),
          box(
            title = "Status Distribution (Delivery Progress)",
            width = 6, status = "info", solidHeader = TRUE,
            plotOutput("plt_status_dist", height = "320px")
          )
        ),
        
        fluidRow(
          box(
            title = "Functionality Distribution",
            width = 6, status = "success", solidHeader = TRUE,
            plotOutput("plt_func_dist", height = "320px")
          ),
          box(
            title = "Budget by Infrastructure Type",
            width = 6, status = "warning", solidHeader = TRUE,
            plotOutput("plt_budget_by_type", height = "320px")
          )
        ),
        
        fluidRow(
          box(
            title = "Not Handed Over Infrastructures",
            width = 12, status = "danger", solidHeader = TRUE,
            DTOutput("tbl_not_handed")
          )
        ),
        
        fluidRow(
          box(
            title = "Top 10 Beneficiaries",
            width = 12, status = "success", solidHeader = TRUE,
            DTOutput("tbl_top_benef")
          )
        )
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session){
  
  data_scope <- reactive({
    x <- df
    if (!is.null(input$type_filter) && input$type_filter != "All")
      x <- x %>% filter(Infrastructure_Type == input$type_filter)
    if (!is.null(input$woreda_filter) && input$woreda_filter != "All")
      x <- x %>% filter(Woreda == input$woreda_filter)
    x
  })
  
  # ======================
  # KPIs (MAP TAB — 4)
  # ======================
  output$vb_total <- renderValueBox({
    d <- data_scope()
    valueBox(
      value = nrow(d), 
      subtitle = "Total Sites", 
      icon = icon("map-marker-alt"), 
      color = "blue"
    )
  })
  
  output$vb_func <- renderValueBox({
    d <- data_scope()
    total <- nrow(d)
    n_func <- sum(d$Func_Class == "Functional", na.rm = TRUE)
    pct <- if (total == 0) 0 else round(100 * n_func / total, 1)
    valueBox(
      value = paste0(n_func, " (", pct, "%)"), 
      subtitle = "Functional", 
      icon = icon("check-circle"), 
      color = "green"
    )
  })
  
  output$vb_dis <- renderValueBox({
    d <- data_scope()
    total <- nrow(d)
    n_yes <- sum(d$Disability_Access == "Yes", na.rm = TRUE)
    pct <- if (total == 0) 0 else round(100 * n_yes / total, 1)
    valueBox(
      value = paste0(n_yes, " (", pct, "%)"), 
      subtitle = "Disability Accessible", 
      icon = icon("wheelchair"), 
      color = "light-blue"
    )
  })
  
  output$vb_types <- renderValueBox({
    d <- data_scope()
    valueBox(
      value = dplyr::n_distinct(d$Infrastructure_Type), 
      subtitle = "Infrastructure Types",
      icon = icon("layer-group"), 
      color = "yellow"
    )
  })
  
  # ======================
  # MAP
  # ======================
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(
        lng1 = bb_all$xmin,
        lat1 = bb_all$ymin,
        lng2 = bb_all$xmax,
        lat2 = bb_all$ymax
      ) %>%
      addControl(
        html = "
      <div style='background:white;padding:10px 12px;border-radius:10px;
                  box-shadow:0 2px 10px rgba(0,0,0,0.15);
                  font-size:12px; line-height:1.4; max-width:220px;'>
        <b>Map Key</b><br/>
        • Each point = Infrastructure site<br/>
        • Hover = details card<br/>
        • Click = popup view<br/><br/>
        <b>Filters:</b><br/>
        • Infrastructure Type<br/>
        • Woreda
      </div>
      ",
        position = "topleft"
      ) %>%
      addLegend(
        position = "bottomright",
        colors = type_style_map$Color,
        labels = type_style_map$Infrastructure_Type,
        title = "Infrastructure Type",
        opacity = 1
      )
  })
  
  # points with hover tooltip
  observe({
    d <- data_scope()
    proxy <- leafletProxy("map", data = d) %>%
      clearMarkers() %>%
      clearPopups()
    if (nrow(d) == 0) return()
    
    # per-type count within current scope
    type_counts <- d %>% count(Infrastructure_Type, name = "Type_Count")
    d2 <- d %>% left_join(type_counts, by = "Infrastructure_Type")
    
    icon_urls <- mapply(svg_icon_url, d2$Shape, d2$Color, USE.NAMES = FALSE)
    icn <- icons(iconUrl = icon_urls, iconWidth = 18, iconHeight = 18, iconAnchorX = 9, iconAnchorY = 9)
    
    hover_html <- paste0(
      "<div style='min-width:320px; max-width:380px;'>",
      "<div class='hoverRow'>",
      "<img class='hoverImg' src='", d2$Image_URL, "'/>",
      "<div class='hoverContent'>",
      "<div class='hoverCardTitle'>", htmlEscape(d2$Name_of_Infrastructure), "</div>",
      "<div class='hoverCardType'>", htmlEscape(d2$Infrastructure_Type), "</div>",
      "<div class='hoverMeta'>",
      "<div><span>Woreda:</span> ", htmlEscape(d2$Woreda), "</div>",
      "<div><span>Status:</span> ", htmlEscape(d2$Func_Class), "</div>",
      "<div><span>Disability:</span> ", htmlEscape(d2$Disability_Access), "</div>",
      "<div><span>Built:</span> ", htmlEscape(d2$Built_Display), "</div>",
      "<div><span>Type Count:</span> ", d2$Type_Count, "</div>",
      "</div>",
      "</div>",
      "</div>",
      "</div>"
    )
    
    proxy %>%
      addMarkers(
        lng = ~Longitude, lat = ~Latitude,
        icon = icn,
        layerId = ~id,
        label = lapply(hover_html, HTML),
        labelOptions = labelOptions(
          direction = "auto",
          offset = c(0, -10),
          textsize = "12px",
          sticky = TRUE,
          className = "mapHoverCard"
        ),
        popup = lapply(hover_html, HTML),
        options = markerOptions(riseOnHover = TRUE)
      )
  })
  
  # ======================
  # PORTFOLIO SUMMARY TAB
  # ======================
  
  output$kpi_infra_total <- renderValueBox({
    d <- data_scope()
    n_infra <- if ("Name_of_Infrastructure" %in% names(d)) dplyr::n_distinct(d$Name_of_Infrastructure) else nrow(d)
    valueBox(
      value = n_infra, 
      subtitle = "Total Infrastructures", 
      icon = icon("industry"), 
      color = "blue"
    )
  })
  
  output$kpi_completion_rate <- renderValueBox({
    d <- data_scope()
    if (all(is.na(d$Status))) {
      valueBox("N/A", "Completion Rate", icon = icon("tasks"), color = "light-blue")
    } else {
      total <- nrow(d)
      completed <- sum(tolower(d$Status) == "completed", na.rm = TRUE)
      pct <- if (total == 0) 0 else round(100 * completed / total, 1)
      valueBox(paste0(pct, "%"), "Completion Rate", icon = icon("tasks"), color = "light-blue")
    }
  })
  
  output$kpi_functionality_rate <- renderValueBox({
    d <- data_scope()
    total <- nrow(d)
    functional <- sum(d$Func_Class == "Functional", na.rm = TRUE)
    pct <- if (total == 0) 0 else round(100 * functional / total, 1)
    valueBox(paste0(pct, "%"), "Functionality Rate", icon = icon("check-double"), color = "green")
  })
  
  output$kpi_handover_not <- renderValueBox({
    d <- data_scope()
    not_ho <- sum(d$Handover_Class == "Not Handed Over", na.rm = TRUE)
    valueBox(not_ho, "Not Handed Over", icon = icon("handshake-slash"), color = "red")
  })
  
  output$kpi_benef_total <- renderValueBox({
    d <- data_scope()
    tot <- sum(d$Beneficiary_Num, na.rm = TRUE)
    valueBox(comma(tot), "Beneficiaries (Numeric)", icon = icon("users"), color = "green")
  })
  
  output$kpi_benef_missing <- renderValueBox({
    d <- data_scope()
    miss <- sum(!d$Beneficiary_IsNumeric | is.na(d$Beneficiary_Raw) | !nzchar(d$Beneficiary_Raw), na.rm = TRUE)
    valueBox(miss, "Beneficiary Missing/Qualitative", icon = icon("question-circle"), color = "yellow")
  })
  
  output$kpi_budget_total <- renderValueBox({
    d <- data_scope()
    tot <- sum(d$Budget_ETB, na.rm = TRUE)
    valueBox(comma(tot), "Total Budget Utilized (ETB)", icon = icon("money-bill-wave"), color = "yellow")
  })
  
  output$kpi_avg_cost <- renderValueBox({
    d <- data_scope()
    b <- d$Budget_ETB
    n_budget <- sum(!is.na(b))
    avg <- if (n_budget == 0) NA_real_ else sum(b, na.rm = TRUE) / n_budget
    med <- if (n_budget == 0) NA_real_ else median(b, na.rm = TRUE)
    valueBox(
      value = if (is.na(avg)) "N/A" else paste0("Avg: ", comma(round(avg))),
      subtitle = if (is.na(med)) "Cost per Infrastructure" else paste0("Cost per Infrastructure (Median: ", comma(round(med)), ")"),
      icon = icon("calculator"),
      color = "light-blue"
    )
  })
  
  # ---- Plots ----
  output$plt_type_dist <- renderPlot({
    d <- data_scope()
    req(nrow(d) > 0)
    dfm <- d %>%
      count(Infrastructure_Type, sort = TRUE) %>%
      mutate(pct = n / sum(n), lab = paste0(n, " (", percent(pct, accuracy = 0.1), ")")) %>%
      mutate(Infrastructure_Type = factor(Infrastructure_Type, levels = Infrastructure_Type))
    
    ggplot(dfm, aes(x = Infrastructure_Type, y = n)) +
      geom_col(fill = "#2563eb", width = 0.75) +
      coord_flip() +
      labs(x = NULL, y = "Count") +
      theme_minimal(base_size = 11)
  })
  
  output$plt_status_dist <- renderPlot({
    d <- data_scope()
    req(nrow(d) > 0)
    if (all(is.na(d$Status))) {
      ggplot() + theme_void() +
        annotate("text", x = 0, y = 0, label = "Status column not available for this dataset")
    } else {
      dfm <- d %>%
        mutate(Status = ifelse(is.na(Status) | !nzchar(Status), "Unknown", Status)) %>%
        count(Status, sort = TRUE) %>%
        mutate(Status = factor(Status, levels = Status))
      
      ggplot(dfm, aes(x = Status, y = n)) +
        geom_col(fill = "#0891b2", width = 0.75) +
        coord_flip() +
        labs(x = NULL, y = "Count") +
        theme_minimal(base_size = 11)
    }
  })
  
  output$plt_func_dist <- renderPlot({
    d <- data_scope()
    req(nrow(d) > 0)
    dfm <- d %>%
      count(Func_Class, sort = TRUE) %>%
      mutate(Func_Class = factor(Func_Class, levels = Func_Class))
    
    ggplot(dfm, aes(x = Func_Class, y = n, fill = Func_Class)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      scale_fill_manual(values = c("Functional" = "#16a34a", "Not Functional" = "#dc2626")) +
      labs(x = NULL, y = "Count") +
      theme_minimal(base_size = 11)
  })
  
  output$plt_budget_by_type <- renderPlot({
    d <- data_scope()
    req(nrow(d) > 0)
    if (all(is.na(d$Budget_ETB))) {
      ggplot() + theme_void() +
        annotate("text", x = 0, y = 0, label = "Budget column not available or empty")
    } else {
      dfm <- d %>%
        group_by(Infrastructure_Type) %>%
        summarise(Budget = sum(Budget_ETB, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(Budget)) %>%
        mutate(Infrastructure_Type = factor(Infrastructure_Type, levels = Infrastructure_Type))
      
      ggplot(dfm, aes(x = Infrastructure_Type, y = Budget)) +
        geom_col(fill = "#f59e0b", width = 0.75) +
        coord_flip() +
        scale_y_continuous(labels = comma) +
        labs(x = NULL, y = "ETB") +
        theme_minimal(base_size = 11)
    }
  })
  
  # ---- Tables ----
  output$tbl_not_handed <- renderDT({
    d <- data_scope()
    ex <- d %>%
      filter(Handover_Class == "Not Handed Over") %>%
      transmute(
        Name = Name_of_Infrastructure,
        Type = Infrastructure_Type,
        Woreda = Woreda,
        Status = Func_Class,
        Handover = Handover_Status
      )
    
    datatable(
      ex,
      options = list(pageLength = 6, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$tbl_top_benef <- renderDT({
    d <- data_scope()
    top10 <- d %>%
      filter(!is.na(Beneficiary_Num)) %>%
      arrange(desc(Beneficiary_Num)) %>%
      transmute(
        Name = Name_of_Infrastructure,
        Type = Infrastructure_Type,
        Woreda = Woreda,
        Beneficiaries = Beneficiary_Num,
        Beneficiary_Text = Beneficiary_Raw
      ) %>%
      slice_head(n = 10)
    
    datatable(
      top10,
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

# Run the app
shinyApp(ui, server)