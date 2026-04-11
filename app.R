# ============================================================
# bs4Dash Dashboard (2 pages)
# 1) Map View: hover mini-card tooltip on points + top 4 KPIs
# ============================================================

library(shiny)
library(leaflet)
library(bs4Dash)
library(dplyr)
library(stringr)
library(htmltools)
library(ggplot2)
library(scales)

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

find_col <- function(nms, candidates) {
  norm <- function(s) tolower(gsub("[^a-z0-9]", "", s))
  nms_n <- norm(nms)
  cand_n <- norm(candidates)
  hit <- match(cand_n, nms_n)
  if (all(is.na(hit))) return(NULL)
  nms[hit[which(!is.na(hit))[1]]]
}

dis_bucket <- function(x) {
  v <- tolower(str_trim(as.character(x %||% "")))
  ifelse(v %in% c("yes","y","true","1"), "Yes",
         ifelse(v %in% c("no","n","false","0"), "No", "N/A"))
}

handover_bucket <- function(x) {
  v <- tolower(str_trim(as.character(x %||% "")))
  ifelse(v %in% c("handed over", "handover", "yes", "y", "true", "1"),
         "Handed Over",
         ifelse(v %in% c("not handed over", "no", "n", "false", "0"),
                "Not Handed Over", "Unknown"))
}

extract_first_number <- function(x) {
  s <- as.character(x)
  s <- str_replace_all(s, ",", "")
  m <- str_extract(s, "\\d+(?:\\.\\d+)?")
  suppressWarnings(as.numeric(m))
}

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
# LOAD DATA
# -----------------------------
file_path <- "data.csv"
raw <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
names(raw) <- str_replace_all(names(raw), "\\s+", "_")

status_col <- find_col(names(raw), c("Status", "Completion_Status", "Implementation_Status", "Progress_Status"))
handover_col <- find_col(names(raw), c("Handover_Status", "Handed_Over", "HandedOver", "Handover", "HandoverStatus"))
benef_col <- find_col(names(raw), c("Beneficiaries", "Beneficiaries_Reached", "People_Served", "Number_of_Beneficiaries"))
budget_col <- find_col(names(raw), c("Budget_Utilized_(ETB)", "Budget_Utilized_ETB", "Budget_ETB"))
dis_col <- find_col(names(raw), c("Disability_Access","Accessible","Accessibility"))
build_col <- find_col(names(raw), c("Year_Constructed","Year_Built","Built_Year"))

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

type_style_map <- build_type_style_map(df$Infrastructure_Type)

df <- df %>%
  left_join(type_style_map, by = "Infrastructure_Type") %>%
  mutate(
    Shape = ifelse(is.na(Shape), "circle", Shape),
    Color = ifelse(is.na(Color), "#2563eb", Color)
  )

# -----------------------------
# UI
# -----------------------------
ui <- bs4DashPage(
  title = "Ifaa SEI Infrastructure Dashboard",
  fullscreen = TRUE,
  
  header = bs4DashNavbar(
    title = "Ifaa SEI Infrastructure Dashboard",
    skin = "light",
    status = "white",
    border = TRUE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("sliders-h")
  ),
  
  sidebar = bs4DashSidebar(
    skin = "dark",
    status = "primary",
    brandColor = "primary",
    title = "Project Explorer",
    url = NULL,
    
    sidebarMenu(
      menuItem("Map View", tabName = "maptab", icon = icon("map-marked-alt")),
      br(),
      selectInput("type_filter", "Infrastructure Type", c("All", sort(unique(df$Infrastructure_Type)))),
      selectInput("woreda_filter", "Woreda", c("All", sort(unique(df$Woreda)))),
      hr(),
      tags$small(class = "text-muted",
                 "Filters affect the map view. Hover a point to see details.")
    )
  ),
  
  body = bs4DashBody(
    bs4TabItems(
      
      bs4TabItem(
        tabName = "maptab",
        
        fluidRow(
          valueBoxOutput("vb_total", width = 3),
          valueBoxOutput("vb_func", width = 3),
          valueBoxOutput("vb_dis", width = 3),
          valueBoxOutput("vb_types", width = 3)
        ),
        
        fluidRow(
          bs4Dash::box(
            title = "Infrastructure Locations (Hover a point for details)",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            height = "680px",
            leafletOutput("map", height = "620px")
          )
        )
      )
    )
  ),
  
  controlbar = bs4DashControlbar(
    skin = "light",
    title = "Notes",
    p("Map View: hover to see details.")
  ),
  
  footer = bs4DashFooter(
    left = "CRS Ifaa SEI — internal visualization",
    right = format(Sys.Date()),
    fixed = FALSE
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
  
  output$vb_total <- renderValueBox({
    d <- data_scope()
    valueBox(nrow(d), "Total Sites", icon = icon("map-marker-alt"), color = "primary")
  })
  
  output$vb_func <- renderValueBox({
    d <- data_scope()
    total <- nrow(d)
    n_func <- sum(d$Func_Class == "Functional", na.rm = TRUE)
    pct <- if (total == 0) 0 else round(100 * n_func / total, 1)
    valueBox(paste0(n_func, " (", pct, "%)"), "Functional", icon = icon("check-circle"), color = "success")
  })
  
  output$vb_dis <- renderValueBox({
    d <- data_scope()
    total <- nrow(d)
    n_yes <- sum(d$Disability_Access == "Yes", na.rm = TRUE)
    pct <- if (total == 0) 0 else round(100 * n_yes / total, 1)
    valueBox(paste0(n_yes, " (", pct, "%)"), "Disability Accessible", icon = icon("wheelchair"), color = "info")
  })
  
  output$vb_types <- renderValueBox({
    d <- data_scope()
    valueBox(dplyr::n_distinct(d$Infrastructure_Type), "Infrastructure Types",
             icon = icon("layer-group"), color = "warning")
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 38.7636, lat = 9.1450, zoom = 6) %>%
      addLegend(
        position = "bottomright",
        colors = type_style_map$Color,
        labels = type_style_map$Infrastructure_Type,
        title = "Infrastructure Type",
        opacity = 1
      )
  })
  
  observe({
    d <- data_scope()
    proxy <- leafletProxy("map", data = d) %>%
      clearMarkers() %>%
      clearPopups()
    
    if (nrow(d) == 0) return()
    
    icon_urls <- mapply(svg_icon_url, d$Shape, d$Color, USE.NAMES = FALSE)
    icn <- icons(iconUrl = icon_urls, iconWidth = 18, iconHeight = 18,
                 iconAnchorX = 9, iconAnchorY = 9)
    
    hover_html <- paste0(
      "<div>",
      "<img style='width:100%;height:140px;border-radius:12px;' src='", d$Image_URL, "'/>",
      "<div><b>", d$Name_of_Infrastructure, "</b></div>",
      "<div>", d$Infrastructure_Type, "</div>",
      "<div>", d$Woreda, "</div>",
      "<div>", d$Func_Class, "</div>",
      "</div>"
    )
    
    proxy %>%
      addMarkers(
        lng = ~Longitude, lat = ~Latitude,
        icon = icn,
        layerId = ~id,
        label = lapply(hover_html, HTML),
        labelOptions = labelOptions(sticky = TRUE)
      )
  })
}

shinyApp(ui, server)