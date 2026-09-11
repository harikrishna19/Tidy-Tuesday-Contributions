# ============================================================
# WORLD CASTLES EXPLORER
# TidyTuesday - 2026-09-01
# ============================================================

library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(readr)
library(stringr)
library(scales)
library(htmltools)

# ============================================================
# 1. LOAD DATA
# ============================================================

world_castles <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-01/world_castles.csv",
  show_col_types = FALSE
)

# ============================================================
# 2. CLEAN DATA
# ============================================================

world_castles <- world_castles %>%
  mutate(
    category = str_to_title(category),
    
    # Make a reasonable marker radius based on fame
    marker_size = case_when(
      fame_rank <= 10   ~ 12,
      fame_rank <= 50   ~ 10,
      fame_rank <= 100  ~ 8,
      fame_rank <= 500  ~ 6,
      TRUE              ~ 4
    ),
    
    # Safe display values
    year_display = case_when(
      is.na(year) ~ "Unknown",
      year < 0 ~ paste0(abs(year), " BC"),
      TRUE ~ as.character(as.integer(year))
    ),
    
    pageviews_display = comma(pageviews),
    
    sitelinks_display = comma(sitelinks)
  )

# ============================================================
# 3. CATEGORY PALETTE
# ============================================================

category_colors <- c(
  "Castle"   = "#C084FC",
  "Fortress" = "#F87171",
  "Palace"   = "#FBBF24",
  "Ruin"     = "#94A3B8"
)

category_pal <- colorFactor(
  palette = category_colors,
  domain = names(category_colors)
)

# ============================================================
# 4. TOP 10
# ============================================================

top10 <- world_castles %>%
  filter(!is.na(fame_rank)) %>%
  arrange(fame_rank) %>%
  slice_head(n = 10)

# ============================================================
# 5. POPUP FUNCTION
# ============================================================

castle_popup <- function(x) {
  
  image_html <- ifelse(
    is.na(x$image) | x$image == "",
    "",
    paste0(
      "<img src='", x$image, "'
           style='
             width:100%;
             height:180px;
             object-fit:cover;
             border-radius:10px;
             margin-bottom:12px;
           '
           onerror=\"this.style.display='none'\">"
    )
  )
  
  wikipedia_html <- ifelse(
    is.na(x$wikipedia) | x$wikipedia == "",
    "",
    paste0(
      "<a href='", x$wikipedia, "'
         target='_blank'
         style='
           display:inline-block;
           margin-top:12px;
           font-weight:700;
           text-decoration:none;
         '>
         Read on Wikipedia →
      </a>"
    )
  )
  
  paste0(
    "<div style='
      width:300px;
      font-family:Inter,Arial,sans-serif;
      line-height:1.45;
    '>",
    
    image_html,
    
    "<div style='
      font-size:19px;
      font-weight:800;
      margin-bottom:4px;
    '>",
    x$name,
    "</div>",
    
    "<div style='
      font-size:12px;
      text-transform:uppercase;
      letter-spacing:1px;
      font-weight:700;
      margin-bottom:12px;
    '>",
    x$category,
    " · ",
    x$country,
    "</div>",
    
    "<div style='
      display:grid;
      grid-template-columns:1fr 1fr;
      gap:8px;
      font-size:13px;
    '>",
    
    "<div>
       <b>Founded</b><br>",
    x$year_display,
    "</div>",
    
    "<div>
       <b>Fame rank</b><br>#",
    comma(x$fame_rank),
    "</div>",
    
    "<div>
       <b>Pageviews</b><br>",
    x$pageviews_display,
    "</div>",
    
    "<div>
       <b>Languages</b><br>",
    x$sitelinks_display,
    "</div>",
    
    "</div>",
    
    wikipedia_html,
    
    "</div>"
  )
}

# ============================================================
# 6. UI
# ============================================================

ui <- page_fillable(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    base_font = font_google("Inter"),
    heading_font = font_google("Manrope")
  ),
  
  # ----------------------------------------------------------
  # HEADER
  # ----------------------------------------------------------
  
  div(
    class = "app-header",
    
    div(
      class = "header-left",
      
      div(
        class = "eyebrow",
        "TIDYTUESDAY · 01 SEP 2026"
      ),
      
      h1(
        "The World's Castles"
      ),
      
      p(
        "Explore castles, fortresses, palaces and ruins across the globe."
      )
    ),
    
    div(
      class = "header-stat",
      
      div(
        class = "big-number",
        format(nrow(world_castles), big.mark = ",")
      ),
      
      div(
        class = "stat-label",
        "LANDMARKS"
      )
    ),
    
    div(
      class = "header-stat",
      
      div(
        class = "big-number",
        length(unique(world_castles$country))
      ),
      
      div(
        class = "stat-label",
        "COUNTRIES"
      )
    )
  ),
  
  # ----------------------------------------------------------
  # MAIN LAYOUT
  # ----------------------------------------------------------
  
  layout_sidebar(
    
    sidebar = sidebar(
      
      width = 310,
      
      h4(
        "Explore"
      ),
      
      textInput(
        "search",
        "Search landmark",
        placeholder = "e.g. Edinburgh Castle"
      ),
      
      selectizeInput(
        "country",
        "Country",
        choices = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "All countries",
          plugins = list("remove_button")
        )
      ),
      
      selectizeInput(
        "category",
        "Landmark type",
        choices = sort(unique(world_castles$category)),
        multiple = TRUE,
        selected = sort(unique(world_castles$category)),
        options = list(
          plugins = list("remove_button")
        )
      ),
      
      hr(),
      
      h5(
        "Map statistics"
      ),
      
      uiOutput(
        "map_stats"
      ),
      
      hr(),
      
      div(
        class = "sidebar-note",
        
        strong("How fame is measured"),
        
        p(
          "The fame rank combines Wikipedia language coverage "
          ,"and article readership. Rank #1 is the most famous landmark."
        )
      )
    ),
    
    # --------------------------------------------------------
    # MAP
    # --------------------------------------------------------
    
    div(
      class = "map-container",
      
      leafletOutput(
        "map",
        height = "calc(100vh - 120px)"
      ),
      
      # ------------------------------------------------------
      # TOP 10 PANEL
      # ------------------------------------------------------
      
      div(
        class = "top10-panel",
        
        div(
          class = "panel-kicker",
          "GLOBAL RANKING"
        ),
        
        h3(
          "Top 10 Famous"
        ),
        
        p(
          "Ranked by Wikipedia-based fame"
        ),
        
        uiOutput(
          "top10"
        )
      )
    )
  )
)

# ============================================================
# 7. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # COUNTRY SELECTOR
  # ----------------------------------------------------------
  
  updateSelectizeInput(
    session,
    "country",
    choices = sort(unique(world_castles$country)),
    server = TRUE
  )
  
  # ----------------------------------------------------------
  # FILTERED DATA
  # ----------------------------------------------------------
  
  filtered_data <- reactive({
    
    data <- world_castles
    
    # Search
    if (!is.null(input$search) &&
        input$search != "") {
      
      data <- data %>%
        filter(
          str_detect(
            str_to_lower(name),
            fixed(
              str_to_lower(input$search)
            )
          )
        )
    }
    
    # Country
    if (!is.null(input$country) &&
        length(input$country) > 0) {
      
      data <- data %>%
        filter(country %in% input$country)
    }
    
    # Category
    if (!is.null(input$category) &&
        length(input$category) > 0) {
      
      data <- data %>%
        filter(category %in% input$category)
    }
    
    data
  })
  
  # ==========================================================
  # MAP
  # ==========================================================
  
  output$map <- renderLeaflet({
    
    leaflet(
      world_castles
    ) %>%
      
      # ------------------------------------------------------
    # BASE MAP
    # ------------------------------------------------------
    
    addProviderTiles(
      providers$Esri.WorldStreetMap,
      group = "Street"
    ) %>%
      
      addProviderTiles(
        providers$CartoDB.DarkMatter,
        group = "Dark"
      ) %>%
      
      # ------------------------------------------------------
    # DEFAULT VIEW
    # ------------------------------------------------------
    
    setView(
      lng = 10,
      lat = 25,
      zoom = 2
    ) %>%
      
      # ------------------------------------------------------
    # LAYER CONTROL
    # ------------------------------------------------------
    
    addLayersControl(
      baseGroups = c(
        "Light",
        "Dark"
      ),
      options = layersControlOptions(
        collapsed = TRUE
      )
    ) %>%
      
      # ------------------------------------------------------
    # LEGEND
    # ------------------------------------------------------
    
    addLegend(
      position = "bottomleft",
      
      colors = unname(category_colors),
      
      labels = names(category_colors),
      
      title = "Landmark type",
      
      opacity = 0.9
    )
  })
  
  # ==========================================================
  # UPDATE MAP
  # ==========================================================
  
  observe({
    
    data <- filtered_data()
    
    proxy <- leafletProxy("map")
    
    proxy %>%
      clearMarkers() %>%
      clearMarkerClusters()
    
    if (nrow(data) == 0) {
      return()
    }
    
    # --------------------------------------------------------
    # ADD MARKERS
    # --------------------------------------------------------
    
    proxy %>%
      
      addCircleMarkers(
        
        data = data,
        
        lng = ~lon,
        lat = ~lat,
        
        radius = ~marker_size,
        
        fillColor = ~category_pal(category),
        
        fillOpacity = 0.75,
        
        color = "#FFFFFF",
        
        opacity = 0.8,
        
        weight = 0.7,
        
        popup = lapply(
          seq_len(nrow(data)),
          function(i) {
            HTML(
              castle_popup(data[i, ])
            )
          }
        ),
        
        label = ~name,
        
        labelOptions = labelOptions(
          direction = "auto",
          textsize = "12px"
        ),
        
        clusterOptions = markerClusterOptions(
          showCoverageOnHover = FALSE,
          zoomToBoundsOnClick = TRUE,
          spiderfyOnMaxZoom = TRUE,
          disableClusteringAtZoom = 7
        )
      )
  })
  
  # ==========================================================
  # MAP STATISTICS
  # ==========================================================
  
  output$map_stats <- renderUI({
    
    data <- filtered_data()
    
    div(
      
      class = "stats-grid",
      
      div(
        class = "mini-stat",
        span(
          class = "mini-number",
          format(nrow(data), big.mark = ",")
        ),
        span(
          class = "mini-label",
          "Landmarks"
        )
      ),
      
      div(
        class = "mini-stat",
        span(
          class = "mini-number",
          format(
            length(unique(data$country)),
            big.mark = ","
          )
        ),
        span(
          class = "mini-label",
          "Countries"
        )
      ),
      
      div(
        class = "mini-stat",
        span(
          class = "mini-number",
          format(
            sum(data$pageviews, na.rm = TRUE),
            big.mark = ","
          )
        ),
        span(
          class = "mini-label",
          "Pageviews"
        )
      )
    )
  })
  
  # ==========================================================
  # TOP 10 PANEL
  # ==========================================================
  
  output$top10 <- renderUI({
    
    items <- lapply(
      seq_len(nrow(top10)),
      function(i) {
        
        x <- top10[i, ]
        
        tags$a(
          
          href = x$wikipedia,
          
          target = "_blank",
          
          class = "top10-item",
          
          tags$div(
            class = "rank",
            paste0("#", x$fame_rank)
          ),
          
          tags$div(
            class = "top10-info",
            
            tags$div(
              class = "top10-name",
              x$name
            ),
            
            tags$div(
              class = "top10-country",
              paste(
                x$category,
                "·",
                x$country
              )
            )
          )
        )
      }
    )
    
    tagList(items)
  })
}

# ============================================================
# 8. CUSTOM CSS
# ============================================================

css <- "
<style>

/* ----------------------------------------------------------
   GLOBAL
---------------------------------------------------------- */

body {
  overflow: hidden;
}

/* ----------------------------------------------------------
   HEADER
---------------------------------------------------------- */

.app-header {

  height: 105px;

  display: flex;
  align-items: center;

  padding: 18px 30px;

  gap: 35px;

  border-bottom: 1px solid rgba(255,255,255,.08);

}

.header-left {
  flex: 1;
}

.eyebrow {

  font-size: 10px;

  font-weight: 800;

  letter-spacing: 2px;

  opacity: .55;

  margin-bottom: 3px;

}

.app-header h1 {

  margin: 0;

  font-size: 28px;

  font-weight: 800;

}

.app-header p {

  margin: 2px 0 0;

  opacity: .55;

  font-size: 13px;

}

.header-stat {

  min-width: 90px;

  text-align: right;

}

.big-number {

  font-size: 25px;

  font-weight: 800;

}

.stat-label {

  font-size: 9px;

  letter-spacing: 1.5px;

  opacity: .5;

  font-weight: 700;

}

/* ----------------------------------------------------------
   SIDEBAR
---------------------------------------------------------- */

.bslib-sidebar-layout {

  height: calc(100vh - 105px) !important;

}

.sidebar {

  border-right: 1px solid rgba(255,255,255,.08);

}

.sidebar h4 {

  font-weight: 800;

  margin-top: 0;

}

.sidebar h5 {

  font-weight: 700;

  font-size: 13px;

}

/* ----------------------------------------------------------
   MAP
---------------------------------------------------------- */

.map-container {

  position: relative;

  width: 100%;

  height: 100%;

}

.leaflet-container {

  background: #111827;

}

/* ----------------------------------------------------------
   TOP 10 PANEL
---------------------------------------------------------- */

.top10-panel {

  position: absolute;

  top: 20px;

  right: 20px;

  z-index: 1000;

  width: 310px;

  max-height: calc(100vh - 160px);

  overflow-y: auto;

  padding: 18px;

  border-radius: 14px;

  background: rgba(15,23,42,.94);

  backdrop-filter: blur(12px);

  box-shadow:
    0 12px 40px rgba(0,0,0,.35);

  border: 1px solid rgba(255,255,255,.10);

}

.panel-kicker {

  font-size: 9px;

  letter-spacing: 2px;

  font-weight: 800;

  opacity: .5;

}

.top10-panel h3 {

  margin: 2px 0;

  font-size: 22px;

  font-weight: 800;

}

.top10-panel > p {

  font-size: 11px;

  opacity: .5;

  margin-bottom: 12px;

}

/* ----------------------------------------------------------
   TOP 10 ITEM
---------------------------------------------------------- */

.top10-item {

  display: flex;

  align-items: center;

  gap: 12px;

  padding: 9px 6px;

  border-top: 1px solid rgba(255,255,255,.07);

  text-decoration: none;

  color: inherit;

  transition: .15s ease;

}

.top10-item:hover {

  background: rgba(255,255,255,.06);

  transform: translateX(3px);

  color: inherit;

}

.rank {

  width: 38px;

  font-size: 12px;

  font-weight: 800;

  opacity: .45;

}

.top10-info {

  flex: 1;

  min-width: 0;

}

.top10-name {

  font-size: 12px;

  font-weight: 700;

  white-space: nowrap;

  overflow: hidden;

  text-overflow: ellipsis;

}

.top10-country {

  font-size: 9px;

  opacity: .45;

  margin-top: 2px;

}

/* ----------------------------------------------------------
   STATS
---------------------------------------------------------- */

.stats-grid {

  display: grid;

  grid-template-columns: 1fr 1fr;

  gap: 8px;

}

.mini-stat {

  padding: 10px;

  border-radius: 8px;

  background: rgba(255,255,255,.04);

}

.mini-number {

  display: block;

  font-size: 16px;

  font-weight: 800;

}

.mini-label {

  display: block;

  font-size: 9px;

  text-transform: uppercase;

  letter-spacing: 1px;

  opacity: .45;

  margin-top: 2px;

}

/* ----------------------------------------------------------
   SIDEBAR NOTE
---------------------------------------------------------- */

.sidebar-note {

  font-size: 11px;

  line-height: 1.5;

  opacity: .55;

}

.sidebar-note strong {

  display: block;

  opacity: .9;

  margin-bottom: 4px;

}

/* ----------------------------------------------------------
   LEAFLET CONTROLS
---------------------------------------------------------- */

.leaflet-control-layers {

  border-radius: 8px !important;

  border: none !important;

}

.leaflet-popup-content-wrapper {

  border-radius: 12px;

}

.leaflet-popup-content {

  margin: 12px;

}

/* ----------------------------------------------------------
   MOBILE
---------------------------------------------------------- */

@media(max-width: 900px) {

  .top10-panel {

    width: 250px;

  }

  .header-stat {

    display: none;

  }

}

</style>
"

# Add CSS to UI
ui <- tagList(
  HTML(css),
  ui
)

# ============================================================
# 9. RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
