
# Option 2: Read directly from GitHub

world_castles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-01/world_castles.csv')
# ============================================================
# TidyTuesday 2026-09-01
# World Castles, Fortresses and Palaces
# Interactive Leaflet Map
# ============================================================

library(tidyverse)
library(leaflet)
library(htmltools)
library(scales)

# ------------------------------------------------------------
# 1. Load TidyTuesday data
# ------------------------------------------------------------

castles <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-01/world_castles.csv"
)

# ------------------------------------------------------------
# 2. Clean data
# ------------------------------------------------------------

castles <- castles %>%
  mutate(
    category = str_to_title(category),
    
    # Fame rank -> inverse score
    fame_score = 1 / fame_rank,
    
    # Scale marker size
    radius = rescale(
      log1p(pageviews),
      to = c(4, 15)
    ),
    
    popup = paste0(
      "<div style='
        width:280px;
        font-family:Arial,sans-serif;
        line-height:1.5;
      '>",
      
      "<h3 style='margin-bottom:5px;'>",
      name,
      "</h3>",
      
      "<b>Type:</b> ", category, "<br>",
      
      "<b>Country:</b> ", country, "<br>",
      
      "<b>Founded:</b> ",
      ifelse(
        is.na(year),
        "Unknown",
        format(year, big.mark = ",")
      ),
      "<br>",
      
      "<b>Wikipedia languages:</b> ",
      comma(sitelinks),
      "<br>",
      
      "<b>Pageviews:</b> ",
      comma(pageviews),
      "<br>",
      
      "<b>Fame rank:</b> #",
      comma(fame_rank),
      
      ifelse(
        !is.na(wikipedia),
        paste0(
          "<br><br>",
          "<a href='", wikipedia,
          "' target='_blank'>",
          "Read on Wikipedia →",
          "</a>"
        ),
        ""
      ),
      
      "</div>"
    )
  )

# ------------------------------------------------------------
# 3. Build map
# ------------------------------------------------------------

leaflet(castles) %>%
  
  # Base map
  addProviderTiles(
    providers$CartoDB.Positron,
    group = "Light"
  ) %>%
  
  addProviderTiles(
    providers$CartoDB.DarkMatter,
    group = "Dark"
  ) %>%
  
  # ----------------------------------------------------------
# Castles
# ----------------------------------------------------------

addCircleMarkers(
  lng = ~lon,
  lat = ~lat,
  
  radius = ~radius,
  
  fillColor = "#8B5CF6",
  fillOpacity = 0.65,
  
  color = "#FFFFFF",
  weight = 0.5,
  
  popup = ~popup,
  
  group = "Castles"
) %>%
  
  # ----------------------------------------------------------
# Fortress
# ----------------------------------------------------------

addCircleMarkers(
  data = filter(castles, category == "Fortress"),
  
  lng = ~lon,
  lat = ~lat,
  
  radius = ~radius,
  
  fillColor = "#EF4444",
  fillOpacity = 0.65,
  
  color = "#FFFFFF",
  weight = 0.5,
  
  popup = ~popup,
  
  group = "Fortresses"
) %>%
  
  # ----------------------------------------------------------
# Palaces
# ----------------------------------------------------------

addCircleMarkers(
  data = filter(castles, category == "Palace"),
  
  lng = ~lon,
  lat = ~lat,
  
  radius = ~radius,
  
  fillColor = "#F59E0B",
  fillOpacity = 0.65,
  
  color = "#FFFFFF",
  weight = 0.5,
  
  popup = ~popup,
  
  group = "Palaces"
) %>%
  
  # ----------------------------------------------------------
# Ruins
# ----------------------------------------------------------

addCircleMarkers(
  data = filter(castles, category == "Ruin"),
  
  lng = ~lon,
  lat = ~lat,
  
  radius = ~radius,
  
  fillColor = "#64748B",
  fillOpacity = 0.65,
  
  color = "#FFFFFF",
  weight = 0.5,
  
  popup = ~popup,
  
  group = "Ruins"
) %>%
  
  # ----------------------------------------------------------
# Layer control
# ----------------------------------------------------------

addLayersControl(
  baseGroups = c(
    "Light",
    "Dark"
  ),
  
  overlayGroups = c(
    "Castles",
    "Fortresses",
    "Palaces",
    "Ruins"
  ),
  
  options = layersControlOptions(
    collapsed = FALSE
  )
) %>%
  
  # ----------------------------------------------------------
# Legend
# ----------------------------------------------------------

addLegend(
  position = "bottomright",
  
  colors = c(
    "#8B5CF6",
    "#EF4444",
    "#F59E0B",
    "#64748B"
  ),
  
  labels = c(
    "Castle",
    "Fortress",
    "Palace",
    "Ruin"
  ),
  
  title = "Landmark type",
  
  opacity = 0.8
)
