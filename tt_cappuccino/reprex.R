# ===============================================================
# COFFEE-COUNTRIES
# TidyTuesday — 2026-09-08
#
# STORY
#   How much of a barista's working day goes into one cappuccino?
#   And what does the café sample behind the index actually look like?
#
# DESIGN
#
#   COFFEE-COUNTRIES ☕
#
#   MIN → MEAN → MEDIAN → MAX, PER REGION
#
#   AFRICA       MAP       [ MIN ][ MEAN ][ MEDIAN ][ MAX ]
#
#                 ↓ SPACE
#
#   ASIA         MAP       [ MIN ][ MEAN ][ MEDIAN ][ MAX ]
#
#                 ↓ SPACE
#
#   EUROPE       MAP       [ MIN ][ MEAN ][ MEDIAN ][ MAX ]
#
#   ...
#
#   CAFÉS IN NUMBERS
#
# ===============================================================


# ---------------------------------------------------------------
# PACKAGES
# ---------------------------------------------------------------

library(grid)
library(dplyr)
library(tidyr)
library(tibble)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(countrycode)
library(readr)
library(showtext)


# ===============================================================
# 1. FONTS
# ===============================================================

font_add_google(
  "Bebas Neue",
  "bebas"
)

font_add_google(
  "Oswald",
  "oswald"
)
font_add_google(
  "Inter",
  "inter"
)

showtext_auto()


# ===============================================================
# 2. LOAD DATA
# ===============================================================

cappuccino_index <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-08/cappuccino_index.csv",
  show_col_types = FALSE
)

cafe <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-08/cafe.csv",
  show_col_types = FALSE
)


# ===============================================================
# 3. CLEAN DATA
# ===============================================================

cappuccino_index <- cappuccino_index %>%
  mutate(
    country = gsub("\u00a0", " ", country)
  )

cafe <- cafe %>%
  mutate(
    country = gsub("\u00a0", " ", country),
    city = na_if(city, "NA")
  )


# ===============================================================
# 4. COUNTRY METADATA
# ===============================================================

cappuccino_index <- cappuccino_index %>%
  mutate(
    continent = countrycode(
      country,
      "country.name",
      "continent"
    ),
    iso3 = countrycode(
      country,
      "country.name",
      "iso3c"
    )
  )

cafe <- cafe %>%
  mutate(
    continent = countrycode(
      country,
      "country.name",
      "continent"
    ),
    iso3 = countrycode(
      country,
      "country.name",
      "iso3c"
    )
  )


# ===============================================================
# 5. CAFÉ-LEVEL AFFORDABILITY
# ===============================================================

cafe <- cafe %>%
  mutate(
    work_minutes =
      price_gbp /
      hourly_wage_gbp *
      60
  )


# ===============================================================
# 6. SAMPLE SIZE
# ===============================================================

n_cafes <- nrow(cafe)

n_countries <- n_distinct(
  cafe$country,
  na.rm = TRUE
)

n_cities <- n_distinct(
  cafe$city,
  na.rm = TRUE
)


# ===============================================================
# 7. URBAN / SUBURBAN / RURAL
# ===============================================================

urban_n <- sum(
  cafe$urban,
  na.rm = TRUE
)

suburban_n <- sum(
  cafe$suburban,
  na.rm = TRUE
)

rural_n <- sum(
  cafe$rural,
  na.rm = TRUE
)

urban_pct <- round(
  urban_n / n_cafes * 100
)

suburban_pct <- round(
  suburban_n / n_cafes * 100
)

rural_pct <- round(
  rural_n / n_cafes * 100
)


# ===============================================================
# 8. COUNTRY SAMPLE SIZE
# ===============================================================

country_sample <- cafe %>%
  filter(
    !is.na(country)
  ) %>%
  count(
    country,
    sort = TRUE,
    name = "n_cafes"
  )

largest_sample <- country_sample %>%
  slice_max(
    n_cafes,
    n = 1,
    with_ties = FALSE
  )

largest_sample_country <-
  largest_sample$country[1]

largest_sample_n <-
  largest_sample$n_cafes[1]


# ===============================================================
# 9. COUNTRY PRICE VARIABILITY
# ===============================================================

country_price_variability <- cafe %>%
  filter(
    !is.na(country),
    !is.na(price_gbp)
  ) %>%
  group_by(
    country
  ) %>%
  summarise(
    n = n(),
    median_price =
      median(
        price_gbp,
        na.rm = TRUE
      ),
    min_price =
      min(
        price_gbp,
        na.rm = TRUE
      ),
    max_price =
      max(
        price_gbp,
        na.rm = TRUE
      ),
    price_range =
      max_price -
      min_price,
    .groups = "drop"
  ) %>%
  filter(
    n >= 3
  )

widest_variation <- country_price_variability %>%
  slice_max(
    price_range,
    n = 1,
    with_ties = FALSE
  )

variation_country <-
  widest_variation$country[1]

variation_range <-
  widest_variation$price_range[1]

variation_min <-
  widest_variation$min_price[1]

variation_max <-
  widest_variation$max_price[1]


# ===============================================================
# 10. CHEAPEST OBSERVED CAFÉ
# ===============================================================

cheapest_cafe <- cafe %>%
  filter(
    !is.na(price_gbp)
  ) %>%
  slice_min(
    price_gbp,
    n = 1,
    with_ties = FALSE
  )

cheapest_cafe_country <-
  cheapest_cafe$country[1]

cheapest_cafe_city <-
  cheapest_cafe$city[1]

cheapest_cafe_price <-
  cheapest_cafe$price_gbp[1]


# ===============================================================
# 11. MOST EXPENSIVE OBSERVED CAFÉ
# ===============================================================

priciest_cafe <- cafe %>%
  filter(
    !is.na(price_gbp)
  ) %>%
  slice_max(
    price_gbp,
    n = 1,
    with_ties = FALSE
  )

priciest_cafe_country <-
  priciest_cafe$country[1]

priciest_cafe_city <-
  priciest_cafe$city[1]

priciest_cafe_price <-
  priciest_cafe$price_gbp[1]


# ===============================================================
# 12. AFFORDABILITY OUTLIER
# ===============================================================

affordability_outlier <- cafe %>%
  filter(
    !is.na(work_minutes),
    !is.na(price_gbp),
    !is.na(hourly_wage_gbp)
  ) %>%
  slice_max(
    work_minutes,
    n = 1,
    with_ties = FALSE
  )

outlier_country <-
  affordability_outlier$country[1]

outlier_city <-
  affordability_outlier$city[1]

outlier_minutes <-
  affordability_outlier$work_minutes[1]

outlier_price <-
  affordability_outlier$price_gbp[1]


# ===============================================================
# 13. OVERALL INDEX
# ===============================================================

d <- cappuccino_index %>%
  filter(
    !is.na(index)
  ) %>%
  arrange(
    index
  )

cheapest_country <-
  d$country[1]

cheapest_minutes <-
  round(
    d$index[1]
  )

priciest_country <-
  d$country[nrow(d)]

priciest_minutes <-
  round(
    d$index[nrow(d)]
  )

n_index_countries <-
  nrow(d)


# ===============================================================
# 14. MIN / MEAN / MEDIAN / MAX BY CONTINENT
# ===============================================================

get_continent_stats <- function(df) {
  
  min_val <-
    min(
      df$index,
      na.rm = TRUE
    )
  
  max_val <-
    max(
      df$index,
      na.rm = TRUE
    )
  
  mean_val <-
    mean(
      df$index,
      na.rm = TRUE
    )
  
  median_val <-
    median(
      df$index,
      na.rm = TRUE
    )
  
  min_row <- df %>%
    slice_min(
      index,
      n = 1,
      with_ties = FALSE
    )
  
  max_row <- df %>%
    slice_max(
      index,
      n = 1,
      with_ties = FALSE
    )
  
  mean_row <- df %>%
    slice_min(
      abs(index - mean_val),
      n = 1,
      with_ties = FALSE
    )
  
  median_row <- df %>%
    slice_min(
      abs(index - median_val),
      n = 1,
      with_ties = FALSE
    )
  
  tibble(
    stat = c(
      "min",
      "mean",
      "median",
      "max"
    ),
    value = c(
      min_val,
      mean_val,
      median_val,
      max_val
    ),
    country = c(
      min_row$country[1],
      mean_row$country[1],
      median_row$country[1],
      max_row$country[1]
    ),
    iso3 = c(
      min_row$iso3[1],
      mean_row$iso3[1],
      median_row$iso3[1],
      max_row$iso3[1]
    )
  )
}


continent_summary <- cappuccino_index %>%
  filter(
    !is.na(continent),
    !is.na(index)
  ) %>%
  group_by(
    continent
  ) %>%
  group_modify(
    ~ get_continent_stats(.x)
  ) %>%
  ungroup() %>%
  mutate(
    stat = factor(
      stat,
      levels = c(
        "min",
        "mean",
        "median",
        "max"
      )
    )
  ) %>%
  arrange(
    continent,
    stat
  )


continents <- sort(
  unique(
    continent_summary$continent
  )
)

n_continents <- length(
  continents
)


# ===============================================================
# 15. COLORS
# ===============================================================

bg_color       <- "#FBE9DC"
box_color      <- "#D8CBB8"

title_color    <- "#1A1A1A"
subtitle_color <- "#514A43"

coffee_color   <- "#8A5A2B"
coffee_dark    <- "#4A2E14"

line_color     <- "#B8AC9D"
footer_color   <- "#756D65"

tray_color     <- "#C7A97E"
tray_line      <- "#9C7B4F"

map_bg_country <- "#EFE6DA"
map_border     <- "#B9A98D"


# ===============================================================
# 16. CUP COLOR SCALE
# ===============================================================

cup_ramp <- colorRampPalette(
  c(
    "#E6C99A",
    coffee_dark
  )
)

stat_cols <- cup_ramp(4)

names(stat_cols) <- c(
  "min",
  "mean",
  "median",
  "max"
)


# ===============================================================
# 17. WORLD MAP
# ===============================================================

world <- ne_countries(
  scale = 110L,
  returnclass = "sf"
)

world$continent_group <- countrycode(
  world$iso_a3,
  "iso3c",
  "continent"
)

world_joined <- world %>%
  left_join(
    continent_summary %>%
      select(
        iso3,
        stat
      ),
    by = c(
      "iso_a3" = "iso3"
    )
  )


# ===============================================================
# 18. MAP FUNCTION
# ===============================================================

make_continent_map <- function(cont) {
  
  bg <- world_joined %>%
    filter(
      continent_group == cont
    )
  
  ggplot(bg) +
    
    geom_sf(
      aes(
        fill = stat
      ),
      color = map_border,
      linewidth = 0.10
    ) +
    
    scale_fill_manual(
      values = stat_cols,
      na.value = map_bg_country,
      guide = "none"
    ) +
    
    coord_sf(
      expand = TRUE,
      datum = NA
    ) +
    
    theme_void() +
    
    theme(
      plot.margin = margin(
        0,
        0,
        0,
        0
      )
    )
}

continent_maps <- lapply(
  continents,
  make_continent_map
)

names(continent_maps) <- continents


# ===============================================================
# 19. TEXT HELPERS
# ===============================================================

text_w <- function(
    label,
    gp
) {
  
  convertWidth(
    grobWidth(
      textGrob(
        label,
        gp = gp
      )
    ),
    "npc",
    valueOnly = TRUE
  )
}


line_h <- function(
    gp
) {
  
  convertHeight(
    grobHeight(
      textGrob(
        "Ag",
        gp = gp
      )
    ),
    "npc",
    valueOnly = TRUE
  )
}


draw_line <- function(
    label,
    x,
    y_top,
    gp,
    hjust = "left"
) {
  
  grid.text(
    label,
    x = x,
    y = y_top,
    just = c(
      hjust,
      "top"
    ),
    gp = gp
  )
}


# ===============================================================
# 20. COFFEE BEAN
# ===============================================================

draw_bean <- function(
    cx,
    cy,
    size = 0.014,
    color = coffee_color,
    angle = 20
) {
  
  pushViewport(
    viewport(
      x = unit(
        cx,
        "npc"
      ),
      y = unit(
        cy,
        "npc"
      ),
      width = unit(
        size,
        "snpc"
      ),
      height = unit(
        size * 1.5,
        "snpc"
      ),
      angle = angle
    )
  )
  
  a <- seq(
    0,
    2 * pi,
    length.out = 80
  )
  
  grid.polygon(
    x =
      0.5 +
      0.5 *
      cos(a),
    y =
      0.5 +
      0.5 *
      sin(a),
    gp = gpar(
      fill = color,
      col = NA
    )
  )
  
  t <- seq(
    0,
    1,
    length.out = 40
  )
  
  grid.lines(
    x =
      0.5 +
      0.09 *
      sin(
        t * pi
      ),
    y = t,
    gp = gpar(
      col = bg_color,
      lwd = 2
    )
  )
  
  upViewport()
}


# ===============================================================
# 21. COUNTRY LABEL
# ===============================================================

abbrev_country <- function(
    name,
    max_chars = 13
) {
  
  if (
    is.na(name)
  ) {
    return("")
  }
  
  if (
    nchar(name) <= max_chars
  ) {
    return(name)
  }
  
  paste0(
    substr(
      name,
      1,
      max_chars - 1
    ),
    "\u2026"
  )
}


# ===============================================================
# 22. ELLIPSE HELPER
# ===============================================================

ellipse_xy <- function(
    cx,
    cy,
    rx,
    ry,
    n = 100
) {
  
  a <- seq(
    0,
    2 * pi,
    length.out = n
  )
  
  list(
    x =
      cx +
      rx *
      cos(a),
    y =
      cy +
      ry *
      sin(a)
  )
}


# ===============================================================
# 23. DRAW COFFEE CUP
#
# IMPORTANT:
#   The cup is deliberately compact vertically.
#   Steam is also shortened so that the complete cup remains
#   inside its continent card.
# ===============================================================

# ===============================================================
# WHITE CERAMIC COFFEE CUP
# ===============================================================

# ===============================================================
# FLAT INFOGRAPHIC COFFEE CUP
# Inspired by vintage coffee infographic illustrations
# ===============================================================

draw_cup <- function(
    cx,
    cy,
    r,
    coffee_level = 0.50,
    steam = TRUE
) {
  
  # -------------------------------------------------------------
  # Dimensions
  # -------------------------------------------------------------
  
  cup_w <- r * 1.05
  cup_h <- r * 1.05
  
  
  # =============================================================
  # SAUCER
  # =============================================================
  
  saucer <- ellipse_xy(
    cx,
    cy - r * 0.82,
    r * 1.22,
    r * 0.16
  )
  
  grid.polygon(
    saucer$x,
    saucer$y,
    gp = gpar(
      fill = "#FFFFFF",
      col = "#FFFFFF"
    )
  )
  
  
  # =============================================================
  # HANDLE — DRAW FIRST SO BODY SITS OVER IT
  # =============================================================
  
  handle_outer <- ellipse_xy(
    cx + r * 0.86,
    cy + r * 0.05,
    r * 0.42,
    r * 0.48
  )
  
  handle_inner <- ellipse_xy(
    cx + r * 0.86,
    cy + r * 0.05,
    r * 0.24,
    r * 0.29
  )
  
  grid.polygon(
    handle_outer$x,
    handle_outer$y,
    gp = gpar(
      fill = "#FFFFFF",
      col = "#FFFFFF"
    )
  )
  
  grid.polygon(
    handle_inner$x,
    handle_inner$y,
    gp = gpar(
      fill = bg_color,
      col = bg_color
    )
  )
  
  
  # =============================================================
  # CUP BODY
  # =============================================================
  
  # Slightly tapered body
  body_x <- c(
    cx - r * 0.78,
    cx + r * 0.78,
    cx + r * 0.66,
    cx + r * 0.50,
    cx,
    cx - r * 0.50,
    cx - r * 0.66,
    cx - r * 0.78
  )
  
  body_y <- c(
    cy + r * 0.43,
    cy + r * 0.43,
    cy - r * 0.38,
    cy - r * 0.70,
    cy - r * 0.78,
    cy - r * 0.70,
    cy - r * 0.38,
    cy + r * 0.43
  )
  
  grid.polygon(
    body_x,
    body_y,
    gp = gpar(
      fill = "#FFFFFF",
      col = "#FFFFFF",
      lwd = 1
    )
  )
  
  
  # =============================================================
  # COFFEE AREA
  # =============================================================
  
  # Map coffee_level to vertical position
  coffee_y <- cy -
    r * 0.65 +
    coffee_level * r * 1.05
  
  coffee_rx <- r * 0.61
  coffee_ry <- r * 0.13
  
  coffee <- ellipse_xy(
    cx,
    coffee_y,
    coffee_rx,
    coffee_ry
  )
  
  grid.polygon(
    coffee$x,
    coffee$y,
    gp = gpar(
      fill = "#4A2E14",
      col = "#4A2E14"
    )
  )
  
  
  # =============================================================
  # COFFEE BODY / VISIBLE FILL
  # =============================================================
  
  # Coffee fill below surface
  coffee_body_x <- c(
    cx - r * 0.61,
    cx + r * 0.61,
    cx + r * 0.55,
    cx + r * 0.43,
    cx,
    cx - r * 0.43,
    cx - r * 0.55,
    cx - r * 0.61
  )
  
  coffee_body_y <- c(
    coffee_y,
    coffee_y,
    cy - r * 0.48,
    cy - r * 0.65,
    cy - r * 0.70,
    cy - r * 0.65,
    cy - r * 0.48,
    coffee_y
  )
  
  # Only draw when coffee is sufficiently high
  if (coffee_level > 0.05) {
    
    grid.polygon(
      coffee_body_x,
      coffee_body_y,
      gp = gpar(
        fill = "#4A2E14",
        col = NA
      )
    )
  }
  
  
  # =============================================================
  # COFFEE SURFACE — LIGHTER TOP
  # =============================================================
  
  coffee_surface <- ellipse_xy(
    cx,
    coffee_y,
    r * 0.59,
    r * 0.115
  )
  
  grid.polygon(
    coffee_surface$x,
    coffee_surface$y,
    gp = gpar(
      fill = "#6B4223",
      col = NA
    )
  )
  
  
  # =============================================================
  # SMALL CREMA HIGHLIGHT
  # =============================================================
  
  crema <- ellipse_xy(
    cx - r * 0.16,
    coffee_y + r * 0.025,
    r * 0.22,
    r * 0.035
  )
  
  grid.polygon(
    crema$x,
    crema$y,
    gp = gpar(
      fill = "#A8794B",
      col = NA,
      alpha = 0.75
    )
  )
  
  
  # =============================================================
  # CUP RIM
  # =============================================================
  
  rim <- ellipse_xy(
    cx,
    cy + r * 0.42,
    r * 0.79,
    r * 0.16
  )
  
  grid.polygon(
    rim$x,
    rim$y,
    gp = gpar(
      fill = "#FFFFFF",
      col = "#FFFFFF"
    )
  )
  
  
  # =============================================================
  # COFFEE VISIBLE INSIDE RIM
  # =============================================================
  
  rim_coffee <- ellipse_xy(
    cx,
    cy + r * 0.42,
    r * 0.60,
    r * 0.085
  )
  
  grid.polygon(
    rim_coffee$x,
    rim_coffee$y,
    gp = gpar(
      fill = "#4A2E14",
      col = NA
    )
  )
  
  
  # =============================================================
  # WHITE FRONT EDGE
  # =============================================================
  
  front_rim <- ellipse_xy(
    cx,
    cy + r * 0.42,
    r * 0.79,
    r * 0.16
  )
  
  grid.lines(
    front_rim$x,
    front_rim$y,
    gp = gpar(
      col = "#FFFFFF",
      lwd = 2
    )
  )
  
  
  # =============================================================
  # STEAM
  # =============================================================
  
  if (steam) {
    
    # Short, chunky steam — closer to infographic style
    for (off in c(-0.22, 0.22)) {
      
      t <- seq(
        0,
        1,
        length.out = 25
      )
      
      sx <- cx +
        r * off +
        sin(t * pi * 1.5) * r * 0.045
      
      sy <- cy +
        r * 0.62 +
        t * r * 0.35
      
      grid.lines(
        sx,
        sy,
        gp = gpar(
          col = "#FFFFFF",
          lwd = 1.2,
          alpha = 0.55,
          lineend = "round"
        )
      )
    }
  }
}

# ===============================================================
# 24. START PAGE
# ===============================================================

grid.newpage()

grid.rect(
  gp = gpar(
    fill = bg_color,
    col = NA
  )
)


# ===============================================================
# 25. PAGE BOUNDS
# ===============================================================

x0 <- 0.035
x_max <- 0.975


# ===============================================================
# 26. TITLE
# ===============================================================

gp_title <- gpar(
  fontsize = 32,
  fontfamily = "oswald",
  col = title_color
)

gp_subtitle <- gpar(
  fontsize = 12.5,
  fontfamily = "oswald",
  col = subtitle_color
)

title_y <- 0.985

title_text <- "Coffee Metrics Visualized"

grid.text(
  title_text,
  x = x0,
  y = title_y,
  just = c(
    "left",
    "top"
  ),
  gp = gp_title
)

title_width <- text_w(
  title_text,
  gp_title
)

draw_bean(
  cx =0.305,
  cy =
    title_y -
    0.023,
  size = 0.035,
  color = coffee_color,
  angle = 20
)


# ---------------------------------------------------------------
# SUBTITLE
# ---------------------------------------------------------------

subtitle_y <-
  title_y -
  0.050

draw_line(
  "How much of a barista's working day goes into one small cappuccino?",
  x0,
  subtitle_y,
  gp_subtitle
)

draw_line(
  "And what does the café sample behind the index actually look like?",
  x0,
  subtitle_y -
    0.022,
  gp_subtitle
)

header_bottom <-
  subtitle_y -
  0.045


# ===============================================================
# 27. INSIGHT BOX
# ===============================================================

box_xmin <- 0.625
box_xmax <- x_max

box_ymax <- 0.985

box_width <-
  box_xmax -
  box_xmin

box_height <- 0.115

box_ymin <-
  box_ymax -
  box_height

grid.roundrect(
  x =
    mean(
      c(
        box_xmin,
        box_xmax
      )
    ),
  y =
    mean(
      c(
        box_ymin,
        box_ymax
      )
    ),
  width = box_width,
  height = box_height,
  r = unit(
    4,
    "mm"
  ),
  gp = gpar(
    fill = box_color,
    col = NA
  )
)

gp_box <- gpar(
  fontsize = 10.3,
  fontfamily = "inter",
  col = title_color
)

box_lines <- c(
  
  sprintf(
    "%s has the cheapest index — %d minutes",
    cheapest_country,
    cheapest_minutes
  ),
  
  sprintf(
    "%s needs %d minutes of work",
    priciest_country,
    priciest_minutes
  ),
  
  sprintf(
    "The index covers %d countries using %d café observations",
    n_index_countries,
    n_cafes
  )
)

box_cursor <-
  box_ymax -
  0.018

for (
  ln in box_lines
) {
  
  draw_line(
    ln,
    box_xmin + 0.018,
    box_cursor,
    gp_box
  )
  
  box_cursor <-
    box_cursor -
    0.028
}


# ===============================================================
# 28. HEADER DIVIDER
# ===============================================================

divider_y <-
  min(
    header_bottom,
    box_ymin
  ) -
  0.010

grid.lines(
  x = c(
    x0,
    x_max
  ),
  y = c(
    divider_y,
    divider_y
  ),
  gp = gpar(
    col = line_color,
    lwd = 1
  )
)


# ===============================================================
# 29. LEGEND
# ===============================================================

gp_axis <- gpar(
  fontsize = 9.5,
  fontfamily = "inter",
  fontface = "bold",
  col = title_color
)

axis_y <-
  divider_y -
  0.012

draw_line(
"For mean and median values, the country closest is selected.",
  x0,
  axis_y,
  gp_axis
)

# draw_line(
#   "COFFEE LEVEL = MIN · MEAN · MEDIAN · MAX",
#   x_max,
#   axis_y,
#   gp_axis,
#   hjust = "right"
# )


# ===============================================================
# 30. MAIN CONTENT AREA
#
# IMPORTANT:
#   The continent rows now have an explicit gap.
#
#   Instead of:
#
#     row_h = plot_h / n_continents
#
#   we reserve space for gaps first.
# ===============================================================

content_top <-
  axis_y -
  0.028

plot_ymax <-
  content_top

plot_ymin <-
  0.315

plot_h <-
  plot_ymax -
  plot_ymin


# ===============================================================
# 31. HORIZONTAL WIDTH
# ===============================================================

label_w <- 0.135

map_w <- 0.030

cups_x0 <-
  label_w +
  map_w +
  0.012

cups_x1 <- 0.985


# ===============================================================
# 32. ROW SPACING
#
# This is the key change.
#
# continent_gap controls the visible space between Africa,
# Asia, Europe, etc.
#
# The card is intentionally smaller than row_h so the background
# of the page is visible between cards.
# ===============================================================

continent_gap <- 0.018

rows_height <-0.8

row_h <-
  rows_height /
  n_continents


# ===============================================================
# 33. MAIN VIEWPORT
# ===============================================================

pushViewport(
  viewport(
    x = 0.5,
    y =
      mean(
        c(
          plot_ymin,
          plot_ymax
        )
      ),
    width =
      x_max -
      x0,
    height =
      plot_h
  )
)

# ===============================================================
# STATISTIC COLUMN HEADERS
# ===============================================================

stat_headers <- c(
  "MIN",
  "MEAN",
  "MEDIAN",
  "MAX"
)

# Centre of each of the four card columns
header_x <- cups_x0 +
  (seq_along(stat_headers) - 0.5) *
  ((cups_x1 - cups_x0) / 4)

# Header position
header_y <- plot_ymax + 0.212

grid.text(
  stat_headers,
  x = header_x,
  y = header_y,
  gp = gpar(
    fontfamily = "oswald",
    fontsize = 15,
    # fontface = "bold",
    col = title_color
  )
)


# ===============================================================
# 34. CONTINENT ROWS
# ===============================================================

    for (
      i in seq_along(continents)
    ) {
      cont <-
        continents[i]
      
      rows <- continent_summary %>%
        filter(
          continent == cont
        ) %>%
        arrange(
          stat
        )
      
      
      # -------------------------------------------------------------
      # ROW POSITION
      #
      # The gap is added between rows.
      # -------------------------------------------------------------
      
      row_top <-
        1 -
        (i - 1) *
        (
          row_h +
            continent_gap
        )
      
      row_bot <-
        row_top -
        row_h
      
      cy <-
        row_top -
        row_h / 2
      
      
      # =============================================================
      # CONTINENT LABEL
      # =============================================================
      
      grid.text(
        cont,
        x = 0,
        y = cy,
        just = c(
          "left",
          "center"
        ),
        gp = gpar(
          fontsize = 14,
          fontfamily = "oswald",
          col = coffee_dark
        )
      )
      
      
      # =============================================================
      # MAP
      # =============================================================
      
      map_center_x <-
        label_w +
        map_w / 3
      
      if (cont == "Oceania") {
        map_x <- map_center_x - 0.19
        map_width <- map_w * 0.90
      }
      else{
        map_x<-map_center_x-0.07
      }
      pushViewport(
        viewport(
          x = map_x,
          y = cy,
          width = 2.37,
          jus="center",
          height =
            row_h
        )
      )
      
      grid.draw(
        ggplotGrob(
          continent_maps[[cont]]
        )
      )
      
      upViewport()
      
      
      # =============================================================
      # COFFEE CARD AREA
      #
      # The card is deliberately smaller than row_h.
      # This creates visible space between continent cards.
      # =============================================================
      
      tray_x0 <-
        cups_x0
      
      tray_x1 <-
        cups_x1
      
      tray_h <-
        row_h *
        1.22
      
      
      # -------------------------------------------------------------
      # CARD
      # -------------------------------------------------------------
      
      grid.roundrect(
        x =
          mean(
            c(
              tray_x0,
              tray_x1
            )
          ),
        y = cy,
        width =
          tray_x1 -
          tray_x0,
        height =
          tray_h,
        r = unit(
          3,
          "mm"
        ),
        gp = gpar(
          fill = tray_color,
          col = tray_line,
          lwd = 0.9,
          alpha = 0.52
        )
      )
      
      
      # =============================================================
      # FOUR CUP CELLS
      # =============================================================
      
      n_cups <-
        nrow(rows)
      
      cell_w <-
        (
          tray_x1 -
            tray_x0
        ) /
        n_cups
      
      
      # -------------------------------------------------------------
      # CUP SIZE
      #
      # Same physical cup size for every statistic.
      #
      # The coffee level — not the cup size — carries the
      # MIN / MEAN / MEDIAN / MAX meaning.
      # -------------------------------------------------------------
      
      base_r <-
        min(
          0.030,
          row_h * 0.27,
          cell_w * 0.22
        )
      
      
      # Same cup size
      size_mult <- c(
        small  = 1,
        medium = 1,
        large  = 1
      )
      
      
      # =============================================================
      # CUP LOOP
      # =============================================================
      
      for (
        j in seq_len(n_cups)
      ) {
        # -----------------------------------------------------------
        # CENTER OF CARD CELL
        # -----------------------------------------------------------
      
        cx <-
          tray_x0 +
          (
            j - 0.5
          ) *
          cell_w
        minutes_value <- rows$value[j]
  
        
        # -----------------------------------------------------------
        # COFFEE LEVEL
        #
        # MIN    = low
        # MEAN   = medium
        # MEDIAN = medium
        # MAX    = high
        # -----------------------------------------------------------
        
        coffee_level <- dplyr::case_when(
          
          rows$stat[j] == "min" ~
            0.18,
          
          rows$stat[j] == "mean" ~
            0.50,
          
          rows$stat[j] == "median" ~
            0.50,
          
          rows$stat[j] == "max" ~
            0.95,
          
          TRUE ~
            0.50
        )
        
        
        # -----------------------------------------------------------
        # CUP RADIUS
        # -----------------------------------------------------------
        
        cup_r <- base_r 
        
        
        # -----------------------------------------------------------
        # CUP POSITION
        #
        # Slightly above card center.
        #
        # The cup is kept away from the top and bottom boundaries
        # so steam and country labels remain inside the card.
        # -----------------------------------------------------------
        
        cup_y <-
          cy +
          tray_h *
          0.055
        
        
        grid.text(
          paste0(round(minutes_value, 0), " min"),
          x = cx,
          y = cup_y + cup_r * 1.55,
          just = "centre",
          gp = gpar(
            fontfamily = "inter",
            fontsize = 18,
            # fontface = "bold",
            col = coffee_dark
          )
        )
        
        # ===========================================================
        # CUP
        # ===========================================================
        
        draw_cup(
          cx = cx,
          cy = cup_y,
          r = cup_r,
          coffee_level = coffee_level,
          steam = TRUE
        )
        
        
        # ===========================================================
        # COUNTRY
        # ===========================================================
        
        grid.text(
          abbrev_country(
            rows$country[j],
            13
          ),
          x = cx,
          y =
            cy -
            tray_h *
            0.24,
          just = "center",
          gp = gpar(
            fontsize = 14,
            fontfamily = "inter",
            fontface = "bold",
            col = title_color
          )
        )
      }
      
      
      # =============================================================
      # ROW DIVIDER
      #
      # This is intentionally very subtle.
      # The actual separation is created by the card gap.
      # =============================================================
      
      if (
        i < n_continents
      ) {
        
        grid.lines(
          x = c(
            0,
            1
          ),
          y = c(
            row_bot -
              continent_gap / 2,
            row_bot -
              continent_gap / 2
          ),
          gp = gpar(
            col = line_color,
            lwd = 0.35,
            lty = "dotted",
            alpha = 0.65
          )
        )
      }
    }


# ===============================================================
# 35. CLOSE MAIN VIEWPORT
# ===============================================================

upViewport()


# ===============================================================
# 37. CAFÉS IN NUMBERS
# ===============================================================

cafe_top <-
  plot_ymin -
  0.012


cafe_bottom <-
  0.075


# ===============================================================
# SECTION HEADER
# ===============================================================

grid.text(
  "CAFÉS IN NUMBERS",
  x = x0,
  y = cafe_top,
  just = c(
    "left",
    "top"
  ),
  gp = gpar(
    fontsize = 20,
    fontfamily = "oswald",
    fontface = "bold",
    col = coffee_dark
  )
)


# ===============================================================
# SECTION DESCRIPTION
# ===============================================================

grid.text(
  "The index starts with a country-level measure. The café sample shows what sits underneath it.",
  x = x0,
  y = cafe_top - 0.029,
  just = c(
    "left",
    "top"
  ),
  gp = gpar(
    fontsize = 11.8,
    fontfamily = "inter",
    col = "black"
  )
)


# ===============================================================
# 38. CAFÉ METRIC CARDS
# ===============================================================

card_top <-
  cafe_top -
  0.048


card_bottom <-
  cafe_bottom


card_gap <- 0.009


card_w <-
  (
    x_max -
      x0 -
      card_gap * 3
  ) /
  4


card_x <- c(
  x0,
  x0 + card_w + card_gap,
  x0 + 2 * (card_w + card_gap),
  x0 + 3 * (card_w + card_gap)
)


card_h <-
  card_top -
  card_bottom

# ===============================================================
# CARD FUNCTION
# ===============================================================

draw_cafe_card <- function(
    x,
    label,
    value,
    detail,
    accent = coffee_color
) {
  
  # -------------------------------------------------------------
  # BACKGROUND
  # -------------------------------------------------------------
  
  grid.roundrect(
    x =
      x +
      card_w / 2,
    y =
      mean(
        c(
          card_bottom,
          card_top
        )
      ),
    width = card_w,
    height = 0,
    r = unit(
      3,
      "mm"
    ),
    gp = gpar(
      fill = cafe_card,
      col = NA
    )
  )
  
  
  # -------------------------------------------------------------
  # TOP ACCENT
  # -------------------------------------------------------------
  
  grid.rect(
    x =
      x +
      card_w / 2,
    y =
      card_top -
      0.004,
    width = card_w,
    height = 0.008,
    gp = gpar(
      fill = accent,
      col = NA
    )
  )
  
  
  # -------------------------------------------------------------
  # LABEL
  # -------------------------------------------------------------
  
  grid.text(
    label,
    x =
      x +
      0.017,
    y =
      card_top -
      0.018,
    just = c(
      "left",
      "top"
    ),
    gp = gpar(
      fontsize = 20.4,
      fontfamily = "bebas",
      fontface = "bold",
      col = footer_color
    )
  )
  
  
  # -------------------------------------------------------------
  # BIG NUMBER
  # -------------------------------------------------------------
  
  grid.text(
    value,
    x =
      x +
      card_w / 2,
    y =
      card_top -
      0.050,
    just = c(
      "centre",
      "top"
    ),
    gp = gpar(
      fontsize = 25,
      fontfamily = "bebas",
      fontface = "bold",
      col = title_color
    )
  )
  
  
  # -------------------------------------------------------------
  # DETAIL
  # -------------------------------------------------------------
  
  grid.text(
    detail,
    x =
      x +
      0.015,
    y =
      card_bottom +
      0.197,
    just = c(
      "left",
      "bottom"
    ),
    gp = gpar(
      fontsize = 7.0,
      fontfamily = "bebas",
      col = subtitle_color
    )
  )
}


# ===============================================================
# CARD 1 — CAFÉS
# ===============================================================

draw_cafe_card(
  card_x[1],
  "CAFÉ SAMPLE",
  sprintf(
    "%s\n%d countries · %d cities",
    format(n_cafes, big.mark = ","),
    n_countries,
    n_cities
  ),
  "",
  accent = coffee_dark
)


# ===============================================================
# CARD 2 — COUNTRIES
# ===============================================================
# CARD 2 — COUNTRIES
# ===============================================================

draw_cafe_card(
  card_x[2],
  "COUNTRIES",
  sprintf(
    "%s\n%d cafés represented",
    format(n_countries, big.mark = ","),
    n_cafes
  ),
  "",
  accent = coffee_color
)


# ===============================================================
# CARD 3 — CITIES
# ===============================================================
draw_cafe_card(
  card_x[3],
  "CITIES",
  sprintf(
    "%s\n%d%% urban · %d%% suburban · %d%% rural",
    format(n_cities, big.mark = ","),
    urban_pct,
    suburban_pct,
    rural_pct
  ),
  "",
  accent = tray_line
)

# ===============================================================
# CARD 4 — CAFÉ PRICE
# ===============================================================

mean_cafe_price <-
  mean(
    cafe$price_gbp,
    na.rm = TRUE
  )

draw_cafe_card(
  card_x[4],
  "AVERAGE CAFÉ PRICE",
  sprintf(
    "£%.2f\nMIN £%.2f  --  MAX £%.2f",
    mean_cafe_price,
    min(cafe$price_gbp, na.rm = TRUE),
    max(cafe$price_gbp, na.rm = TRUE)
  ),
  "",
  accent = coffee_dark
)

# ===============================================================
# 39. SECONDARY CAFÉ INSIGHT LINE
# ===============================================================

secondary_y <-
  cafe_bottom -
  0.005

# grid.text(
#   sprintf(
#     "WHERE THEY ARE  ·  %d%% urban  ·  %d%% suburban  ·  %d%% rural",
#     urban_pct,
#     suburban_pct,
#     rural_pct
#   ),
#   x = x0,
#   y = secondary_y,
#   just = c(
#     "left",
#     "top"
#   ),
#   gp = gpar(
#     fontsize = 7.7,
#     fontfamily = "inter",
#     fontface = "bold",
#     col = subtitle_color
#   )
# )


# ===============================================================
# 40. EDITORIAL TAKEAWAY
# ===============================================================
# 
# takeaway_y <-
#   secondary_y -
#   0.025
# 
# grid.text(
#   "WHY IT MATTERS",
#   x = x0,
#   y = 0.255,
#   just = c(
#     "left",
#     "top"
#   ),
#   gp = gpar(
#     fontsize = 7.8,
#     fontfamily = "inter",
#     fontface = "bold",
#     col = coffee_dark
#   )
# )
# 
# grid.text(
#   "A cappuccino's price is only half the story. The same drink can demand very different amounts of work depending on local wages — and the café sample itself is uneven across countries.",
#   x = x0 + 0.195,
#   y = 0.255,
#   just = c(
#     "left",
#     "top"
#   ),
#   gp = gpar(
#     fontsize = 7.7,
#     fontfamily = "inter",
#     col = subtitle_color
#   )
# )


# ===============================================================
# 41. FOOTER
# ===============================================================

footer_bottom <- 0.012

footer_divider_y <-
  footer_bottom +
  0.285

# grid.lines(
#   x = c(
#     x0,
#     x_max
#   ),
#   y = c(
#     footer_divider_y,
#     footer_divider_y
#   ),
#   gp = gpar(
#     col = line_color,
#     lwd = 0.8
#   )
# )

grid.text(
  "Data: TidyTuesday- Hari Krishna",
  x = x_max,
  y = footer_bottom + 0.065,
  just = c(
    "right",
    "center"
  ),
  gp = gpar(
    fontsize = 17.5,
    fontfamily = "oswald",
    col = "black"
  )
)
