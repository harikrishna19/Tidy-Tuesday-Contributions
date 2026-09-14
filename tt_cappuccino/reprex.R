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
#   subtitle
#
#   -------------------------------------------------------------
#
#   LIGHTER = CHEAPER
#
#   EUROPE       MAP       [ CUP ][ CUP ][ CUP ][ CUP ][ CUP ]
#   ASIA         MAP       [ CUP ][ CUP ][ CUP ][ CUP ][ CUP ]
#   AFRICA       MAP       [ CUP ][ CUP ][ CUP ][ CUP ][ CUP ]
#   ...
#
#   -------------------------------------------------------------
#
#   CAFÉS IN NUMBERS
#   sample · urbanicity · price variation · affordability
#
# ===============================================================


# ---------------------------------------------------------------
# PACKAGES
# ---------------------------------------------------------------

library(grid)
library(dplyr)
library(tidyr)
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

# Editorial headline + clean modern body font
#
# Bebas Neue:
#   Strong condensed display font for titles.
#
# Inter:
#   Clean, highly readable font for data labels and supporting text.

font_add_google(
  "Bebas Neue",
  "bebas"
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
# 14. TOP 5 CHEAPEST BY CONTINENT
# ===============================================================

top5_by_continent <- cappuccino_index %>%
  filter(
    !is.na(continent),
    !is.na(index)
  ) %>%
  group_by(
    continent
  ) %>%
  arrange(
    index,
    .by_group = TRUE
  ) %>%
  slice_head(
    n = 5
  ) %>%
  mutate(
    rank = row_number()
  ) %>%
  ungroup()


continents <- sort(
  unique(
    top5_by_continent$continent
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

rank_cols <- cup_ramp(5)


# ===============================================================
# 17. WORLD MAP
# ===============================================================

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

world$continent_group <- countrycode(
  world$iso_a3,
  "iso3c",
  "continent"
)


world_joined <- world %>%
  left_join(
    top5_by_continent %>%
      st_drop_geometry() %>%
      select(
        iso3,
        rank
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
        fill = factor(rank)
      ),
      color = map_border,
      linewidth = 0.10
    ) +
    
    scale_fill_manual(
      values = setNames(
        rank_cols,
        as.character(1:5)
      ),
      na.value = map_bg_country,
      guide = "none"
    ) +
    
    coord_sf(
      expand = TRUE
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
# 23. DRAW CUP
# ===============================================================

draw_cup <- function(
    cx,
    cy,
    r,
    rim_col,
    fill_col,
    steam = TRUE
) {
  
  rx <- r
  ry <- r * 0.78
  
  
  # -------------------------------------------------------------
  # SHADOW
  # -------------------------------------------------------------
  
  sh <- ellipse_xy(
    cx + r * 0.06,
    cy - r * 0.55,
    rx * 1.05,
    ry * 0.35
  )
  
  
  grid.polygon(
    sh$x,
    sh$y,
    gp = gpar(
      fill = "#2A1B0E",
      col = NA,
      alpha = 0.14
    )
  )
  
  
  # -------------------------------------------------------------
  # SAUCER
  # -------------------------------------------------------------
  
  sc <- ellipse_xy(
    cx,
    cy - r * 0.05,
    rx * 1.55,
    ry * 0.95
  )
  
  
  grid.polygon(
    sc$x,
    sc$y,
    gp = gpar(
      fill = "#FFFFFF",
      col = map_border,
      lwd = 0.5,
      alpha = 0.55
    )
  )
  
  
  # -------------------------------------------------------------
  # HANDLE
  # -------------------------------------------------------------
  
  ha <- seq(
    -pi * 0.55,
    pi * 0.55,
    length.out = 30
  )
  
  
  hx <- cx +
    rx * 0.92 +
    rx * 0.34 *
    cos(ha)
  
  
  hy <- cy +
    ry * 0.30 *
    sin(ha)
  
  
  grid.lines(
    hx,
    hy,
    gp = gpar(
      col = rim_col,
      lwd = 2.6,
      lineend = "round"
    )
  )
  
  
  # -------------------------------------------------------------
  # BODY
  # -------------------------------------------------------------
  
  body <- ellipse_xy(
    cx,
    cy,
    rx,
    ry
  )
  
  
  grid.polygon(
    body$x,
    body$y,
    gp = gpar(
      fill = fill_col,
      col = rim_col,
      lwd = 2.2
    )
  )
  
  
  # -------------------------------------------------------------
  # INNER RIM
  # -------------------------------------------------------------
  
  inner <- ellipse_xy(
    cx,
    cy,
    rx * 0.94,
    ry * 0.94
  )
  
  
  grid.polygon(
    inner$x,
    inner$y,
    gp = gpar(
      fill = NA,
      col = rim_col,
      lwd = 0.6,
      alpha = 0.35
    )
  )
  
  
  # -------------------------------------------------------------
  # HIGHLIGHT
  # -------------------------------------------------------------
  
  hl <- ellipse_xy(
    cx - rx * 0.30,
    cy + ry * 0.28,
    rx * 0.30,
    ry * 0.16
  )
  
  
  grid.polygon(
    hl$x,
    hl$y,
    gp = gpar(
      fill = "#FFFFFF",
      col = NA,
      alpha = 0.30
    )
  )
  
  
  # -------------------------------------------------------------
  # STEAM
  # -------------------------------------------------------------
  
  if (steam) {
    
    for (
      off in c(
        -0.35,
        0.35
      )
    ) {
      
      t <- seq(
        0,
        1,
        length.out = 30
      )
      
      
      sx <- cx +
        rx * off +
        0.12 *
        r *
        sin(
          t * 3 * pi
        )
      
      
      sy <- cy +
        ry * 0.9 +
        t *
        r *
        1.8
      
      
      grid.lines(
        sx,
        sy,
        gp = gpar(
          col = "#FFFFFF",
          lwd = 1.1,
          alpha = (1 - t) * 0.5
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
  fontfamily = "bebas",
  col = title_color
)


gp_subtitle <- gpar(
  fontsize = 12.5,
  fontfamily = "inter",
  col = subtitle_color
)


title_y <- 0.985


# ---------------------------------------------------------------
# TITLE
# ---------------------------------------------------------------

title_text <- "COFFEE-COUNTRIES"


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


# ---------------------------------------------------------------
# COFFEE BEAN AFTER TITLE
# ---------------------------------------------------------------

title_width <- text_w(
  title_text,
  gp_title
)


draw_bean(
  cx =
    x0 +
    title_width +
    0.027,
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
  fontsize = 10.5,
  fontfamily = "inter",
  fontface = "bold",
  col = title_color
)


axis_y <-
  divider_y -
  0.012


draw_line(
  "LIGHTER = CHEAPER",
  x0,
  axis_y,
  gp_axis
)


draw_line(
  "DARKER = PRICIER · WITHIN EACH REGION'S TOP 5",
  x_max,
  axis_y,
  gp_axis,
  hjust = "right"
)


# ===============================================================
# 30. MAIN CONTENT AREA
#
# More vertical space is given to the rows.
# The café section is pulled upward so there is no large
# dead area after Oceania.
# ===============================================================

content_top <-
  axis_y -
  0.028


plot_ymax <-
  content_top


# Previously the café section began much lower.
# Pull it up substantially.

plot_ymin <-
  0.305


plot_h <-
  plot_ymax -
  plot_ymin


row_h <-
  plot_h /
  n_continents


# ===============================================================
# 31. HORIZONTAL WIDTH
# ===============================================================

label_w <- 0.135

map_w <- 0.250

cups_x0 <-
  label_w +
  map_w +
  0.012


cups_x1 <- 0.985


# ===============================================================
# 32. MAIN VIEWPORT
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
# 33. CONTINENT ROWS
# ===============================================================

for (
  i in seq_along(continents)
) {
  
  cont <-
    continents[i]
  
  
  rows <- top5_by_continent %>%
    filter(
      continent == cont
    ) %>%
    arrange(
      index
    )
  
  
  # -------------------------------------------------------------
  # ROW POSITION
  # -------------------------------------------------------------
  
  row_top <-
    1 -
    (i - 1) *
    row_h
  
  
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
      fontfamily = "bebas",
      col = coffee_dark
    )
  )
  
  
  # =============================================================
  # MAP
  # =============================================================
  
  map_center_x <-
    label_w +
    map_w / 2
  
  
  pushViewport(
    viewport(
      x = map_center_x,
      y = cy,
      width = map_w,
      height =
        row_h *
        0.95
    )
  )
  
  
  grid.draw(
    ggplotGrob(
      continent_maps[[cont]]
    )
  )
  
  
  upViewport()
  
  
  # =============================================================
  # COFFEE TRAY
  #
  # Taller tray = larger cups.
  # =============================================================
  
  tray_x0 <-
    cups_x0
  
  
  tray_x1 <-
    cups_x1
  
  
  tray_h <-
    row_h *
    0.88
  
  
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
  # CUPS
  # =============================================================
  
  n_cups <-
    min(
      nrow(rows),
      5
    )
  
  
  cell_w <-
    (
      tray_x1 -
        tray_x0
    ) /
    n_cups
  
  
  # Increased from the previous 0.030
  cup_r <-
    min(
      0.036,
      row_h * 0.29
    )
  
  
  for (
    j in seq_len(n_cups)
  ) {
    
    cx <-
      tray_x0 +
      (j - 0.5) *
      cell_w
    
    
    cup_y <-
      cy +
      tray_h *
      0.075
    
    
    # -----------------------------------------------------------
    # CUP
    # -----------------------------------------------------------
    
    draw_cup(
      cx,
      cup_y,
      r = cup_r,
      rim_col = coffee_dark,
      fill_col = rank_cols[j],
      steam = TRUE
    )
    
    
    # -----------------------------------------------------------
    # VALUE
    # -----------------------------------------------------------
    
    grid.text(
      paste0(
        round(
          rows$index[j]
        ),
        "m"
      ),
      x = cx,
      y = cup_y,
      gp = gpar(
        fontsize = 8.5,
        fontfamily = "inter",
        fontface = "bold",
        col = "#FFFFFF"
      )
    )
    
    
    # -----------------------------------------------------------
    # COUNTRY
    # -----------------------------------------------------------
    
    grid.text(
      abbrev_country(
        rows$country[j],
        13
      ),
      x = cx,
      y =
        cy -
        tray_h *
        0.39,
      gp = gpar(
        fontsize = 8.0,
        fontfamily = "inter",
        col = title_color
      ),
      just = "center"
    )
  }
  
  
  # =============================================================
  # ROW DIVIDER
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
        row_bot,
        row_bot
      ),
      gp = gpar(
        col = line_color,
        lwd = 0.4,
        lty = "dotted"
      )
    )
  }
}


# ===============================================================
# CLOSE MAIN VIEWPORT
# ===============================================================

upViewport()


# ===============================================================
# 34. CAFÉS IN NUMBERS
# ===============================================================

cafe_top <-
  plot_ymin -
  0.012


cafe_bottom <-
  0.075


# ---------------------------------------------------------------
# SECTION TITLE
# ---------------------------------------------------------------

gp_cafe_title <- gpar(
  fontsize = 15,
  fontfamily = "bebas",
  col = coffee_dark
)


draw_line(
  "CAFÉS IN NUMBERS",
  x0,
  cafe_top,
  gp_cafe_title
)


# ---------------------------------------------------------------
# SECTION DESCRIPTION
# ---------------------------------------------------------------

gp_cafe_sub <- gpar(
  fontsize = 9.0,
  fontfamily = "inter",
  col = subtitle_color
)


draw_line(
  "The country index is only part of the story. Café-level observations reveal how much prices can vary.",
  x0,
  cafe_top -
    0.028,
  gp_cafe_sub
)


# ===============================================================
# 35. INSIGHT CARDS
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
# 36. CARD FUNCTION
# ===============================================================

draw_card <- function(
    x,
    title,
    value,
    detail,
    accent = coffee_color
) {
  
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
    height = card_h,
    r = unit(
      3,
      "mm"
    ),
    gp = gpar(
      fill = "#F4E4D5",
      col = NA
    )
  )
  
  
  # accent strip
  
  grid.rect(
    x =
      x +
      0.003,
    y =
      card_top -
      0.006,
    width =
      0.006,
    height =
      card_h -
      0.012,
    gp = gpar(
      fill = accent,
      col = NA
    )
  )
  
  
  # title
  
  grid.text(
    title,
    x =
      x +
      0.018,
    y =
      card_top -
      0.015,
    just = c(
      "left",
      "top"
    ),
    gp = gpar(
      fontsize = 7.8,
      fontfamily = "inter",
      fontface = "bold",
      col = footer_color
    )
  )
  
  
  # value
  
  grid.text(
    value,
    x =
      x +
      0.018,
    y =
      card_top -
      0.047,
    just = c(
      "left",
      "top"
    ),
    gp = gpar(
      fontsize = 15.5,
      fontfamily = "inter",
      fontface = "bold",
      col = title_color
    )
  )
  
  
  # detail
  
  grid.text(
    detail,
    x =
      x +
      0.018,
    y =
      card_bottom +
      0.018,
    just = c(
      "left",
      "bottom"
    ),
    gp = gpar(
      fontsize = 7.6,
      fontfamily = "inter",
      col = subtitle_color
    )
  )
}


# ===============================================================
# 37. CARD 1 — SAMPLE
# ===============================================================

draw_card(
  card_x[1],
  "CAFÉ SAMPLE",
  format(
    n_cafes,
    big.mark = ","
  ),
  sprintf(
    "%d countries · %d cities",
    n_countries,
    n_cities
  )
)


# ===============================================================
# 38. CARD 2 — URBANITY
# ===============================================================

draw_card(
  card_x[2],
  "WHERE ARE THE CAFÉS?",
  paste0(
    urban_pct,
    "% urban"
  ),
  sprintf(
    "%d%% suburban · %d%% rural",
    suburban_pct,
    rural_pct
  )
)


# ===============================================================
# 39. CARD 3 — PRICE VARIATION
# ===============================================================

draw_card(
  card_x[3],
  "WIDEST PRICE SPREAD",
  variation_country,
  sprintf(
    "£%.2f → £%.2f · £%.2f range",
    variation_min,
    variation_max,
    variation_range
  )
)


# ===============================================================
# 40. CARD 4 — AFFORDABILITY OUTLIER
# ===============================================================

draw_card(
  card_x[4],
  "MOST WORK-INTENSIVE CAFÉ",
  paste0(
    round(
      outlier_minutes
    ),
    " min"
  ),
  sprintf(
    "%s · £%.2f cappuccino",
    outlier_country,
    outlier_price
  ),
  accent = coffee_dark
)


# ===============================================================
# 41. EDITORIAL TAKEAWAY
# ===============================================================

takeaway_y <-
  cafe_bottom -
  0.018


gp_takeaway <- gpar(
  fontsize = 8.8,
  fontfamily = "inter",
  col = subtitle_color
)


takeaway_text <- paste0(
  "WHY IT MATTERS  ·  ",
  "A cappuccino's price is only half the story. ",
  "The same drink can demand very different amounts of work depending on local wages — ",
  "and the café sample itself is uneven across countries."
)


grid.text(
  takeaway_text,
  x = x0,
  y = takeaway_y,
  just = c(
    "left",
    "top"
  ),
  gp = gp_takeaway
)


# ===============================================================
# 42. FOOTER
# ===============================================================

gp_footer <- gpar(
  fontsize = 7.8,
  fontfamily = "inter",
  col = footer_color
)


footer_bottom <- 0.012


footer_text_y <-
  footer_bottom +
  0.004


footer_divider_y <-
  footer_text_y +
  0.020


grid.lines(
  x = c(
    x0,
    x_max
  ),
  y = c(
    footer_divider_y,
    footer_divider_y
  ),
  gp = gpar(
    col = line_color,
    lwd = 0.8
  )
)


# draw_line(
#   "made with R",
#   x0,
#   footer_text_y +
#     0.012,
#   gp_footer
# )


draw_line(
  "Source: TidyTuesday 2026-09-08 ·Hari Krishna·",
  x_max,
  footer_text_y +
    0.012,
  gp_footer,
  hjust = "right"
)


