# ============================================================
# TIDYTUESDAY — WORLD CASTLES
#
# CASTLEMAPS
#
# EDITORIAL TYPOGRAPHY + FAME SHARE + COUNTRY FLAGS
#
# STORY:
# WHERE DOES LANDMARK FAME CONCENTRATE?
#
# DESIGN:
#   • Bebas Neue → titles + category headings
#   • Inter      → supporting text
#   • Real PNG country flags
#   • Bold category stories
#   • Pageview share explained once
#   • Editorial data-journalism layout
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(scales)
library(ggtext)
library(showtext)
library(ggimage)


# ============================================================
# 2. FONTS
# ============================================================

font_add_google("Inter", "Inter")
font_add_google("Bebas Neue", "Bebas Neue")

showtext_auto()

font_body  <- "Inter"
font_title <- "Bebas Neue"


# ============================================================
# 3. LOAD DATA
# ============================================================

castles <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-01/world_castles.csv",
  show_col_types = FALSE
)


# ============================================================
# 4. SETTINGS
# ============================================================

categories <- c(
  "castle",
  "fortress",
  "palace",
  "ruin"
)


category_labels <- c(
  castle   = "CASTLES",
  fortress = "FORTRESSES",
  palace   = "PALACES",
  ruin     = "RUINS"
)


# Category palette
category_colours <- c(
  castle   = "#3A6EA5",
  fortress = "#C5423D",
  palace   = "#3E8E5B",
  ruin     = "#8E5A9E"
)


# Overall palette
bg_colour  <- "#F8F6F0"
map_fill   <- "#E9E6DD"
map_border <- "#D2CEC2"

ink        <- "#20201D"
muted_ink  <- "#77736A"
light_ink  <- "#AAA69C"


# ============================================================
# 5. TOP 5 PER CATEGORY
# ============================================================

top5 <- castles %>%
  filter(
    category %in% categories,
    !is.na(fame_rank),
    !is.na(lat),
    !is.na(lon)
  ) %>%
  arrange(fame_rank) %>%
  group_by(category) %>%
  slice_head(n = 5) %>%
  mutate(
    category_rank = row_number()
  ) %>%
  ungroup()


cat("\nTOP 5 LANDMARKS\n")

top5 %>%
  select(
    category,
    category_rank,
    name,
    country,
    fame_rank,
    pageviews,
    year
  ) %>%
  print(n = Inf)


# ============================================================
# 6. WORLD BASEMAP
# ============================================================

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)


# ============================================================
# 7. COUNTRY HELPERS
# ============================================================

normalise_country <- function(country_name) {
  
  if_else(
    country_name %in% c(
      "England",
      "Scotland",
      "Wales",
      "Northern Ireland"
    ),
    "United Kingdom",
    country_name
  )
}


# ------------------------------------------------------------
# ISO-2 COUNTRY CODES
#
# These are used only for the flag images.
# ------------------------------------------------------------

country_iso <- c(
  
  # UK
  "England"          = "gb",
  "Scotland"         = "gb",
  "Wales"            = "gb",
  "Northern Ireland" = "gb",
  "United Kingdom"   = "gb",
  
  # Europe
  "France"           = "fr",
  "Germany"          = "de",
  "Italy"            = "it",
  "Spain"            = "es",
  "Portugal"         = "pt",
  "Austria"          = "at",
  "Belgium"          = "be",
  "Netherlands"      = "nl",
  "Switzerland"      = "ch",
  "Ireland"          = "ie",
  "Denmark"          = "dk",
  "Sweden"           = "se",
  "Norway"           = "no",
  "Finland"          = "fi",
  "Iceland"          = "is",
  "Poland"           = "pl",
  "Czech Republic"   = "cz",
  "Czechia"          = "cz",
  "Slovakia"         = "sk",
  "Hungary"          = "hu",
  "Romania"          = "ro",
  "Bulgaria"         = "bg",
  "Croatia"          = "hr",
  "Slovenia"         = "si",
  "Serbia"           = "rs",
  "Greece"           = "gr",
  "Turkey"           = "tr",
  "Ukraine"          = "ua",
  "Russia"           = "ru",
  "Estonia"          = "ee",
  "Latvia"           = "lv",
  "Lithuania"        = "lt",
  
  # Americas
  "United States"    = "us",
  "Canada"           = "ca",
  "Mexico"           = "mx",
  "Brazil"           = "br",
  "Argentina"        = "ar",
  "Chile"            = "cl",
  "Peru"             = "pe",
  "Colombia"         = "co",
  
  # Asia
  "India"            = "in",
  "China"            = "cn",
  "Japan"            = "jp",
  "South Korea"      = "kr",
  "North Korea"      = "kp",
  "Thailand"         = "th",
  "Vietnam"          = "vn",
  "Indonesia"        = "id",
  "Philippines"      = "ph",
  "Israel"           = "il",
  "Jordan"           = "jo",
  
  # Oceania
  "Australia"        = "au",
  "New Zealand"      = "nz",
  
  # Africa
  "South Africa"     = "za",
  "Egypt"            = "eg",
  "Morocco"          = "ma"
)


# ------------------------------------------------------------
# FLAG URL
#
# Real PNG images rather than emoji.
# ------------------------------------------------------------

flag_url <- function(country) {
  
  code <- unname(
    country_iso[as.character(country)]
  )
  
  ifelse(
    is.na(code),
    NA_character_,
    paste0(
      "https://flagcdn.com/48x36/",
      code,
      ".png"
    )
  )
}


# ============================================================
# 8. STORY HELPERS
# ============================================================

category_story <- function(category_name) {
  
  x <- top5 %>%
    filter(
      category == category_name
    )
  
  country_counts <- x %>%
    count(
      country,
      sort = TRUE
    )
  
  leader <- country_counts %>%
    slice_head(n = 1)
  
  total_landmarks <- castles %>%
    filter(
      category == category_name,
      !is.na(lat),
      !is.na(lon)
    ) %>%
    nrow()
  
  
  if (leader$n >= 2) {
    
    paste0(
      leader$country,
      " • ",
      leader$n,
      "/5 of the top landmarks",
      "   |   ",
      comma(total_landmarks),
      " mapped"
    )
    
  } else {
    
    paste0(
      "Five countries share the top five",
      "   |   ",
      comma(total_landmarks),
      " mapped"
    )
  }
}


# ============================================================
# 9. OVERALL STORY
# ============================================================

overall_country_counts <- top5 %>%
  count(
    country,
    sort = TRUE
  )


top_country <- overall_country_counts %>%
  slice_head(n = 1)


runner_up <- overall_country_counts %>%
  slice(2)


big_story <- if (top_country$n >= 2) {
  
  pct_top <- round(
    top_country$n /
      nrow(top5) *
      100
  )
  
  
  if (nrow(runner_up) > 0) {
    
    pct_runner <- round(
      runner_up$n /
        nrow(top5) *
        100
    )
    
    
    paste0(
      top_country$country,
      " leads with ",
      top_country$n,
      " of the 20 landmarks",
      " (",
      pct_top,
      "%)",
      " — ",
      runner_up$country,
      " follows with ",
      runner_up$n,
      " (",
      pct_runner,
      "%)."
    )
    
  } else {
    
    paste0(
      top_country$country,
      " leads with ",
      top_country$n,
      " of the 20 landmarks",
      " (",
      pct_top,
      "%)."
    )
  }
  
} else {
  
  paste0(
    nrow(top5),
    " landmarks span ",
    n_distinct(top5$country),
    " countries."
  )
}


# ============================================================
# 10. VECTOR CATEGORY ICONS
# ============================================================

icon_layer <- function(
    category_name,
    colour
) {
  
  switch(
    
    category_name,
    
    # --------------------------------------------------------
    # CASTLE
    # --------------------------------------------------------
    
    castle = list(
      
      annotate(
        "rect",
        xmin = 0.01,
        xmax = 0.085,
        ymin = 0.55,
        ymax = 0.80,
        fill = colour,
        colour = NA
      ),
      
      annotate(
        "rect",
        xmin = 0.010,
        xmax = 0.030,
        ymin = 0.80,
        ymax = 1.00,
        fill = colour,
        colour = NA
      ),
      
      annotate(
        "rect",
        xmin = 0.0425,
        xmax = 0.0625,
        ymin = 0.80,
        ymax = 1.00,
        fill = colour,
        colour = NA
      ),
      
      annotate(
        "rect",
        xmin = 0.065,
        xmax = 0.085,
        ymin = 0.80,
        ymax = 1.00,
        fill = colour,
        colour = NA
      )
    ),
    
    
    # --------------------------------------------------------
    # FORTRESS
    # --------------------------------------------------------
    
    fortress = list(
      
      annotate(
        "polygon",
        x = c(
          0.01,
          0.01,
          0.0475,
          0.085,
          0.085
        ),
        y = c(
          1.00,
          0.65,
          0.55,
          0.65,
          1.00
        ),
        fill = colour,
        colour = NA
      )
    ),
    
    
    # --------------------------------------------------------
    # PALACE
    # --------------------------------------------------------
    
    palace = list(
      
      annotate(
        "rect",
        xmin = 0.01,
        xmax = 0.085,
        ymin = 0.55,
        ymax = 0.68,
        fill = colour,
        colour = NA
      ),
      
      annotate(
        "rect",
        xmin = 0.01,
        xmax = 0.085,
        ymin = 0.68,
        ymax = 0.72,
        fill = colour,
        colour = NA
      ),
      
      annotate(
        "polygon",
        x = c(
          0.01,
          0.0475,
          0.085
        ),
        y = c(
          0.72,
          1.00,
          0.72
        ),
        fill = colour,
        colour = NA
      )
    ),
    
    
    # --------------------------------------------------------
    # RUIN
    # --------------------------------------------------------
    
    ruin = list(
      
      annotate(
        "rect",
        xmin = 0.015,
        xmax = 0.035,
        ymin = 0.55,
        ymax = 0.78,
        fill = colour,
        colour = NA
      ),
      
      annotate(
        "rect",
        xmin = 0.050,
        xmax = 0.070,
        ymin = 0.55,
        ymax = 0.95,
        fill = colour,
        colour = NA
      ),
      
      annotate(
        "rect",
        xmin = 0.075,
        xmax = 0.090,
        ymin = 0.55,
        ymax = 0.62,
        fill = colour,
        colour = NA
      )
    )
  )
}


# ============================================================
# 11. CATEGORY MAP
# ============================================================

make_category_map <- function(
    category_name,
    point_colour
) {
  
  cat_top5 <- top5 %>%
    filter(
      category == category_name
    ) %>%
    mutate(
      country_norm =
        normalise_country(country)
    )
  
  
  country_summary <- cat_top5 %>%
    group_by(country_norm) %>%
    summarise(
      n_landmarks = n(),
      .groups = "drop"
    )
  
  
  highlight_shapes <- world %>%
    mutate(
      match_key = case_when(
        
        name_long %in%
          country_summary$country_norm ~
          name_long,
        
        name %in%
          country_summary$country_norm ~
          name,
        
        admin %in%
          country_summary$country_norm ~
          admin,
        
        TRUE ~ NA_character_
      )
    ) %>%
    
    filter(
      !is.na(match_key)
    ) %>%
    
    distinct(
      match_key,
      .keep_all = TRUE
    ) %>%
    
    left_join(
      country_summary,
      by = c(
        "match_key" =
          "country_norm"
      )
    )
  
  
  ggplot() +
    
    # World
    geom_sf(
      data = world,
      fill = map_fill,
      colour = map_border,
      linewidth = 0.15
    ) +
    
    # Highlighted countries
    geom_sf(
      data = highlight_shapes,
      aes(fill = n_landmarks),
      colour = point_colour,
      linewidth = 0.65,
      show.legend = FALSE
    ) +
    
    scale_fill_gradient(
      low = alpha(
        point_colour,
        0.30
      ),
      high = point_colour
    ) +
    
    coord_sf(
      crs = "+proj=robin",
      datum = NA,
      expand = TRUE
    ) +
    
    theme_void() +
    
    theme(
      plot.margin =
        margin(
          t = 2,
          r = 4,
          b = 2,
          l = 4
        )
    )
}


# ============================================================
# 12. FAME SHARE LEADERBOARD
#
# FIX: previously the gap between the landmark name and the
# flag (label_space vs flag_space, 0.48x vs 0.08x max_pv) was
# huge, while the flag itself sat almost on top of the rank
# circle. This version defines three explicit gaps so the
# layout reads, left to right, as:
#
#   [ landmark name ]  gap  [ flag ]  gap  [ rank circle | bar ]
#
# and shrinks the total left-side whitespace dramatically.
# ============================================================

# ============================================================
# 12. FAME SHARE LEADERBOARD — DIVERGING BAR CHART
#
# Left  bar = % share of sitelinks  (Wikidata sitelinks)
# Right bar = % share of pageviews  (Wikipedia pageviews)
# Both bars diverge from a shared zero line per landmark.
# ============================================================

# ============================================================
# 12. FAME SHARE LEADERBOARD — DIVERGING BAR CHART
#
# Left  bar = % share of sitelinks  (Wikidata sitelinks)
# Right bar = % share of pageviews  (Wikipedia pageviews)
# Both bars diverge from a shared zero line per landmark.
# No rank numbers — order is conveyed by vertical position only.
# ============================================================

# ============================================================
# 12. FAME SHARE — DUMBBELL CHART
#
# Each landmark gets one row: name + flag on the left,
# then two connected points on a shared 0-100% axis —
#   • hollow point  = % share of sitelinks
#   • solid point   = % share of pageviews
# The connecting segment shows the gap between the two metrics.
# ============================================================

# ============================================================
# 12. FAME SHARE — HORIZONTAL DUMBBELL CHART
#
# One row per landmark:
#   [flag] Name        o------O        (sitelinks % ---- pageviews %)
#
# No coord_flip — rows are placed directly on the y-axis,
# values plotted directly on the x-axis. This keeps the label
# block (flag + name) and the dumbbell perfectly horizontal.
# ============================================================

# ============================================================
# 12. FAME SHARE — HORIZONTAL DUMBBELL CHART
#
# One row per landmark:
#
#   [flag] Name      32%  o──────────O  58%
#                    ^left label      ^right label
#                    (sitelinks)      (pageviews)
#
# Percentages sit just outside each end of the dumbbell,
# not stacked above the points — easier to scan two numbers
# per row at a glance.
# ============================================================

# ============================================================
# 12. FAME SHARE — CLEAN COMPACT DUMBBELL
#
# Layout:
#
# [flag] Landmark name     32%  ○────────●  58%
#
# ○ = Sitelinks share
# ● = Pageview share
#
# No artificial whitespace.
# ============================================================

# ============================================================
# 12. FAME SHARE — MIRRORED LOLLIPOP
#
# Layout:
#
# SITELINKS                              PAGEVIEWS
#
# 32%  ○───────────────┐      ┌───────────────●  58%
# 24%  ○────────┐      │      │      ┌────────●  31%
# 18%  ○──────┐ │      │      │      │ ┌──────●  37%
# 14%  ○────┐ │ │      │      │      │ │ ┌────●   9%
# 12%  ○──┐ │ │ │      │      │      │ │ │ ┌──●  15%
#
# 🇬🇧 Tower of London
# 🇫🇷 Versailles
#
# IMPORTANT:
# The coordinate system is GLOBAL.
# Every category gets exactly the same geometry.
# ============================================================


# ============================================================
# GLOBAL LOLLIPOP GEOMETRY
#
# These values NEVER change between categories.
# ============================================================

lollipop_center <- 55

lollipop_width <- 27

lollipop_left  <- lollipop_center - lollipop_width
lollipop_right <- lollipop_center + lollipop_width

lollipop_name_x <- 16
lollipop_flag_x <- 12

lollipop_value_gap <- 2.0


# ============================================================
# FAME SHARE LOLLIPOP
# ============================================================

make_fame_leaderboard <- function(category_name, colour) {
  
  d <- top5 %>%
    filter(category == category_name) %>%
    mutate(
      sitelinks = replace_na(sitelinks, 0),
      pageviews = replace_na(pageviews, 0)
    ) %>%
    mutate(
      pct_sitelinks = sitelinks / sum(sitelinks) * 100,
      pct_pageviews = pageviews / sum(pageviews) * 100,
      row_y = 5:1
    ) %>%
    mutate(
      x_sitelinks =
        lollipop_center -
        (pct_sitelinks / 100) * lollipop_width,
      
      x_pageviews =
        lollipop_center +
        (pct_pageviews / 100) * lollipop_width,
      
      flag = flag_url(country)
    )
  
  ggplot(d) +
    
    # ------------------------------------------------------
  # Fixed centre reference
  # ------------------------------------------------------
  
  geom_segment(
    aes(
      x = x_sitelinks,
      xend = lollipop_center,
      y = row_y,
      yend = row_y
    ),
    linewidth = 1.5,
    colour = colour,
    alpha = 0.45
  ) +
    
    geom_segment(
      aes(
        x = lollipop_center,
        xend = x_pageviews,
        y = row_y,
        yend = row_y
      ),
      linewidth = 1.5,
      colour = colour,
      alpha = 0.45
    ) +
    
    # ------------------------------------------------------
  # Sitelinks — hollow dot
  # ------------------------------------------------------
  
  geom_point(
    aes(
      x = x_sitelinks,
      y = row_y
    ),
    size = 4,
    shape = 21,
    fill = bg_colour,
    colour = colour,
    stroke = 1.5
  ) +
    
    # ------------------------------------------------------
  # Pageviews — filled dot
  # ------------------------------------------------------
  
  geom_point(
    aes(
      x = x_pageviews,
      y = row_y
    ),
    size = 4,
    shape = 21,
    fill = colour,
    colour = colour,
    stroke = 1.2
  ) +
    
    # ------------------------------------------------------
  # Landmark names
  # ------------------------------------------------------
  
  geom_text(
    aes(
      x = lollipop_name_x,
      y = row_y,
      label = name
    ),
    hjust = 0,
    family = font_body,
    fontface = "bold",
    size = 3.4,
    colour = ink
  ) +
    
    # ------------------------------------------------------
  # Country flags
  # ------------------------------------------------------
  
  ggimage::geom_image(
    aes(
      x = lollipop_flag_x,
      y = row_y,
      image = flag
    ),
    size = 0.099,
    asp = 1.4
  ) +
    
    # ------------------------------------------------------
  # Sitelinks percentage
  # ------------------------------------------------------
  
  geom_text(
    aes(
      x = x_sitelinks - lollipop_value_gap,
      y = row_y,
      label = paste0(round(pct_sitelinks, 1), "%")
    ),
    hjust = 1,
    family = font_body,
    fontface = "bold",
    size = 3,
    colour = ink
  ) +
    
    # ------------------------------------------------------
  # Pageviews percentage
  # ------------------------------------------------------
  
  geom_text(
    aes(
      x = x_pageviews + lollipop_value_gap,
      y = row_y,
      label = paste0(round(pct_pageviews, 1), "%")
    ),
    hjust = 0,
    family = font_body,
    fontface = "bold",
    size = 3,
    colour = colour
  ) +
    
    # ------------------------------------------------------
  # Fixed scales = identical position in every panel
  # ------------------------------------------------------
  
  scale_x_continuous(
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
    
    scale_y_continuous(
      limits = c(0.35, 5.65),
      expand = c(0, 0)
    ) +
    
    theme_void(base_family = font_body) +
    
    theme(
      plot.margin = margin(0, 0, 0, 0),
      panel.spacing = unit(0, "pt")
    )
}

# ============================================================
# 13. CATEGORY PANEL
# ============================================================

make_category_panel <- function(
    category_name
) {
  
  colour <-
    category_colours[[category_name]]
  
  
  story <-
    category_story(
      category_name
    )
  
  
  # ----------------------------------------------------------
  # Header
  # ----------------------------------------------------------
  
  header <-
    
    ggplot() +
    
    icon_layer(
      category_name,
      colour
    ) +
    
    # Category title
    annotate(
      "text",
      x = 0.12,
      y = 1,
      label =
        category_labels[[category_name]],
      hjust = 0,
      vjust = 1,
      size = 8.4,
      fontface = "bold",
      colour = colour,
      family = font_title
    ) +
    
    # Bold story
    annotate(
      "text",
      x = 0,
      y = 0.28,
      label = story,
      hjust = 0,
      vjust = 1,
      size = 3.15,
      colour = ink,
      fontface = "bold",
      family = font_body
    ) +
    
    xlim(
      0,
      1
    ) +
    
    ylim(
      0,
      1
    ) +
    
    theme_void() +
    
    theme(
      plot.margin =
        margin(
          t = 6,
          r = 4,
          b = 2,
          l = 4
        )
    )
  
  
  # ----------------------------------------------------------
  # Combine
  # ----------------------------------------------------------
  
  wrap_plots(
    
    header,
    
    make_category_map(
      category_name,
      colour
    ),
    
    make_fame_leaderboard(
      category_name,
      colour
    ),
    
    ncol = 1,
    
    heights = c(
      0.15,
      0.39,
      0.46
    )
  )
}


# ============================================================
# 14. BUILD PANELS
# ============================================================

castle_panel <-
  make_category_panel(
    "castle"
  )


fortress_panel <-
  make_category_panel(
    "fortress"
  )


palace_panel <-
  make_category_panel(
    "palace"
  )


ruin_panel <-
  make_category_panel(
    "ruin"
  )


# ============================================================
# 15. FINAL 2 × 2 LAYOUT
# ============================================================

final_plot <-
  
  (
    castle_panel |
      fortress_panel
  ) /
  
  (
    palace_panel |
      ruin_panel
  ) +
  
  plot_annotation(
    
    # --------------------------------------------------------
    # Main title
    # --------------------------------------------------------
    
    title =
      "CASTLEMAPS",
    
    # --------------------------------------------------------
    # Subtitle
    # --------------------------------------------------------
    
    subtitle =
      paste0(
        "The World's Most Famous Castles, Mapped by Fame · ",
        big_story
      ),
    
    # --------------------------------------------------------
    # Single global explanation
    # --------------------------------------------------------
    
    # caption =
    #   # paste0(
    #   #   "<b>PAGEVIEW SHARE</b>  ",
    #   #   "Share of combined Wikipedia pageviews among each ",
    #   #   "category's top five landmarks."
    #   # ),
    
    # --------------------------------------------------------
    # Global theme
    # --------------------------------------------------------
    
    theme =
      theme(
        
        # Main title
        plot.title =
          element_text(
            family = font_title,
            size = 42,
            face = "bold",
            colour = ink,
            hjust = 0,
            lineheight = 0.85,
            margin =
              margin(
                b = 5
              )
          ),
        
        # Subtitle
        plot.subtitle =
          element_text(
            family = font_body,
            size = 12.5,
            face = "bold",
            colour = muted_ink,
            hjust = 0,
            lineheight = 1.15,
            margin =
              margin(
                b = 12
              )
          ),
        
        # Single pageview-share explanation
        plot.caption =
          element_markdown(
            family = font_body,
            size = 8.5,
            colour = muted_ink,
            hjust = 0,
            lineheight = 1.2,
            margin =
              margin(
                t = 10
              )
          ),
        
        # Background
        plot.background =
          element_rect(
            fill = bg_colour,
            colour = NA
          ),
        
        # Outer whitespace
        plot.margin =
          margin(
            t = 18,
            r = 22,
            b = 14,
            l = 22
          )
      )
  )


# ============================================================
# 16. SAVE / DISPLAY
# ============================================================


# ------------------------------------------------------------
# Square — Instagram / LinkedIn
# ------------------------------------------------------------

# ggsave(
#   "castle_maps_square.png",
#   final_plot,
#   width = 11,
#   height = 11,
#   dpi = 300,
#   bg = bg_colour
# )


# ------------------------------------------------------------
# Wide — X / Twitter / YouTube
# ------------------------------------------------------------

# ggsave(
#   "castle_maps_wide.png",
#   final_plot,
#   width = 14,
#   height = 7.9,
#   dpi = 300,
#   bg = bg_colour
# )


# ------------------------------------------------------------
# Print / poster
# ------------------------------------------------------------

# ggsave(
#   "castle_maps_print.png",
#   final_plot,
#   width = 14,
#   height = 16,
#   dpi = 300,
#   bg = bg_colour
# )


# ------------------------------------------------------------
# Display
# ------------------------------------------------------------

final_plot


# ============================================================
# 17. ALT TEXT
# ============================================================

alt_text <- paste(
  
  "Editorial data visualization titled CASTLEMAPS.",
  
  "Four panels compare castles, fortresses,",
  "palaces and ruins.",
  
  "Each panel contains a world map highlighting",
  "the countries represented among the category's",
  "five most famous landmarks.",
  
  "A ranked leaderboard shows each landmark's",
  "share of combined Wikipedia pageviews.",
  
  "Country flags appear beside each landmark name.",
  
  "The visualization explores where landmark fame",
  "is concentrated across the world."
)