# ============================================================
# TIDYTUESDAY — WORLD CASTLES  (V10 — VECTOR ICONS)
#
# STORY:
# WHERE DOES LANDMARK FAME CONCENTRATE?
#
# CHANGES FROM V9:
#
# ICONS REBUILT AS VECTOR SHAPES — the unicode emoji icons
# (🏰 🛡️ 🏛️ 🏚️) rendered as blank "tofu" boxes because the R
# graphics device being used doesn't have an emoji-capable font
# installed. Rather than depend on any font being present, each
# icon is now drawn directly with plain ggplot geoms (rectangles
# and a triangle/shield polygon) inside the header panel:
#
#   castle   -> a crenellated tower (body + 3 merlons on top)
#   fortress -> a shield outline
#   palace   -> a pediment (triangle roof) over a columned base
#   ruin     -> two broken columns of uneven height + rubble
#
# These are pure vectors — no font, no external image, no new
# package — so they render identically on every machine and stay
# crisp at any export size. The category name text is shifted
# right slightly (x = 0 -> 0.12) to make room for the icon beside
# it.
#
# NOTE: the icon coordinates below are a reasonable first pass in
# the header's 0-1 x/y space, but I can't render this locally to
# fine-tune pixel-perfect alignment — after you run it, if an icon
# sits a little high/low/too close to the text, nudge the y-range
# (0.55-1.0) or the text's x-offset (0.12) by a few hundredths.
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


# ============================================================
# 2. LOAD DATA
# ============================================================

castles <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-01/world_castles.csv",
  show_col_types = FALSE
)


# ============================================================
# 3. SETTINGS
# ============================================================

categories <- c("castle", "fortress", "palace", "ruin")

category_labels <- c(
  castle   = "CASTLES",
  fortress = "FORTRESSES",
  palace   = "PALACES",
  ruin     = "RUINS"
)

category_colours <- c(
  castle   = "#3A6EA5",
  fortress = "#C5423D",
  palace   = "#3E8E5B",
  ruin     = "#8E5A9E"
)

bg_colour     <- "#FBFAF6"
map_fill      <- "#EDEAE1"
map_border    <- "#D9D5C8"
ink           <- "#20201D"
muted_ink     <- "#6B6960"
font_family   <- "serif"


# ============================================================
# 4. TOP 5 PER CATEGORY
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
  mutate(category_rank = row_number()) %>%
  ungroup()

cat("\nTOP 5 LANDMARKS\n")
top5 %>%
  select(category, category_rank, name, country, fame_rank, pageviews, year) %>%
  print(n = Inf)


# ============================================================
# 5. WORLD BASEMAP
# ============================================================

world <- ne_countries(scale = "medium", returnclass = "sf")

normalise_country <- function(country_name) {
  if_else(
    country_name %in% c("England", "Scotland", "Wales", "Northern Ireland"),
    "United Kingdom",
    country_name
  )
}


# ============================================================
# 6. STORY TEXT HELPERS
# ============================================================

category_story <- function(category_name) {
  
  x <- top5 %>% filter(category == category_name)
  
  country_counts <- x %>% count(country, sort = TRUE)
  leader <- country_counts %>% slice_head(n = 1)
  
  total_landmarks <- castles %>%
    filter(category == category_name, !is.na(lat), !is.na(lon)) %>%
    nrow()
  
  year_span <- max(x$year) - min(x$year)
  
  if (leader$n >= 2) {
    pct_leader <- round(leader$n / nrow(x) * 100)
    paste0(
      leader$country, " claims ", leader$n, " of the top 5 (", pct_leader, "%)  ·  ",
      comma(total_landmarks), " ", category_name, " landmarks mapped  ·  ",
      comma(year_span), "-yr span top-to-bottom"
    )
  } else {
    paste0(
      "Five countries, five leaders  ·  ",
      comma(total_landmarks), " ", category_name, " landmarks mapped  ·  ",
      comma(year_span), "-yr span top-to-bottom"
    )
  }
}

overall_country_counts <- top5 %>% count(country, sort = TRUE)
top_country <- overall_country_counts %>% slice_head(n = 1)
runner_up   <- overall_country_counts %>% slice(2)

big_story <- if (top_country$n >= 2) {
  pct_top <- round(top_country$n / nrow(top5) * 100)
  paste0(
    top_country$country, " appears ", top_country$n,
    " times (", pct_top, "%) among the 20 most famous landmarks on Earth",
    if (nrow(runner_up) > 0) {
      pct_runner <- round(runner_up$n / nrow(top5) * 100)
      paste0(" — ", runner_up$country, " is next with ", runner_up$n, " (", pct_runner, "%).")
    } else {
      "."
    }
  )
} else {
  paste0(nrow(top5), " landmarks span ", n_distinct(top5$country), " different countries.")
}


# ============================================================
# 7. CATEGORY ICON — HAND-DRAWN VECTOR SHAPES
#
# NEW. Returns a list of annotate() layers drawing a small icon
# in the header's 0-1 coordinate space (roughly x: 0.01-0.09,
# y: 0.55-1.0), coloured to match the category. No font or image
# dependency — pure geoms, so they render everywhere.
# ============================================================

icon_layer <- function(category_name, colour) {
  
  switch(
    category_name,
    
    castle = list(
      annotate("rect", xmin = 0.01, xmax = 0.085, ymin = 0.55, ymax = 0.80,
               fill = colour, colour = NA),
      annotate("rect", xmin = 0.010, xmax = 0.030, ymin = 0.80, ymax = 1.00,
               fill = colour, colour = NA),
      annotate("rect", xmin = 0.0425, xmax = 0.0625, ymin = 0.80, ymax = 1.00,
               fill = colour, colour = NA),
      annotate("rect", xmin = 0.065, xmax = 0.085, ymin = 0.80, ymax = 1.00,
               fill = colour, colour = NA)
    ),
    
    fortress = list(
      annotate("polygon",
               x = c(0.01, 0.01, 0.0475, 0.085, 0.085),
               y = c(1.00, 0.65, 0.55, 0.65, 1.00),
               fill = colour, colour = NA)
    ),
    
    palace = list(
      annotate("rect", xmin = 0.01, xmax = 0.085, ymin = 0.55, ymax = 0.68,
               fill = colour, colour = NA),
      annotate("rect", xmin = 0.01, xmax = 0.085, ymin = 0.68, ymax = 0.72,
               fill = colour, colour = NA),
      annotate("polygon",
               x = c(0.01, 0.0475, 0.085),
               y = c(0.72, 1.00, 0.72),
               fill = colour, colour = NA)
    ),
    
    ruin = list(
      annotate("rect", xmin = 0.015, xmax = 0.035, ymin = 0.55, ymax = 0.78,
               fill = colour, colour = NA),
      annotate("rect", xmin = 0.050, xmax = 0.070, ymin = 0.55, ymax = 0.95,
               fill = colour, colour = NA),
      annotate("rect", xmin = 0.075, xmax = 0.090, ymin = 0.55, ymax = 0.62,
               fill = colour, colour = NA)
    )
  )
}


# ============================================================
# 8. CATEGORY MAP — SHADING ONLY, TOP-5 COUNTRIES, NO LABELS
# ============================================================

make_category_map <- function(category_name, point_colour) {
  
  cat_top5 <- top5 %>%
    filter(category == category_name) %>%
    mutate(country_norm = normalise_country(country))
  
  country_summary <- cat_top5 %>%
    group_by(country_norm) %>%
    summarise(n_landmarks = n(), .groups = "drop")
  
  highlight_shapes <- world %>%
    mutate(
      match_key = case_when(
        name_long %in% country_summary$country_norm ~ name_long,
        name      %in% country_summary$country_norm ~ name,
        admin     %in% country_summary$country_norm ~ admin,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(match_key)) %>%
    distinct(match_key, .keep_all = TRUE) %>%
    left_join(country_summary, by = c("match_key" = "country_norm"))
  
  ggplot() +
    
    geom_sf(data = world, fill = map_fill, colour = map_border, linewidth = 0.15) +
    
    geom_sf(
      data = highlight_shapes,
      aes(fill = n_landmarks),
      colour = point_colour,
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    scale_fill_gradient(low = alpha(point_colour, 0.35), high = point_colour) +
    
    coord_sf(crs = "+proj=robin", datum = NA, expand = TRUE) +
    theme_void() +
    theme(plot.margin = margin(t = 2, r = 4, b = 2, l = 4))
}


# ============================================================
# 9. FAME-SHARE LEADERBOARD
# ============================================================

make_fame_leaderboard <- function(category_name, point_colour) {
  
  d <- top5 %>%
    filter(category == category_name) %>%
    mutate(
      pct_share  = pageviews / sum(pageviews) * 100,
      year_label = if_else(
        year < 0,
        paste0(comma(abs(year)), " BCE"),
        paste0(comma(year), " CE")
      ),
      name_label = fct_reorder(str_wrap(name, 18), -category_rank)
    )
  
  max_pv   <- max(d$pageviews)
  badge_y  <- max_pv * 0.075
  gap      <- max_pv * 0.07
  pct_x    <- d$pageviews + gap
  year_x   <- d$pageviews + gap + max_pv * 0.36
  
  ggplot(d, aes(x = name_label, y = pageviews)) +
    
    geom_col(aes(alpha = category_rank), fill = point_colour, width = 0.42) +
    scale_alpha_continuous(range = c(1, 0.42), guide = "none") +
    
    geom_point(
      aes(y = badge_y), shape = 21, size = 7.5,
      fill = "white", colour = point_colour, stroke = 1.2
    ) +
    geom_text(
      aes(y = badge_y, label = category_rank),
      colour = point_colour, fontface = "bold", family = font_family, size = 3
    ) +
    
    geom_label(
      aes(y = pct_x, label = paste0(round(pct_share), "%")),
      hjust = 0,
      colour = point_colour,
      fill = alpha(point_colour, 0.14),
      label.size = 0,
      label.padding = unit(0.22, "lines"),
      label.r = unit(0.14, "lines"),
      fontface = "bold", family = font_family, size = 5.4
    ) +
    
    geom_text(
      aes(y = year_x, label = year_label),
      hjust = 0, colour = muted_ink, family = font_family, size = 3.2
    ) +
    
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.9))) +
    labs(x = NULL, y = NULL) +
    theme_void() +
    theme(
      axis.text.y = element_text(
        size = 9.5, face = "bold", colour = ink, hjust = 1,
        family = font_family, margin = margin(r = 8), lineheight = 0.85
      ),
      plot.margin = margin(t = 8, r = 50, b = 4, l = 4)
    )
}


# ============================================================
# 10. CATEGORY PANEL = HEADER + MAP + FAME-SHARE LEADERBOARD
#
# CHANGED: header now draws icon_layer() before the text, and the
# category name text is shifted right (x = 0 -> 0.12) to sit next
# to the icon instead of on top of it.
# ============================================================

make_category_panel <- function(category_name) {
  
  colour <- category_colours[[category_name]]
  story  <- category_story(category_name)
  
  header <- ggplot() +
    icon_layer(category_name, colour) +
    annotate(
      "text", x = 0.12, y = 1, label = category_labels[[category_name]],
      hjust = 0, vjust = 1, size = 7.6, fontface = "bold", colour = colour, family = font_family
    ) +
    annotate(
      "text", x = 0, y = 0.32, label = story,
      hjust = 0, vjust = 1, size = 3.6, colour = muted_ink, family = font_family
    ) +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    theme(plot.margin = margin(t = 6, r = 4, b = 2, l = 4))
  
  wrap_plots(
    header,
    make_category_map(category_name, colour),
    make_fame_leaderboard(category_name, colour),
    ncol = 1,
    heights = c(0.16, 0.44, 0.40)
  )
}


# ============================================================
# 11. BUILD THE 2x2 GRID
# ============================================================

castle_panel   <- make_category_panel("castle")
fortress_panel <- make_category_panel("fortress")
palace_panel   <- make_category_panel("palace")
ruin_panel     <- make_category_panel("ruin")

final_plot <-
  (castle_panel | fortress_panel) /
  (palace_panel  | ruin_panel) +
  
  plot_annotation(
    title = "Kingdoms of Clicks",
    subtitle = paste0(
      "Where the world's most famous castles, fortresses, palaces and ruins rule — ",
      "ranked by Wikidata fame and Wikipedia attention  ·  ", big_story
    ),
    caption = paste0(
      "**Darker shading = a country holds more of that category's top 5.**  ",
      "Bars show each landmark's share of the top 5's combined Wikipedia pageviews, with build year.<br>",
      "Data: TidyTuesday · world_castles (Wikidata)"
    ),
    theme = theme(
      plot.title = element_text(size = 30, face = "bold", colour = ink, hjust = 0, margin = margin(b = 6), family = font_family),
      plot.subtitle = element_text(size = 13.5, colour = muted_ink, hjust = 0, margin = margin(b = 16), family = font_family),
      plot.caption = element_markdown(size = 9.5, colour = muted_ink, hjust = 0, lineheight = 1.3, margin = margin(t = 12), family = font_family),
      plot.background = element_rect(fill = bg_colour, colour = NA),
      plot.margin = margin(t = 18, r = 20, b = 14, l = 20)
    )
  )


# ============================================================
# 12. SAVE / DISPLAY
# ============================================================

# Square (Instagram / LinkedIn feed)
# ggsave("world_castles_fame_square.png", final_plot, width = 11, height = 11, dpi = 300, bg = bg_colour)

# 16:9 (Twitter/X, YouTube community post)
# ggsave("world_castles_fame_wide.png", final_plot, width = 14, height = 7.9, dpi = 300, bg = bg_colour)

# Original print-style proportions
# ggsave("world_castles_fame_map.png", final_plot, width = 14, height = 16, dpi = 300, bg = bg_colour)

final_plot


# ============================================================
# 13. ALT TEXT (for accessibility when posting)
# ============================================================

alt_text <- paste(
  "Four panels comparing the world's most famous castles, fortresses,",
  "palaces, and ruins, each marked with an icon. Each panel shows a",
  "world map shaded by which countries hold that category's top 5 most",
  "famous landmarks, plus a ranked bar chart showing each landmark's",
  "share of Wikipedia pageviews and its year built."
)