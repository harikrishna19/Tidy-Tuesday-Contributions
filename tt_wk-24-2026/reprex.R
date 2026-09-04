# ============================================================
# TIDYTUESDAY — WORLD CASTLES
#
# STORY:
# WHERE DOES LANDMARK FAME CONCENTRATE?
#
# Headline finding (computed dynamically below, not hardcoded):
#   England appears 4x among the 20 most famous landmarks in this
#   dataset — more than any other country. France is next with 3.
#
# DESIGN CHANGE FROM V1:
#   V1 stacked one small map per unique country in a column, which
#   made each category column a different length (England/India
#   collapsed two landmarks into one card, most others didn't) —
#   so the four columns never lined up and the piece read as a
#   list of stamps rather than a comparison.
#
#   V2 gives every category ONE panel of fixed structure:
#     (a) a world dot-map — only the countries with a top-5 landmark
#         are highlighted, bubble size = rank (bigger = more famous)
#     (b) a ranked bar chart below it, using real Wikipedia pageviews
#         — so you see not just WHO is #1 but by how much
#   Four identical panels in a 2x2 grid = a real comparison, and it
#   scales cleanly if the dataset grows next year.
#
# Four categories:
#   CASTLES | FORTRESSES | PALACES | RUINS
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(ggrepel)
library(scales)


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

# A slightly warmer, more editorial palette than V1 —
# still distinct at a glance, less "default ggplot" feeling.
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
  select(category, category_rank, name, country, fame_rank, pageviews) %>%
  print(n = Inf)


# ============================================================
# 5. WORLD BASEMAP
# ============================================================

world <- ne_countries(scale = "medium", returnclass = "sf")

# Natural Earth has no separate polygons for the UK's constituent
# countries, so we fold them into "United Kingdom" for matching —
# same fix as V1, just centralised into one helper used everywhere.
normalise_country <- function(country_name) {
  if_else(
    country_name %in% c("England", "Scotland", "Wales", "Northern Ireland"),
    "United Kingdom",
    country_name
  )
}

match_world <- function(names_vec) {
  world %>%
    filter(name_long %in% names_vec | name %in% names_vec | admin %in% names_vec)
}


# ============================================================
# 6. STORY TEXT HELPERS
# ============================================================

category_story <- function(category_name) {
  
  x <- top5 %>% filter(category == category_name)
  
  country_counts <- x %>% count(country, sort = TRUE)
  leader <- country_counts %>% slice_head(n = 1)
  n_countries <- n_distinct(x$country)
  
  total_landmarks <- castles %>%
    filter(category == category_name, !is.na(lat), !is.na(lon)) %>%
    nrow()
  
  if (leader$n >= 2) {
    paste0(
      leader$country, " claims ", leader$n, " of the top 5  ·  ",
      comma(total_landmarks), " ", category_name, " landmarks mapped"
    )
  } else {
    paste0(
      "Five countries, five leaders  ·  ",
      comma(total_landmarks), " ", category_name, " landmarks mapped"
    )
  }
}

# Overall headline: which country shows up most across ALL 20
# top-5 landmarks, spanning every category. This is the number
# that should anchor the piece — it's the most surprising one.
overall_country_counts <- top5 %>% count(country, sort = TRUE)
top_country <- overall_country_counts %>% slice_head(n = 1)
runner_up   <- overall_country_counts %>% slice(2)

big_story <- if (top_country$n >= 2) {
  paste0(
    top_country$country, " appears ", top_country$n,
    " times among the 20 most famous landmarks on Earth",
    if (nrow(runner_up) > 0) {
      paste0(" — ", runner_up$country, " is next with ", runner_up$n, ".")
    } else {
      "."
    }
  )
} else {
  paste0(nrow(top5), " landmarks span ", n_distinct(top5$country), " different countries.")
}


# ============================================================
# 7. CATEGORY DOT-MAP
# ============================================================

make_category_map <- function(category_name, point_colour) {
  
  cat_top5 <- top5 %>% filter(category == category_name) %>% arrange(category_rank)
  
  highlight_names <- unique(normalise_country(cat_top5$country))
  highlight_shapes <- match_world(highlight_names)
  
  points_sf <- cat_top5 %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  # Label only the top 3 to keep the map legible — the timeline
  # underneath carries #4 and #5 by name anyway.
  label_data <- cat_top5 %>% filter(category_rank <= 3)
  
  label_coords <- label_data %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    bind_cols(label_data %>% select(name, category_rank))
  
  ggplot() +
    
    geom_sf(data = world, fill = map_fill, colour = map_border, linewidth = 0.15) +
    
    geom_sf(
      data = highlight_shapes,
      fill = alpha(point_colour, 0.14),
      colour = alpha(point_colour, 0.55),
      linewidth = 0.35
    ) +
    
    # Bubble size communicates rank at a glance: #1 is the biggest dot.
    geom_sf(
      data = points_sf,
      aes(size = 6 - category_rank),
      shape = 21,
      fill = point_colour,
      colour = "white",
      stroke = 0.7,
      alpha = 0.95
    ) +
    scale_size_continuous(range = c(2.4, 6.4), guide = "none") +
    
    geom_text_repel(
      data = label_coords,
      aes(X, Y, label = paste0("#", category_rank, "  ", str_wrap(name, 16))),
      size = 2.5,
      fontface = "bold",
      colour = ink,
      lineheight = 0.9,
      box.padding = 0.4,
      point.padding = 0.3,
      segment.colour = alpha(point_colour, 0.8),
      segment.linewidth = 0.35,
      min.segment.length = 0,
      seed = 42
    ) +
    
    coord_sf(crs = "+proj=robin", datum = NA, expand = TRUE) +
    theme_void() +
    theme(plot.margin = margin(t = 2, r = 4, b = 2, l = 4))
}


# ============================================================
# 8. CATEGORY TIMELINE
#
# The map's bubble size already encodes rank/fame, so a bar chart
# of the same top-5 would mostly repeat that story. Instead this
# plots WHEN each landmark was built — a dimension the map can't
# show at all — with bubble size still tied to pageviews. The
# span between oldest and newest is computed and shown as a
# caption, which tends to be a genuinely surprising number
# (fortresses in particular can span 1,000+ years).
# ============================================================

make_timeline <- function(category_name, point_colour) {
  
  d <- top5 %>%
    filter(category == category_name) %>%
    arrange(year) %>%
    mutate(
      idx        = row_number(),
      side       = if_else(idx %% 2 == 1, 1, -1),   # alternate labels above/below the line
      year_label = if_else(
        year < 0,
        paste0(comma(abs(year)), " BCE"),
        paste0(comma(year), " CE")
      )
    )
  
  span_years <- max(d$year) - min(d$year)
  span_text  <- paste0(comma(span_years), "-year span from oldest to newest in the top 5")
  
  ggplot(d, aes(x = year, y = 0)) +
    
    geom_hline(yintercept = 0, colour = alpha(point_colour, 0.4), linewidth = 0.5) +
    
    geom_segment(
      aes(xend = year, y = 0, yend = side * 0.55),
      colour = alpha(point_colour, 0.55), linewidth = 0.35
    ) +
    
    # Bubble size = pageviews, so the "who's most visited" story
    # from V2's bar chart survives — just folded into a richer plot.
    geom_point(
      aes(size = pageviews),
      shape = 21, fill = point_colour, colour = "white", stroke = 0.7, alpha = 0.95
    ) +
    scale_size_continuous(range = c(3.4, 9.6), guide = "none") +
    
    geom_text(
      data = d %>% filter(side == 1),
      aes(y = 0.68, label = paste0("#", category_rank, "  ", str_wrap(name, 14))),
      size = 2.25, fontface = "bold", colour = ink, lineheight = 0.85, vjust = 0
    ) +
    geom_text(
      data = d %>% filter(side == 1),
      aes(y = 1.05, label = year_label),
      size = 1.95, colour = muted_ink, vjust = 0
    ) +
    geom_text(
      data = d %>% filter(side == -1),
      aes(y = -0.68, label = paste0("#", category_rank, "  ", str_wrap(name, 14))),
      size = 2.25, fontface = "bold", colour = ink, lineheight = 0.85, vjust = 1
    ) +
    geom_text(
      data = d %>% filter(side == -1),
      aes(y = -1.05, label = year_label),
      size = 1.95, colour = muted_ink, vjust = 1
    ) +
    
    scale_x_continuous(expand = expansion(mult = 0.16)) +
    ylim(-1.9, 1.9) +
    labs(caption = span_text) +
    theme_void() +
    theme(
      plot.caption = element_text(
        size = 6.9, colour = point_colour, face = "bold", hjust = 0.5, margin = margin(t = 4)
      ),
      plot.margin = margin(t = 8, r = 10, b = 4, l = 10)
    )
}


# ============================================================
# 9. CATEGORY PANEL = HEADER + MAP + LEADERBOARD
# ============================================================

make_category_panel <- function(category_name) {
  
  colour <- category_colours[[category_name]]
  story  <- category_story(category_name)
  
  header <- ggplot() +
    annotate(
      "text", x = 0, y = 1, label = category_labels[[category_name]],
      hjust = 0, vjust = 1, size = 6.2, fontface = "bold", colour = colour
    ) +
    annotate(
      "text", x = 0, y = 0.32, label = story,
      hjust = 0, vjust = 1, size = 2.85, colour = muted_ink
    ) +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    theme(plot.margin = margin(t = 6, r = 4, b = 2, l = 4))
  
  wrap_plots(
    header,
    make_category_map(category_name, colour),
    make_leaderboard(category_name, colour),
    ncol = 1,
    heights = c(0.16, 0.50, 0.34)
  )
}


# ============================================================
# 10. BUILD THE 2x2 GRID
# ============================================================

castle_panel   <- make_category_panel("castle")
fortress_panel <- make_category_panel("fortress")
palace_panel   <- make_category_panel("palace")
ruin_panel     <- make_category_panel("ruin")

final_plot <-
  (castle_panel | fortress_panel) /
  (palace_panel  | ruin_panel) +
  
  plot_annotation(
    title = "Where does landmark fame concentrate?",
    subtitle = paste0(
      "The world's 5 most famous castles, fortresses, palaces and ruins, ranked by ",
      "Wikidata fame rank  ·  ", big_story
    ),
    caption = paste0(
      "Bubble size = rank within category (larger = more famous)  ·  ",
      "Bar length = Wikipedia pageviews\n",
      "Data: TidyTuesday · world_castles (Wikidata)"
    ),
    theme = theme(
      plot.title = element_text(size = 23, face = "bold", colour = ink, hjust = 0, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 10.5, colour = muted_ink, hjust = 0, margin = margin(b = 14)),
      plot.caption = element_text(size = 7.6, colour = muted_ink, hjust = 0, lineheight = 1.3, margin = margin(t = 10)),
      plot.background = element_rect(fill = bg_colour, colour = NA),
      plot.margin = margin(t = 16, r = 18, b = 12, l = 18)
    )
  )


# ============================================================
# 11. SAVE / DISPLAY
# ============================================================

ggsave(
  "world_castles_fame_map.png",
  final_plot,
  width = 12, height = 11.5, dpi = 300, bg = bg_colour
)

final_plot