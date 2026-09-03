# ============================================================
# TIDYTUESDAY — WORLD CASTLES
#
# STORY:
# WHERE DOES LANDMARK FAME CONCENTRATE?
#
# Four categories:
#   CASTLES | FORTRESSES | PALACES | RUINS
#
# DESIGN:
#   • Top 5 landmarks by fame_rank
#   • One map per country
#   • Countries appearing multiple times are NOT repeated
#   • All landmarks in that country/category shown as faint dots
#   • Top-5 landmarks highlighted
#   • Rank shown on highlighted landmarks
#   • Dynamic editorial insights
#
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


category_colours <- c(
  castle   = "#4C78A8",
  fortress = "#E45756",
  palace   = "#59A14F",
  ruin     = "#B279A2"
)


# ============================================================
# 4. BASIC DATA CHECK
# ============================================================

cat("\nCATEGORY COUNTS\n")
print(
  castles %>%
    count(category, sort = TRUE)
)


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
  
  arrange(
    fame_rank
  ) %>%
  
  group_by(
    category
  ) %>%
  
  slice_head(
    n = 5
  ) %>%
  
  mutate(
    category_rank = row_number()
  ) %>%
  
  ungroup()


# ============================================================
# 6. VIEW TOP 5
# ============================================================

cat("\nTOP 5 LANDMARKS\n")

top5 %>%
  
  select(
    category,
    category_rank,
    name,
    country,
    fame_rank,
    lat,
    lon
  ) %>%
  
  print(
    n = Inf
  )


# ============================================================
# 7. WORLD MAP
# ============================================================

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)


# ============================================================
# 8. ALL LANDMARKS -> SF
# ============================================================

all_points <- castles %>%
  
  filter(
    !is.na(lat),
    !is.na(lon)
  ) %>%
  
  st_as_sf(
    coords = c(
      "lon",
      "lat"
    ),
    crs = 4326,
    remove = FALSE
  )


# ============================================================
# 9. COUNTRY SHAPE FUNCTION
# ============================================================

get_country_shape <- function(
    country_name
) {
  
  result <- world %>%
    
    filter(
      name_long == country_name |
        name == country_name |
        admin == country_name
    )
  
  
  # ----------------------------------------------------------
  # UK constituent countries
  # ----------------------------------------------------------
  
  if (
    nrow(result) == 0 &&
    country_name %in% c(
      "England",
      "Scotland",
      "Wales",
      "Northern Ireland"
    )
  ) {
    
    result <- world %>%
      
      filter(
        name_long == "United Kingdom"
      )
  }
  
  
  result
}


# ============================================================
# 10. COUNTRY STATISTICS
# ============================================================

get_country_stats <- function(
    category_name,
    country_name
) {
  
  # ----------------------------------------------------------
  # All landmarks
  # ----------------------------------------------------------
  
  all_count <- castles %>%
    
    filter(
      category == category_name,
      country == country_name,
      !is.na(lat),
      !is.na(lon)
    ) %>%
    
    nrow()
  
  
  # ----------------------------------------------------------
  # Top 5 landmarks
  # ----------------------------------------------------------
  
  selected <- top5 %>%
    
    filter(
      category == category_name,
      country == country_name
    ) %>%
    
    arrange(
      category_rank
    )
  
  
  tibble(
    
    top5_count =
      nrow(selected),
    
    all_count =
      all_count,
    
    best_rank =
      ifelse(
        nrow(selected) > 0,
        min(selected$category_rank),
        NA
      ),
    
    worst_rank =
      ifelse(
        nrow(selected) > 0,
        max(selected$category_rank),
        NA
      )
  )
}


# ============================================================
# 11. EDITORIAL COUNTRY INSIGHT
# ============================================================

country_insight <- function(
    category_name,
    country_name
) {
  
  stats <- get_country_stats(
    category_name,
    country_name
  )
  
  
  n_top5 <- stats$top5_count
  n_all  <- stats$all_count
  best   <- stats$best_rank
  
  
  # ----------------------------------------------------------
  # Multiple Top 5 landmarks
  # ----------------------------------------------------------
  
  if (n_top5 >= 3) {
    
    return(
      paste0(
        n_top5,
        " of the Top 5 • ",
        comma(n_all),
        " ",
        category_name,
        " landmarks"
      )
    )
  }
  
  
  if (n_top5 == 2) {
    
    return(
      paste0(
        "2 of the Top 5 • ",
        comma(n_all),
        " ",
        category_name,
        " landmarks"
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # #1
  # ----------------------------------------------------------
  
  if (best == 1) {
    
    return(
      paste0(
        "Home to the #1 ",
        category_name,
        " • ",
        comma(n_all),
        " landmarks"
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Everything else
  # ----------------------------------------------------------
  
  paste0(
    "Top 5 landmark • ",
    comma(n_all),
    " ",
    category_name,
    " landmarks"
  )
}


# ============================================================
# 12. CATEGORY-LEVEL STORY
# ============================================================

category_story <- function(
    category_name
) {
  
  # ----------------------------------------------------------
  # Top 5
  # ----------------------------------------------------------
  
  x <- top5 %>%
    
    filter(
      category == category_name
    )
  
  
  # ----------------------------------------------------------
  # Countries
  # ----------------------------------------------------------
  
  country_counts <- x %>%
    
    count(
      country,
      sort = TRUE
    )
  
  
  leader <- country_counts %>%
    
    slice_head(
      n = 1
    )
  
  
  n_countries <- n_distinct(
    x$country
  )
  
  
  # ----------------------------------------------------------
  # All landmarks
  # ----------------------------------------------------------
  
  total_landmarks <- castles %>%
    
    filter(
      category == category_name,
      !is.na(lat),
      !is.na(lon)
    ) %>%
    
    nrow()
  
  
  # ----------------------------------------------------------
  # Story
  # ----------------------------------------------------------
  
  if (leader$n >= 2) {
    
    paste0(
      leader$country,
      " claims ",
      leader$n,
      " of the Top 5 • ",
      n_countries,
      " countries represented"
    )
    
  } else {
    
    paste0(
      "Five landmarks across ",
      n_countries,
      " countries • ",
      comma(total_landmarks),
      " landmarks mapped"
    )
  }
}


# ============================================================
# 13. CATEGORY HEADER
# ============================================================

make_category_header <- function(
    category_name,
    colour
) {
  
  story <- category_story(
    category_name
  )
  
  
  ggplot() +
    
    # --------------------------------------------------------
  # CATEGORY
  # --------------------------------------------------------
  
  annotate(
    "text",
    x = 0,
    y = 1,
    label = category_labels[
      category_name
    ],
    hjust = 0,
    vjust = 1,
    size = 6.5,
    fontface = "bold",
    colour = colour
  ) +
    
    # --------------------------------------------------------
  # STORY
  # --------------------------------------------------------
  
  annotate(
    "text",
    x = 0,
    y = 0.42,
    label = str_wrap(
      story,
      width = 34
    ),
    hjust = 0,
    vjust = 1,
    size = 2.9,
    colour = "#555555"
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
      plot.margin = margin(
        t = 8,
        r = 6,
        b = 5,
        l = 6
      )
    )
}


# ============================================================
# 14. COUNTRY MAP
# ============================================================

make_country_map <- function(
    category_name,
    country_name,
    point_colour
) {
  
  
  # ----------------------------------------------------------
  # COUNTRY SHAPE
  # ----------------------------------------------------------
  
  country_shape <- get_country_shape(
    country_name
  )
  
  
  # ----------------------------------------------------------
  # ALL LANDMARKS IN COUNTRY
  # ----------------------------------------------------------
  
  country_points <- all_points %>%
    
    filter(
      category == category_name,
      country == country_name
    )
  
  
  # ----------------------------------------------------------
  # TOP 5 LANDMARKS
  # ----------------------------------------------------------
  
  selected_points <- top5 %>%
    
    filter(
      category == category_name,
      country == country_name
    ) %>%
    
    arrange(
      category_rank
    ) %>%
    
    st_as_sf(
      coords = c(
        "lon",
        "lat"
      ),
      crs = 4326,
      remove = FALSE
    )
  
  
  # ----------------------------------------------------------
  # COUNTRY NOT FOUND
  # ----------------------------------------------------------
  
  if (
    nrow(country_shape) == 0
  ) {
    
    return(
      
      ggplot() +
        
        annotate(
          "text",
          x = 0,
          y = 0,
          label = country_name,
          size = 5,
          fontface = "bold"
        ) +
        
        theme_void()
    )
  }
  
  
  # ==========================================================
  # LABEL COORDINATES
  # ==========================================================
  
  label_coords <- selected_points %>%
    
    st_coordinates() %>%
    
    as.data.frame() %>%
    
    bind_cols(
      
      selected_points %>%
        
        st_drop_geometry() %>%
        
        select(
          name,
          category_rank,
          fame_rank
        )
    )
  
  
  # ==========================================================
  # MAP
  # ==========================================================
  
  ggplot() +
    
    # --------------------------------------------------------
  # COUNTRY
  # --------------------------------------------------------
  
  geom_sf(
    data = country_shape,
    
    fill = "#F4F2EC",
    
    colour = "#BDBBB4",
    
    linewidth = 0.45
  ) +
    
    # --------------------------------------------------------
  # ALL LANDMARKS
  #
  # Small muted dots provide geographic context.
  # --------------------------------------------------------
  
  geom_sf(
    data = country_points,
    
    shape = 16,
    
    size = 0.65,
    
    alpha = 0.30,
    
    colour = "#8D8D87"
  ) +
    
    # --------------------------------------------------------
  # TOP LANDMARK OUTER GLOW
  # --------------------------------------------------------
  
  geom_sf(
    data = selected_points,
    
    shape = 21,
    
    size = 9,
    
    fill = point_colour,
    
    colour = NA,
    
    alpha = 0.10
  ) +
    
    # --------------------------------------------------------
  # TOP LANDMARK WHITE RING
  # --------------------------------------------------------
  
  geom_sf(
    data = selected_points,
    
    shape = 21,
    
    size = 5.4,
    
    fill = "#FFFFFF",
    
    colour = point_colour,
    
    linewidth = 1
  ) +
    
    # --------------------------------------------------------
  # TOP LANDMARK CENTRE
  # --------------------------------------------------------
  
  geom_sf(
    data = selected_points,
    
    shape = 21,
    
    size = 2.8,
    
    fill = point_colour,
    
    colour = NA
  ) +
    
    # --------------------------------------------------------
  # RANK LABEL
  # --------------------------------------------------------
  
  geom_text_repel(
    
    data = label_coords,
    
    aes(
      X,
      Y,
      label = paste0(
        "#",
        category_rank
      )
    ),
    
    size = 2.7,
    
    fontface = "bold",
    
    colour = "#222222",
    
    box.padding = 0.35,
    
    point.padding = 0.25,
    
    segment.colour = "#8A8A8A",
    
    segment.linewidth = 0.30,
    
    min.segment.length = 0,
    
    seed = 42
  ) +
    
    # --------------------------------------------------------
  # NAME ONLY FOR #1
  #
  # Prevents clutter while identifying the most important
  # landmark in each category.
  # --------------------------------------------------------
  
  geom_text_repel(
    
    data = label_coords %>%
      
      filter(
        category_rank == 1
      ),
    
    aes(
      X,
      Y,
      label = str_wrap(
        name,
        width = 18
      )
    ),
    
    size = 2.45,
    
    fontface = "bold",
    
    colour = "#222222",
    
    box.padding = 0.7,
    
    point.padding = 0.5,
    
    segment.colour = "#777777",
    
    segment.linewidth = 0.35,
    
    min.segment.length = 0,
    
    seed = 100
  ) +
    
    # --------------------------------------------------------
  # ZOOM
  # --------------------------------------------------------
  
  coord_sf(
    datum = NA,
    expand = TRUE
  ) +
    
    # --------------------------------------------------------
  # TITLE
  # --------------------------------------------------------
  
  labs(
    
    title =
      country_name,
    
    subtitle =
      country_insight(
        category_name,
        country_name
      )
  ) +
    
    # --------------------------------------------------------
  # THEME
  # --------------------------------------------------------
  
  theme_void() +
    
    theme(
      
      plot.title = element_text(
        size = 10.5,
        face = "bold",
        colour = "#202020",
        hjust = 0.5,
        margin = margin(
          t = 3,
          b = 1
        )
      ),
      
      plot.subtitle = element_text(
        size = 7.2,
        colour = point_colour,
        face = "bold",
        hjust = 0.5,
        margin = margin(
          t = 0,
          b = 5
        )
      ),
      
      plot.margin = margin(
        t = 3,
        r = 5,
        b = 6,
        l = 5
      )
    )
}


# ============================================================
# 15. CATEGORY COLUMN
#
# THIS IS THE IMPORTANT PART.
#
# Instead of:
#
#   France
#   France
#   Japan
#   Germany
#
# We create:
#
#   France
#   Japan
#   Germany
#
# with multiple Top-5 landmarks on the same France map.
# ============================================================

make_category_column <- function(
    category_name,
    point_colour
) {
  
  
  # ----------------------------------------------------------
  # Top 5 for category
  # ----------------------------------------------------------
  
  category_top5 <- top5 %>%
    
    filter(
      category == category_name
    )
  
  
  # ----------------------------------------------------------
  # UNIQUE COUNTRIES
  #
  # Sort according to the best-ranked landmark.
  # ----------------------------------------------------------
  
  countries <- category_top5 %>%
    
    group_by(
      country
    ) %>%
    
    summarise(
      
      best_rank =
        min(category_rank),
      
      n_top5 =
        n(),
      
      .groups = "drop"
    ) %>%
    
    arrange(
      best_rank
    )
  
  
  # ----------------------------------------------------------
  # CREATE MAPS
  # ----------------------------------------------------------
  
  maps <- map(
    
    seq_len(
      nrow(countries)
    ),
    
    function(i) {
      
      make_country_map(
        
        category_name =
          category_name,
        
        country_name =
          countries$country[i],
        
        point_colour =
          point_colour
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # HEADER
  # ----------------------------------------------------------
  
  header <- make_category_header(
    
    category_name =
      category_name,
    
    colour =
      point_colour
  )
  
  
  # ----------------------------------------------------------
  # COMBINE HEADER + MAPS
  # ----------------------------------------------------------
  
  wrap_plots(
    
    c(
      list(header),
      maps
    ),
    
    ncol = 1,
    
    heights = c(
      0.58,
      rep(
        1,
        length(maps)
      )
    )
  )
}


# ============================================================
# 16. BUILD FOUR COLUMNS
# ============================================================

castle_column <- make_category_column(
  
  category_name =
    "castle",
  
  point_colour =
    category_colours["castle"]
)


fortress_column <- make_category_column(
  
  category_name =
    "fortress",
  
  point_colour =
    category_colours["fortress"]
)


palace_column <- make_category_column(
  
  category_name =
    "palace",
  
  point_colour =
    category_colours["palace"]
)


ruin_column <- make_category_column(
  
  category_name =
    "ruin",
  
  point_colour =
    category_colours["ruin"]
)


# ============================================================
# 17. OVERALL COUNTRY STORY
# ============================================================

overall_country_counts <- top5 %>%
  
  count(
    country,
    sort = TRUE
  )


top_country <- overall_country_counts %>%
  
  slice_head(
    n = 1
  )


overall_countries <- n_distinct(
  top5$country
)


total_top5 <- nrow(
  top5
)


# ============================================================
# 18. OVERALL STORY
# ============================================================

big_story <- if (
  top_country$n >= 2
) {
  
  paste0(
    
    top_country$country[1],
    
    " appears ",
    top_country$n,
    
    " times among the 20 most famous landmarks"
  )
  
} else {
  
  paste0(
    
    total_top5,
    
    " landmarks span ",
    
    overall_countries,
    
    " countries"
  )
}


# ============================================================
# 19. FINAL PLOT
# ============================================================

final_plot <-
  
  (
    castle_column |
      
      fortress_column |
      
      palace_column |
      
      ruin_column
  ) +
  
  plot_annotation(
    
    # --------------------------------------------------------
    # TITLE
    # --------------------------------------------------------
    
    title =
      "Where does landmark fame concentrate?",
    
    # --------------------------------------------------------
    # SUBTITLE
    # --------------------------------------------------------
    
    subtitle =
      paste0(
        
        "The world's most famous castles, fortresses, palaces and ruins — ",
        
        "and the countries that repeatedly rise to the top"
      ),
    
    # --------------------------------------------------------
    # CAPTION
    # --------------------------------------------------------
    
    caption =
      paste0(
        
        big_story,
        
        "\n\n",
        
        "● Top 5 landmarks   ·   ",
        "• Every landmark in the country/category",
        
        "\n",
        
        "Rank = position within category by fame rank",
        
        "   ·   ",
        
        "Data: TidyTuesday • Castlemap / Wikidata"
      ),
    
    # --------------------------------------------------------
    # GLOBAL THEME
    # --------------------------------------------------------
    
    theme =
      theme(
        
        plot.title =
          element_text(
            
            size = 24,
            
            face = "bold",
            
            colour = "#202020",
            
            hjust = 0,
            
            margin = margin(
              b = 5
            )
          ),
        
        plot.subtitle =
          element_text(
            
            size = 11,
            
            colour = "#666666",
            
            hjust = 0,
            
            margin = margin(
              b = 15
            )
          ),
        
        plot.caption =
          element_text(
            
            size = 8,
            
            colour = "#777777",
            
            hjust = 0,
            
            lineheight = 1.35,
            
            margin = margin(
              t = 12
            )
          ),
        
        plot.background =
          element_rect(
            
            fill = "#FCFBF7",
            
            colour = NA
          ),
        
        plot.margin =
          margin(
            t = 15,
            r = 18,
            b = 12,
            l = 18
          )
      )
  )


# ============================================================
# 20. DISPLAY
# ============================================================

final_plot