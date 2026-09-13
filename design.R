library(grid)

# ---------------------------------------------------------------
# COLORS
# ---------------------------------------------------------------
bg_color       <- "#FBE9DC"
box_color      <- "#DAD0BE"
title_color    <- "#1A1A1A"
coffee_color   <- "#8A5A2B"
subtitle_color <- "#4D4D4D"
line_color     <- "#B0A99A"
footer_color   <- "#6B6B6B"

# ---------------------------------------------------------------
# HELPER: measure text width in npc for precise, non-overlapping
# placement of mixed-color runs on the same line
# ---------------------------------------------------------------
text_w <- function(label, gp) convertWidth(grobWidth(textGrob(label, gp = gp)), "npc", valueOnly = TRUE)

# ---------------------------------------------------------------
# HELPER: draw a simple coffee-bean icon (ellipse + curved crack).
# size is in "snpc" so it stays round regardless of device shape.
# ---------------------------------------------------------------
draw_bean <- function(cx, cy, size = 0.022, color = coffee_color, angle = 20) {
  pushViewport(viewport(x = unit(cx, "npc"), y = unit(cy, "npc"),
                        width  = unit(size, "snpc"),
                        height = unit(size * 1.5, "snpc"),
                        angle = angle))
  a <- seq(0, 2 * pi, length.out = 80)
  grid.polygon(x = 0.5 + 0.5 * cos(a), y = 0.5 + 0.5 * sin(a),
               gp = gpar(fill = color, col = NA))
  t <- seq(0, 1, length.out = 40)
  grid.lines(x = 0.5 + 0.09 * sin(t * pi), y = t,
             gp = gpar(col = bg_color, lwd = 2))
  upViewport()
}

# ---------------------------------------------------------------
# NEW PAGE  (use a tall/portrait device so the proportions below
# look right), e.g.:
#   png("infographic_design.png", width = 900, height = 1600, res = 150)
#   ... run the drawing code below ...
#   dev.off()
# ---------------------------------------------------------------
grid.newpage()
grid.rect(gp = gpar(fill = bg_color, col = NA))

# ---------------------------------------------------------------
# TITLE  ("Global Trade Flow" / "of Coffee Beans" + bean icon)
# ---------------------------------------------------------------
x0 <- 0.04
gp_title_black <- gpar(fontsize = 24, fontface = "bold", col = title_color)
gp_title_brown <- gpar(fontsize = 24, fontface = "bold", col = coffee_color)

# grid.text("Global Trade Flow", x = x0, y = 0.965, just = "left", gp = gp_title_black)
# 
# y_line2 <- 0.935
# grid.text("of ", x = x0, y = y_line2, just = "left", gp = gp_title_black)
# w_of <- text_w("of ", gp_title_black)
# 
# grid.text("Coffee", x = x0 + w_of, y = y_line2, just = "left", gp = gp_title_brown)
# w_coffee <- text_w("Coffee", gp_title_brown)
# 
# grid.text(" Beans", x = x0 + w_of + w_coffee, y = y_line2, just = "left", gp = gp_title_black)
# w_beans <- text_w(" Beans", gp_title_black)

draw_bean(x0 + w_of + w_coffee + w_beans + 0.014, y_line2 + 0.004, size = 0.014)

# ---------------------------------------------------------------
# SUBTITLE
# ---------------------------------------------------------------
# gp_sub <- gpar(fontsize = 12, col = subtitle_color)
# grid.text("Based on tonnes of non-roasted,", x = x0, y = 0.905, just = "left", gp = gp_sub)
# grid.text("non decaffeinated coffee traded in 2019.", x = x0, y = 0.892, just = "left", gp = gp_sub)

# ---------------------------------------------------------------
# TOP-RIGHT INFO BOX
# ---------------------------------------------------------------
box_xmin <- 0.58; box_xmax <- 0.965
box_ymin <- 0.865; box_ymax <- 0.975

grid.roundrect(x = mean(c(box_xmin, box_xmax)), y = mean(c(box_ymin, box_ymax)),
               width = box_xmax - box_xmin, height = box_ymax - box_ymin,
               r = unit(4, "mm"), gp = gpar(fill = box_color, col = NA))

gp_box <- gpar(fontsize = 11, col = title_color)
# box_lines <- c(
#   "84% of global coffee",
#   "exports come from just",
#   "10 countries, and 68% of",
#   "all coffee exports are",
#   "imported by the U.S., EU,",
#   "UK, and Canada."
# )
box_lines<-""
box_text_x <- box_xmin + 0.02
box_text_y0 <- box_ymax - 0.018
line_gap <- 0.0165
for (i in seq_along(box_lines)) {
  grid.text(box_lines[i], x = box_text_x, y = box_text_y0 - (i - 1) * line_gap,
            just = "left", gp = gp_box)
}
draw_bean(box_xmax - 0.025, box_ymin + 0.01, size = 0.011)

# ---------------------------------------------------------------
# DIVIDER UNDER HEADER
# ---------------------------------------------------------------
grid.lines(x = c(x0, 0.965), y = c(0.845, 0.845), gp = gpar(col = line_color, lwd = 1))

# ---------------------------------------------------------------
# "Export from" / "Import to" ROW
# ---------------------------------------------------------------
gp_axis <- gpar(fontsize = 12, col = title_color)
# grid.text("Export from", x = x0, y = 0.825, just = "left", gp = gp_axis)
# grid.text("Import to", x = 0.965, y = 0.825, just = "right", gp = gp_axis)

w_export <- text_w("Export from ", gp_axis)
w_import <- text_w(" Import to", gp_axis)
grid.lines(x = c(x0 + w_export, 0.5), y = c(0.825, 0.825), gp = gpar(col = line_color, lwd = 0.6))
grid.lines(x = c(0.5, 0.965 - w_import), y = c(0.825, 0.825), gp = gpar(col = line_color, lwd = 0.6))

# ---------------------------------------------------------------
# BLANK PLACEHOLDER FOR THE (ALLUVIAL) PLOT AREA
# ---------------------------------------------------------------
plot_ymin <- 0.075; plot_ymax <- 0.805
grid.rect(x = 0.5, y = mean(c(plot_ymin, plot_ymax)),
          width = 0.965 - x0, height = plot_ymax - plot_ymin,
          gp = gpar(fill = "white", col = "grey75", lty = "dashed", lwd = 1, alpha = 0.4))
grid.text("[ plot area \u2014 left blank ]", x = 0.5, y = mean(c(plot_ymin, plot_ymax)),
          gp = gpar(fontsize = 13, fontface = "italic", col = "grey55"))

# ---------------------------------------------------------------
# FOOTER
# ---------------------------------------------------------------
grid.lines(x = c(x0, 0.965), y = c(0.05, 0.05), gp = gpar(col = line_color, lwd = 1))

gp_footer <- gpar(fontsize = 11, col = footer_color)
grid.text("created by Hari Krishna", x = x0, y = 0.028, just = "left", gp = gp_footer)
w_credit <- text_w("created by Airi (Iris) Ryu ", gp_footer)
draw_bean(x0 + w_credit + 0.006, 0.033, size = 0.011)

grid.text("Source:Tidy-Tuesday Data",
          x = 0.965, y = 0.028, just = "right", gp = gp_footer)