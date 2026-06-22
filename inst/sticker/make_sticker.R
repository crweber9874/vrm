library(ggplot2)
library(showtext)

font_add_google("Fira Sans", "firasans")
showtext_auto()

# --- Hex outline coordinates ---
hex_angles <- seq(30, 390, by = 60) * pi / 180
hex_x <- cos(hex_angles)
hex_y <- sin(hex_angles)

# --- "isualization" as vertical letters descending from V ---
viz_letters <- strsplit("isualization", "")[[1]]
n_letters <- length(viz_letters)
viz_y_start <- 0.22  # just below the V
viz_spacing <- 0.085
viz_x <- -0.30

# --- Build sticker as pure ggplot ---
p <- ggplot() +
  # Hex background
  annotate("polygon", x = hex_x, y = hex_y,
           fill = "#1B5E20", color = "#4CAF50", linewidth = 2) +

  # Large "V" - upper left
  annotate("text", x = -0.42, y = 0.45, label = "V",
           size = 36, fontface = "bold", color = "#90EE90",
           family = "firasans", hjust = 0.5) +

  # "arious" extending right from V
  annotate("text", x = -0.10, y = 0.45, label = "arious",
           size = 10, color = "#90EE90",
           family = "firasans", hjust = 0) +

  # "Regression" to the right of the vertical column
  annotate("text", x = 0.05, y = 0.10, label = "Regression",
           size = 7, color = "#C8E6C9",
           family = "firasans", hjust = 0) +

  # "Models" below Regression
  annotate("text", x = 0.05, y = -0.12, label = "Models",
           size = 7, color = "#C8E6C9",
           family = "firasans", hjust = 0) +

  coord_fixed(xlim = c(-1.15, 1.15), ylim = c(-1.15, 1.15)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(0, 0, 0, 0))

# Add vertical "isualization" letters one by one
for (i in seq_along(viz_letters)) {
  p <- p + annotate("text",
                     x = viz_x,
                     y = viz_y_start - (i - 1) * viz_spacing,
                     label = viz_letters[i],
                     size = 8, color = "#90EE90",
                     family = "firasans", hjust = 0.5)
}

ggsave("man/figures/vrm_hex.png", p, width = 3, height = 3.46,
       dpi = 300, bg = "transparent")

cat("Sticker saved to man/figures/vrm_hex.png\n")
