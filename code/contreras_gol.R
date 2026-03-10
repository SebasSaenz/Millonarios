library(ggplot2)
library(dplyr)
library(tibble)
library(grid)
library(scales)
library(ggtext)

# -----------------------------
# Pitch dimensions
# -----------------------------
pitch_length <- 105
pitch_width <- 68
goal_center_y <- pitch_width / 2
goal_x <- pitch_length

# Main shot
shot_distance <- 58.5
shot_x <- goal_x - shot_distance
shot_y <- goal_center_y

# Comparison distances
penalty_dist <- 11
transmilenio_dist <- 27
primada <- 40
bolivar <- 55

# -----------------------------
# Helpers
# -----------------------------
circle_df <- function(
  center = c(0, 0),
  r = 1,
  n = 300,
  start = 0,
  end = 2 * pi
) {
  tibble(
    x = center[1] + r * cos(seq(start, end, length.out = n)),
    y = center[2] + r * sin(seq(start, end, length.out = n))
  )
}

centre_circle <- circle_df(
  center = c(pitch_length / 2, pitch_width / 2),
  r = 9.15
)

left_pen_arc <- circle_df(
  center = c(11, pitch_width / 2),
  r = 9.15,
  start = -pi / 3,
  end = pi / 3
) %>%
  filter(x >= 16.5)

right_pen_arc <- circle_df(
  center = c(pitch_length - 11, pitch_width / 2),
  r = 9.15,
  start = 2 * pi / 3,
  end = 4 * pi / 3
) %>%
  filter(x <= pitch_length - 16.5)

# -----------------------------
# Comparison arrows INSIDE pitch
# All arrows END at the goal
# -----------------------------
comparisons <- tibble(
  label = c(
    "Tiro penalti",
    "Biarticulado",
    "Torre Catedral Primada",
    "1/2 Plaza de Bolívar"
  ),
  distance = c(
    penalty_dist,
    transmilenio_dist,
    primada,
    bolivar
  ),
  y = c(24, 18, 12, 6),
  color = c("#5dade2", "#f39c12", "#af7ac5", "#f03b20")
) %>%
  mutate(
    x_end = goal_x,
    x_start = goal_x - distance,
    x_mid = (x_start + x_end) / 2,
    label_text = paste0(label, " • ", distance, " m")
  )

# -----------------------------
# Plot
# -----------------------------
p <- ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(
      colour = "black",
      family = "optima",
      face = "bold",
      size = 28,
      hjust = 0.5,
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      colour = "#d9e2ec",
      size = 13,
      hjust = 0.5,
      margin = margin(b = 14)
    ),
    plot.caption = element_markdown(
      colour = "black",
      size = 10,
      hjust = 0.5,
      margin = margin(t = 14)
    )
  ) +

  # Pitch
  annotate(
    "rect",
    xmin = 0,
    xmax = pitch_length,
    ymin = 0,
    ymax = pitch_width,
    fill = "#145a32",
    colour = NA
  ) +

  # Pitch stripes
  annotate(
    "rect",
    xmin = 0,
    xmax = 105 / 6,
    ymin = 0,
    ymax = pitch_width,
    fill = alpha("white", 0.03),
    colour = NA
  ) +
  annotate(
    "rect",
    xmin = 2 * 105 / 6,
    xmax = 3 * 105 / 6,
    ymin = 0,
    ymax = pitch_width,
    fill = alpha("white", 0.03),
    colour = NA
  ) +
  annotate(
    "rect",
    xmin = 4 * 105 / 6,
    xmax = 5 * 105 / 6,
    ymin = 0,
    ymax = pitch_width,
    fill = alpha("white", 0.03),
    colour = NA
  ) +

  # Outer lines
  annotate(
    "rect",
    xmin = 0,
    xmax = pitch_length,
    ymin = 0,
    ymax = pitch_width,
    fill = NA,
    colour = "white",
    linewidth = 1.2
  ) +

  # Halfway line
  annotate(
    "segment",
    x = pitch_length / 2,
    xend = pitch_length / 2,
    y = 0,
    yend = pitch_width,
    colour = "white",
    linewidth = 1
  ) +

  # Centre circle + spot
  geom_path(
    data = centre_circle,
    aes(x, y),
    colour = "white",
    linewidth = 1
  ) +
  annotate(
    "point",
    x = pitch_length / 2,
    y = pitch_width / 2,
    colour = "white",
    size = 1.5
  ) +

  # Left penalty area
  annotate(
    "rect",
    xmin = 0,
    xmax = 16.5,
    ymin = (pitch_width / 2) - 20.16,
    ymax = (pitch_width / 2) + 20.16,
    fill = NA,
    colour = "white",
    linewidth = 1
  ) +
  annotate(
    "rect",
    xmin = 0,
    xmax = 5.5,
    ymin = (pitch_width / 2) - 9.16,
    ymax = (pitch_width / 2) + 9.16,
    fill = NA,
    colour = "white",
    linewidth = 1
  ) +
  annotate(
    "point",
    x = 11,
    y = pitch_width / 2,
    colour = "white",
    size = 1.5
  ) +
  geom_path(
    data = left_pen_arc,
    aes(x, y),
    colour = "white",
    linewidth = 1
  ) +

  # Right penalty area
  annotate(
    "rect",
    xmin = pitch_length - 16.5,
    xmax = pitch_length,
    ymin = (pitch_width / 2) - 20.16,
    ymax = (pitch_width / 2) + 20.16,
    fill = NA,
    colour = "white",
    linewidth = 1
  ) +
  annotate(
    "rect",
    xmin = pitch_length - 5.5,
    xmax = pitch_length,
    ymin = (pitch_width / 2) - 9.16,
    ymax = (pitch_width / 2) + 9.16,
    fill = NA,
    colour = "white",
    linewidth = 1
  ) +
  annotate(
    "point",
    x = pitch_length - 11,
    y = pitch_width / 2,
    colour = "white",
    size = 1.5
  ) +
  geom_path(
    data = right_pen_arc,
    aes(x, y),
    colour = "white",
    linewidth = 1
  ) +

  # Goals
  annotate(
    "rect",
    xmin = -1.5,
    xmax = 0,
    ymin = goal_center_y - 3.66,
    ymax = goal_center_y + 3.66,
    fill = NA,
    colour = "white",
    linewidth = 1
  ) +
  annotate(
    "rect",
    xmin = pitch_length,
    xmax = pitch_length + 1.5,
    ymin = goal_center_y - 3.66,
    ymax = goal_center_y + 3.66,
    fill = NA,
    colour = "white",
    linewidth = 1
  ) +

  # Main shot arrow
  annotate(
    "curve",
    x = shot_x,
    y = shot_y,
    xend = goal_x - 0.7,
    yend = goal_center_y,
    curvature = 0.08,
    arrow = arrow(length = unit(0.35, "cm"), type = "closed"),
    colour = "blue",
    linewidth = 1.8
  ) +

  # Main shot point
  annotate(
    "point",
    x = shot_x,
    y = shot_y,
    colour = alpha("blue", 0.25),
    size = 9
  ) +
  annotate(
    "point",
    x = shot_x,
    y = shot_y,
    colour = "blue",
    size = 3.2
  ) +

  # Goal point
  annotate(
    "point",
    x = goal_x,
    y = goal_center_y,
    colour = alpha("#ff5a5f", 0.22),
    size = 8
  ) +
  annotate(
    "point",
    x = goal_x,
    y = goal_center_y,
    colour = "#ff5a5f",
    size = 2.8
  ) +

  # Main shot label
  annotate(
    "label",
    x = (shot_x + goal_x) / 2,
    y = goal_center_y + 7,
    label = "Rodrigo Contreras 58.5 m",
    fill = "blue",
    colour = "white",
    fontface = "bold",
    size = 4,
    label.size = 0,
    label.r = unit(0.18, "lines")
  ) +

  # Comparison arrows
  geom_segment(
    data = comparisons,
    aes(x = x_start, xend = x_end, y = y, yend = y, color = color),
    linewidth = 1.5,
    arrow = arrow(ends = "both", type = "closed", length = unit(0.16, "cm")),
    inherit.aes = FALSE
  ) +

  # Comparison ticks
  # geom_segment(
  #   data = comparisons,
  #   aes(
  #     x = x_start,
  #     xend = x_start,
  #     y = y - 1.1,
  #     yend = y + 1.1,
  #     color = color
  #   ),
  #   linewidth = 1,
  #   inherit.aes = FALSE
  # ) +
  # geom_segment(
  #   data = comparisons,
  #   aes(x = x_end, xend = x_end, y = y - 1.1, yend = y + 1.1, color = color),
  #   linewidth = 1,
  #   inherit.aes = FALSE
  # ) +

  # Comparison labels
  geom_label(
    data = comparisons,
    aes(x = x_mid, y = y + 2.5, label = label_text, fill = color),
    color = "white",
    fontface = "bold",
    size = 2.5,
    label.size = 0,
    label.r = unit(0.15, "lines"),
    inherit.aes = FALSE
  ) +

  scale_color_identity() +
  scale_fill_identity() +

  # coord_fixed(
  #   xlim = c(-2, pitch_length + 2),
  #   ylim = c(-2, pitch_width + 2),
  #   expand = FALSE,
  #   clip = "off"
  # ) +

  labs(
    title = "El Puskás Contreras",
    caption = "<b>Code:</b> @SaenzJohanS - GitHub: SebasSaenz"
  )

p

ggsave(filename = "plots/gol_contreras.png", width = 7, height = 5, dpi = 300)
