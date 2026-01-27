library(tidyverse)
library(ggforce)
library(ggtext)

L <- 105
W <- 68

x_min <- L * 2 / 3
goal_depth <- 2.44

pen_area_depth <- 16.5
pen_area_width <- 40.32
six_depth <- 5.5
six_width <- 18.32
goal_width <- 7.32

pen_spot_dist <- 11
pen_arc_r <- 9.15
corner_r <- 1

y_mid <- W / 2

x_goal <- L
x_pa <- L - pen_area_depth
x_6 <- L - six_depth
x_pen <- L - pen_spot_dist

y_pa_min <- y_mid - pen_area_width / 2
y_pa_max <- y_mid + pen_area_width / 2
y_6_min <- y_mid - six_width / 2
y_6_max <- y_mid + six_width / 2
y_g_min <- y_mid - goal_width / 2
y_g_max <- y_mid + goal_width / 2

# penalty "D" angles (outside the box)
cval <- (x_pa - x_pen) / pen_arc_r
theta <- acos(cval)

goles <- tibble(
  x = c(35, 43, 36, 34, 32, 34.5, 38, 39, 35, 44, 29),
  y = c(93, 88, 95, 94, 105, 94, 98, 94.5, 96, 99, 82),
  recurso = c(
    "Izquierda",
    "Derecha",
    "Cabeza",
    "Penalti",
    "Derecha",
    "Penalti",
    "Cabeza",
    "Cabeza",
    "Cabeza",
    "Derecha",
    "Derecha"
  )
)

gol_direccion <- tibble(
  y = c(82.5, 93, 88, 95, 94, 94, 98, 94.5, 96, 99),
  yend = c(107, 107, 105, 106, 105, 105, 105, 105, 105, 105),
  x = c(29, 35, 43, 36, 35, 34, 38, 39, 35, 44),
  xend = c(37, 33, 31, 37, 35.5, 37, 31, 31.5, 32.5, 33.5),
  recurso = c(
    "Derecha",
    "Izquierda",
    "Derecha",
    "Cabeza",
    "Penalti",
    "Penalti",
    "Cabeza",
    "Cabeza",
    "Cabeza",
    "Derecha"
  )
)

base_color <- c('#0571b0', '#f4a582', '#92c5de', '#ca0020')

p <- ggplot() +

  # attacking third boundary (SWAPPED x/y)
  geom_rect(
    aes(xmin = 0, xmax = W, ymin = x_min, ymax = x_goal),
    fill = NA,
    colour = "black",
    linewidth = 0.7
  ) +

  # penalty area
  geom_rect(
    aes(xmin = y_pa_min, xmax = y_pa_max, ymin = x_pa, ymax = x_goal),
    fill = NA,
    colour = "black",
    linewidth = 0.7
  ) +

  # 6-yard box
  geom_rect(
    aes(xmin = y_6_min, xmax = y_6_max, ymin = x_6, ymax = x_goal),
    fill = NA,
    colour = "black",
    linewidth = 0.7
  ) +

  # goal (behind goal line)
  geom_rect(
    aes(
      xmin = y_g_min,
      xmax = y_g_max,
      ymin = x_goal,
      ymax = x_goal + goal_depth
    ),
    fill = NA,
    colour = "black",
    linewidth = 0.7
  ) +

  # penalty spot
  geom_point(aes(x = y_mid, y = x_pen), size = 2, colour = "black") +

  # penalty arc ("D")
  ggforce::geom_arc(
    aes(
      x0 = y_mid,
      y0 = x_pen,
      r = pen_arc_r,
      start = theta,
      end = 2 * pi - theta
    ),
    colour = "black",
    linewidth = 0.7
  ) +
  coord_fixed() +
  geom_point(
    aes(x = x, y = y, fill = recurso, ),
    shape = 21,
    size = 3,
    stroke = 0.5,
    data = goles
  ) +
  geom_segment(
    aes(y = y, yend = yend, x = x, xend = xend, color = recurso),
    linewidth = 0.4,
    linetype = 2,
    data = gol_direccion,
    show.legend = FALSE
  ) +
  scale_color_manual(values = base_color) +
  scale_fill_manual(values = base_color) +
  labs(
    title = "Los 11 goles de Falcao con Millonarios",
    caption = "*La posición del balón antes del remate fue calculada visualmente usando video. **@SaenzJohanS - GitHub: SebasSaenz**"
  ) +
  theme_void() +
  theme(
    plot.margin = margin(t = 0, r = 5, b = 0, l = 5),
    text = element_text(family = "optima"),
    plot.title = element_text(
      family = "optima",
      face = "bold",
      size = 20,
      margin = margin(t = 0, b = 7)
    ),
    plot.caption = element_markdown(size = 8, hjust = 0),
    legend.title = element_blank(),
    legend.position = c(0.80, 0.35),
    legend.justification = c("left", "center"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(2, "pt"),
    panel.background = element_rect(colour = "white", fill = "white"),
    plot.background = element_rect(color = "white", fill = "white")
  )


p

ggsave(filename = "plots/falcao_goles.png", width = 6, height = 4, dpi = 300)
