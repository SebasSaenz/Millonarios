# Load library -----------------------------------------------------------------
library(tidyverse)
library(geofacet)
library(ggh4x)
library(ggtext)

# Load data adn set titles -----------------------------------------------------

df <- read_tsv("data/rendimiento_gonzales.txt")

title <- c(
  "El Millonarios de González: rendimiento en el top de<br> los últimos cinco años"
)

subtitle <- "Con 38 puntos el Millonarios de González alcanzo un 63 % de rendimiento, el segundo mejor<br>de los ultimos cinco años"

caption <- "**Data: Wikipedia (26.05.2025) - @SaenzJohanS - GitHub: SebasSaenz**"

# Wrangle data -----------------------------------------------------------------

pivot_longer <- df %>%
  pivot_longer(-fecha, names_to = "torneo", values_to = "puntos") %>%
  filter(fecha <= 20) %>%
  mutate(gozales = "2025_1" == torneo)

# Make plot --------------------------------------------------------------------
pivot_longer %>%
  ggplot(aes(x = fecha, y = puntos, group = torneo, color = gozales)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  annotate(
    geom = "text",
    x = 3.5,
    y = 41.3,
    label = "Puntos acumulados",
    size = 3,
    family = "Optima"
  ) +
  annotate(
    geom = "text",
    x = 23,
    y = 1.3,
    label = "Jornada",
    size = 3,
    family = "Optima"
  ) +
  annotate(
    geom = "text",
    x = 21.5,
    y = 37,
    label = "González",
    size = 3,
    color = "blue",
    family = "Optima"
  ) +
  annotate(
    geom = "text",
    x = 22.8,
    y = 39,
    label = "Campeón 2023-1",
    size = 3,
    color = "grey",
    family = "Optima"
  ) +
  annotate(
    geom = "text",
    x = 21.5,
    y = 30,
    label = "2020-1",
    size = 3,
    color = "grey",
    family = "Optima"
  ) +
  annotate(
    geom = "text",
    x = 21.5,
    y = 42,
    label = "2022-1",
    size = 3,
    color = "grey",
    family = "Optima"
  ) +
  scale_x_continuous(limits = c(0, 24), breaks = c(1, 5, 10, 15, 20)) +
  scale_y_continuous(limits = c(0, 42), breaks = seq(0, 42, 5)) +
  scale_color_manual(
    name = NULL,
    breaks = c(T, F),
    values = c("blue", "grey82")
  ) +
  coord_cartesian() +
  labs(
    x = NULL,
    y = NULL,
    title = title,
    caption = caption,
    subtitle = subtitle
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = 1, linewidth = 0.4),
    legend.position = "none",
    axis.text.y = element_text(vjust = -0.5, margin = margin(l = 20, r = -10)),
    axis.text.x = element_text(size = 8, vjust = 6),
    axis.title.y = element_blank(),
    plot.background = element_rect(color = "white", fill = "white"),
    text = element_text(family = "Optima"),
    plot.title = element_markdown(family = "Optima", face = "bold"),
    plot.subtitle = element_markdown(size = 8),
    plot.caption = element_markdown(size = 7, hjust = 0)
  )

# save plot --------------------------------------------------------------------
ggsave(
  filename = "plots/rendimiento_gonzales.png",
  width = 5,
  height = 4,
  dpi = 300
)
