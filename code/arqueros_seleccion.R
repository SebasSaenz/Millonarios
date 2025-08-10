library(tidyverse)
library(ggtext)

df <- read_tsv("data/arqueros_colombia.txt")

base_color <- c('#f1eef6', '#bdc9e1', '#74a9cf', '#0570b0')

caption <- "**Data: TransferMarkt (30.05.2025) - @SaenzJohanS - GitHub: SebasSaenz**"


goles_partido <- df %>%
  group_by(jugador) %>%
  summarise(
    sum_minutos = sum(minutos),
    sum_goles = sum(goles_concedidos),
    .groups = "drop"
  ) %>%
  mutate(
    goles_en_90 = 90 * (sum_goles / sum_minutos),
    text_bar = paste0(sum_goles, " goles en ", sum_minutos, " minutos"),
    colour_bar = case_when(
      jugador == "Alvaro Montero" ~ "Millonarios",
      .default = "otro"
    ),
    jugador = str_replace(jugador, " ", "\n")
  )

goles_partido %>%
  ggplot(aes(
    x = fct_reorder(jugador, goles_en_90),
    y = goles_en_90,
    fill = colour_bar
  )) +
  geom_col(width = 0.5) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_text(
    aes(x = jugador, y = goles_en_90 + 0.2, label = jugador),
    family = "optima",
    fontface = "bold",
    lineheight = 0.8
  ) +
  geom_text(
    aes(x = jugador, y = goles_en_90 + 0.05, label = text_bar),
    colour = "grey30",
    size = 2,
    family = "optima"
  ) +
  geom_text(
    x = 1,
    y = 1.55,
    label = "Goles por partido",
    size = 3,
    family = "optima"
  ) +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(values = c('#0570b0', '#bdc9e1')) +
  labs(
    x = NULL,
    y = NULL,
    title = "Álvaro Montero el arquero de mejor rendimiento",
    subtitle = "Mientras que Montero ha recibido aproximadamente un gol cada dos partidos en el ultimo año, Vargas, el titular de la selección, ha recibido alrededor de tres goles cada dos partidos",
    caption = caption
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "optima"),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(
      linetype = 2,
      linewidth = 0.3,
      colour = "grey"
    ),
    panel.background = element_rect(colour = "white", fill = "white"),
    plot.background = element_rect(colour = "white", fill = "white"),
    axis.text.x = element_blank(),
    plot.title = element_markdown(face = "bold", size = 12),
    plot.subtitle = element_textbox_simple(size = 8),
    plot.caption = element_markdown(size = 8)
  )

ggsave(
  filename = "plots/arqueros_selecion.png",
  width = 4,
  height = 4,
  dpi = 350
)
