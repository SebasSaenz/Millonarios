library(tidyverse)
library(patchwork)
library(ggtext)

data <- read_tsv("data/100_jugadores.txt")

color <- c('#023858', '#0570b0', '#74a9cf', '#d0d1e6')


ranking_plot <- data %>%
  ggplot(aes(
    x = 1,
    y = fct_reorder(Nombre, Ranking)
  )) +
  geom_text(aes(label = Ranking), hjust = 0.5, size = 3) +
  scale_x_continuous(limits = c(0.8, 1.2), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = 0.6)) +
  coord_cartesian(clip = "off") +
  labs(title = "#") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "optima", face = "bold"),
    text = element_text(family = "optima", face = "bold")
  )

partidos <- data %>%
  ggplot(aes(
    x = Apariciones,
    y = fct_reorder(Nombre, Ranking),
    fill = Posicion
  )) +
  geom_col(key_glyph = draw_key_point) +
  scale_x_continuous(breaks = seq(0, 250, 50)) +
  scale_y_discrete(expand = expansion(add = 0.6)) +
  scale_fill_manual(values = color) +
  labs(x = NULL, y = NULL, title = "Partidos") +
  theme_minimal() +
  theme(
    text = element_text(family = "optima", colour = "black"),
    plot.title = element_text(hjust = 0.5, family = "optima", face = "bold"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(
      linetype = 2,
      linewidth = 0.3,
      colour = "grey"
    )
  )

goles <- data %>%
  ggplot(aes(x = Goles, y = fct_reorder(Nombre, Ranking), fill = Posicion)) +
  geom_col(key_glyph = draw_key_point) +
  scale_x_continuous(breaks = seq(0, 50, 10)) +
  scale_y_discrete(expand = expansion(add = 0.6)) +
  scale_fill_manual(values = color) +
  labs(x = NULL, y = NULL, title = "Goles") +
  theme_minimal() +
  theme(
    text = element_text(family = "optima"),
    plot.title = element_text(hjust = 0.5, family = "optima", face = "bold"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(
      linetype = 2,
      linewidth = 0.3,
      colour = "grey"
    )
  )

titulos <- data %>%
  ggplot(aes(
    x = 1,
    y = fct_reorder(Nombre, Ranking)
  )) +
  geom_text(
    aes(label = ifelse(Titulos == 0, "", strrep("★", Titulos))),
    color = "#FFD700",
    hjust = 0,
    size = 7,
    family = "Arial Unicode MS"
  ) +
  scale_x_continuous(limits = c(1, 2), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = 0.6)) +
  labs(x = NULL, y = NULL, title = "Títulos") +
  theme_minimal() +
  theme(
    text = element_text(family = "optima", face = "bold"),
    plot.title = element_text(hjust = 0.5, family = "optima", face = "bold"),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank()
  )

ranking_plot +
  partidos +
  goles +
  titulos +
  plot_layout(widths = c(0.25, 2, 2, 2), guides = "collect", axes = "collect") +
  plot_annotation(
    title = "Los 100 más importantes de la historia de Millonarios",
    caption = "<b>Data:</b> LosMillonarios.net, Transfermark, Wikipedia -<b>Twitter:</b> @SaenzJohanS - <b>GitHub:</b> SebasSaenz",
    theme = theme(
      plot.title = element_textbox_simple(
        size = 30,
        face = "bold",
        family = "optima",
        hjust = 0
      ),
      plot.caption = element_markdown(family = "optima")
    )
  ) &
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.ticks = element_blank()
  ) &
  guides(
    fill = guide_legend(
      override.aes = list(shape = 21, size = 5)
    )
  )

ggsave(filename = "plots/100_mejores.png", width = 8, height = 4, dpi = 300)
