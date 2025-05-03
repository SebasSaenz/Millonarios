# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(ggtext)

base_color <- c('#f1eef6', '#bdc9e1', '#74a9cf', '#0570b0')

caption <- "**Data: TransferMarkt (03.04.2025) - @SaenzJohanS - GitHub: SebasSaenz**"

# Load data -------------------------------------------------------------------
df <- read.delim(
  "data/baby_millos.tsv",
  sep = "\t",
  header = TRUE,
  encoding = "UTF-8"
)

total_partidos <- df %>%
  select(jugador, total_partidos)

mean(total_partidos$total_partidos)
mean(df$edad)

df %>%
  mutate(partidos_no_millos = total_partidos - partidos_millos) %>%
  select(-total_partidos) %>%
  pivot_longer(
    cols = -c(jugador, posicion, edad),
    names_to = "tipo_partidos",
    values_to = "numero_partidos"
  ) %>%
  inner_join(total_partidos, by = "jugador") %>%
  ggplot(aes(
    x = numero_partidos,
    y = fct_reorder(jugador, total_partidos),
    fill = tipo_partidos
  )) +
  geom_col() +
  geom_text(
    aes(
      x = total_partidos + 5,
      y = jugador,
      label = paste0(jugador, " (", edad, " años)")
    ),
    hjust = 0,
    family = "Optima",
    size = 3.3
  ) +
  scale_x_continuous(limits = c(0, 900), breaks = seq(0, 600, 150)) +
  scale_fill_manual(
    values = c('#0570b0', '#bdc9e1'),
    label = c("Partidos con Millonarios", "Partidos con otros equipos")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "¿Baby Millos?",
    subtitle = 'Con 26 años y más de 200 partidos en promedio, el "Baby Millos"<br>desborda experiencia',
    caption = caption
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    legend.position = c(0.7, 0.1),
    legend.title = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(
      linetype = 2,
      linewidth = 0.2,
      colour = "grey"
    ),
    plot.title = element_markdown(size = 20, family = "Optima", face = "bold"),
    plot.subtitle = element_markdown(family = "Optima"),
    panel.background = element_rect(colour = "white", fill = "white"),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.caption = element_markdown(size = 7, hjust = 0)
  )

ggsave("plots/baby_millos.png", dpi = 300, height = 6, width = 4.6)
