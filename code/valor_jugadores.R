library(tidyverse)
library(ggtext)
library(ggrepel)

df <- read_tsv("data/contrataciones_2025-2.txt")

df_clean <- df %>%
  pivot_longer(-jugador, names_to = "year", values_to = "valor") %>%
  group_by(jugador) %>%
  mutate(diff = valor[2] - valor[1]) %>%
  mutate(
    color = case_when(
      diff < 0 ~ "Perdio valor",
      diff > 0 ~ "Gano Valor",
      diff == 0 ~ "No cambio"
    )
  )

df_left <-
  df_clean %>%
  filter(year == 2024, diff < 0)

df_right <-
  df_clean %>%
  filter(year == 2025, diff >= 0)

df_clean %>%
  ggplot(aes(x = year, y = valor, group = jugador, colour = color)) +
  geom_line() +
  geom_point() +
  geom_text_repel(
    data = df_left,
    aes(label = jugador),
    direction = "y",
    nudge_x = -0.1,
    hjust = 1,
    size = 2.5,
    segment.size = 0.1
  ) +
  geom_text_repel(
    data = df_right,
    aes(label = jugador),
    direction = "y",
    nudge_x = 0.1,
    hjust = 0,
    size = 2.5,
    segment.size = 0.1
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  annotate(
    geom = "text",
    x = 0.57,
    y = 825,
    label = "mil €",
    size = 3,
    family = "Optima"
  ) +
  scale_y_continuous(limits = c(0, 825)) +
  scale_color_manual(values = c("blue", "grey60", "red")) +
  coord_cartesian() +
  labs(
    x = NULL,
    y = NULL,
    title = "¿Refuerzos o contrataciones?",
    subtitle = "De los jugadores que se unieron a Millonarios para la temporada<br> 2025-II, solo uno aumentó su valor en el último año, mientras<br> que el resto lo mantuvo o lo perdió.",
    caption = "**Data: Transfermarkt (10.08.2025) - @SaenzJohanS - GitHub: SebasSaenz**<br>*Los valores mas cercanos a 06.2024 y 06.2025 fueron usados"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    text = element_text(family = "optima"),
    plot.title = element_markdown(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_markdown(size = 9, hjust = 0, halign = 0),
    plot.caption = element_markdown(size = 7, hjust = 0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linetype = 2),
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_text(
      vjust = -0.3,
      margin = margin(l = 20, r = -10),
      color = "black"
    )
  )

ggsave("plots/contrataciones_2025.png", width = 4, height = 4, dpi = 350)
