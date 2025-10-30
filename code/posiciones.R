library(tidyverse)
library(janitor)
library(rvest)
library(ggtext)


url <- "https://es.wikipedia.org/wiki/Anexo:Trayectoria_de_Millonarios_Fútbol_Club"


table <- read_html(url) %>%
  html_table(fill = TRUE) %>%
  purrr::pluck(1) %>%
  clean_names()


numbers <- paste0(1:17, "°")

table %>%
  filter(
    !temporada == "Torneos Cortos",
    !temporada_2 == "2025-II",
    !temporada == "1989"
  ) %>%
  count(posicion) %>%
  mutate(
    posicion = factor(posicion, levels = paste0(17:1, "\u00BA"), ordered = TRUE)
  ) %>%
  ggplot(aes(x = n, y = posicion)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1, 19, 3))


table_clean <- table %>%
  filter(
    !temporada == "Torneos Cortos",
    !temporada_2 == "2025-II",
    !temporada == "1989"
  ) %>%
  mutate(
    posicion = str_remove(posicion, "\u00BA"),
    posicion = as.integer(posicion),
    labels = case_when(
      posicion == 1 ~ "1°",
      posicion > 1 & posicion <= 4 ~ "2° - 4°",
      posicion >= 5 & posicion <= 7 ~ "5° - 7°",
      posicion >= 8 & posicion <= 10 ~ "8° - 10°",
      posicion >= 11 & posicion <= 13 ~ "11° - 13°",
      posicion > 13 ~ "14° - 17°"
    )
  ) %>%
  mutate(
    labels = factor(
      labels,
      levels = c(
        "1°",
        "2° - 4°",
        "5° - 7°",
        "8° - 10°",
        "11° - 13°",
        "14° - 17°"
      )
    )
  )

first_season <- first(table_clean$temporada_2)
last_season <- last(table_clean$temporada_2)
cortos <- "2001"

table_clean %>%
  ggplot(aes(x = factor(temporada_2), y = 1, fill = labels)) +
  geom_tile(width = 0.95, height = 1, color = "white") +
  scale_fill_manual(
    values = rev(c(
      '#b2182b',
      '#ef8a62',
      '#fddbc7',
      '#d1e5f0',
      '#67a9cf',
      '#2166ac'
    ))
  ) +
  scale_x_discrete(
    breaks = c(first_season, cortos, last_season),
    labels = c(first_season, cortos, last_season)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Histórico de posiciones de Millonarios (1948–2025)",
    subtitle = "Millonarios ha terminado por debajo del puesto 13 en ocho ocasiones; el torneo 2025-II<br> podría ampliar ese antirrécord",
    caption = "**Data: Wikipedia (22.10.2025) - @SaenzJohanS - GitHub: SebasSaenz**"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "optima"),
    plot.title = element_markdown(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_markdown(size = 9, hjust = 0, halign = 0),
    plot.caption = element_markdown(size = 7, hjust = 0),
    legend.title = element_blank(),
    legend.key.size = unit(3, "mm"), # smaller keys
    legend.text = element_text(size = 8),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 6, vjust = 4),
    panel.grid = element_blank(),
    panel.background = element_rect(colour = "white", fill = "white"),
    plot.background = element_rect(colour = "white", fill = "white")
  )

ggsave(filename = "plots/posiciones.png", width = 6, height = 2, dpi = 300)
