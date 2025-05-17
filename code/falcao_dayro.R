library(tidyverse)
library(gganimate)
library(ggtext)

df <- read_tsv("data/falcao_vs_dayro.txt")

base_color <- c('#bdc9e1', '#0570b0')

caption <- "**Data: Wikipedia (17.05.2025) - @SaenzJohanS - GitHub: SebasSaenz**"


plot_df <- df %>%
  mutate(
    # Extract first 4-digit number from the season string
    Year = str_extract(Temporada, "\\d{4}") %>% as.numeric(),
    #goles_total = Goles_club+Goles_selecion)
    total_goles = Goles_club + Goles_selecion
  ) %>%
  select(Year, total_goles, Jugador) %>%
  arrange(Jugador, Year) %>%
  group_by(Jugador, Year) %>%
  summarise(sum_gols = sum(total_goles), .groups = "drop") %>%
  group_by(Jugador) %>%
  mutate(
    CumulativeGoals = cumsum(sum_gols),
    image = case_when(
      Jugador == "Falcao" ~ normalizePath("images/falcao.png"),
      Jugador == "Dayro" ~ normalizePath("images/dayro.png")
    )
  ) %>%
  ungroup()

animations <- plot_df %>%
  ggplot(aes(y = Jugador, x = CumulativeGoals, fill = Jugador)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(
    aes(x = CumulativeGoals + 2, label = as.character(CumulativeGoals)),
    hjust = "left"
  ) +
  scale_y_discrete(
    labels = \(x) glue::glue("<img src='images/{x}.png' height=70 />")
  ) +
  transition_states(Year, wrap = FALSE) +
  scale_x_continuous(limits = c(0, 400), breaks = seq(0, 400, 50)) +
  scale_fill_manual(values = base_color) +
  labs(
    y = NULL,
    x = "Goles acumulados",
    title = 'Duelo de titanes - {closest_state}',
    subtitle = "Con el regreso de Falcao a las canchas, el duelo por ser el<br> maximo goleador colombiano se definira en los cuadrangualres",
    caption = caption
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Optima"),
    axis.text.y = element_markdown(),
    plot.title = element_text(size = 25, family = "Optima", face = "bold"),
    plot.subtitle = element_markdown(family = "Optima"),
    plot.caption = element_markdown(family = "Optima"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(
      linetype = 2,
      linewidth = 0.3,
      color = "grey"
    )
  )

animate(
  animations,
  height = 400,
  width = 550,
  duration = 15,
  res = 100,
  end_pause = 10
)

anim_save("plots/falao_dayro.gif")
