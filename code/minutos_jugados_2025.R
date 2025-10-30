# Load library -----------------------------------------------------------------
library(tidyverse)
library(ggtext)


# Read file --------------------------------------------------------------------
df <- readxl::read_xlsx("data/minutos_new.xlsx")


# Wrangle data and create plot -------------------------------------------------
df %>%
  mutate(
    min_convo = minutos / convocado,
    median_min = round(mean(min_convo)),
    difference = min_convo - median_min,
    refuerzo = if_else(is.na(refuerzo), FALSE, refuerzo)
  ) %>%
  ggplot(aes(x = min_convo, y = fct_reorder(nombre, min_convo))) +
  geom_col(
    aes(fill = refuerzo),
    width = 0.8,
    #color = "black",
    #linewidth = 0.2
  ) +
  #geom_vline(xintercept = 0, color = "black") +
  geom_text(
    aes(label = paste0(nombre, " (", convocado, ")"), x = min_convo + 1),
    size = 2.8,
    family = "Optima",
    hjust = 0
  ) +
  scale_x_continuous(limits = c(0, 135), breaks = seq(0, 90, 15)) +
  scale_fill_manual(values = c('#d9d9d9', '#3182bd')) +
  labs(
    x = "Minutos x convocatoria",
    y = NULL,
    title = "Millonarios: refuerzos para la banca",
    subtitle = "La mayoria de los refuerzos del 2025 jugaron en promedio menos de<br> 45 minutos cada vez que fueron convocados para la liga<br> 2025-II, incluso algunos de ellos jugaron menos de<br> 30 minutos.",
    caption = "**Data: Transfermarkt (30.10.2025) - @SaenzJohanS - GitHub: SebasSaenz**<br>Los numbero en parentesis representa las convocatorias"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Optima"),
    panel.background = element_rect(color = "white", fill = "white"),
    plot.background = element_rect(
      color = "white",
      fill = "white"
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_markdown(colour = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(linetype = 2),
    legend.position = "none",
    plot.title = element_markdown(size = 17, face = "bold"),
    plot.subtitle = element_markdown(family = "Optima"),
    plot.caption = element_markdown(hjust = 0)
  )

# Save plot --------------------------------------------------------------------
ggsave(
  filename = "plots/minutos_jugados_2025.png",
  width = 4,
  height = 6,
  dpi = 300
)
