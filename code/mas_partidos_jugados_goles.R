library(tidyverse)
library(ggtext)



df <- read_csv("data/mas_partidos.csv")

base_color <- c('#bdc9e1','#74a9cf','#0570b0')

title <- "Entre los jugadores con mas partidos en Millonarios,
Iguarán es<br /> el de mejor promedio goleador, 120 goles en 336 partidos."

caption <- "**Data: Wikipedia (16.02.2025) - @SaenzJohanS - GitHub: SebasSaenz**"


df %>% 
  mutate(ratio = Goles/Partidos,
         Posición = factor(Posición,
                           levels = c("Defensa", "Mediocampista", "Delantero"))) %>% 
  ggplot(aes(x = ratio,
         y = fct_reorder(Nombre, ratio),
         fill = Posición)) +
  geom_col() +
  geom_text(aes(x = ratio + 0.01,
                y = Nombre,
                label = paste0(Nombre, " (", Goles, "/", Partidos, ")")),
            size = 2.3,
            hjust =0,
            family = "Optima") +
  scale_x_continuous(limits = c(0, 0.6),
                     expand = c(0,0)) +
  scale_fill_manual(values = base_color) +
  labs(x = NULL,
       caption = caption,
       title = title) +
  theme_minimal() +
  theme(text = element_text(family = "Optima"),
        panel.background = element_rect(color = "white", fill = "white"),
        plot.background = element_rect(
          color = "white",
          fill = "white"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(color = "black", family = "Optima"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(linetype = 2, 
                                          color = "grey", 
                                          linewidth = 0.15),
        legend.position = c(0.78, 0.1),
        legend.text = element_text(size = 8, family = "Optima"),
        legend.title = element_blank(),
        legend.key.size = unit(1, "line"),
        plot.caption = element_markdown(size = 7, hjust = 0),
        plot.title = element_markdown(size = 10,family = "Optima",
                                      margin = margin(t=5,b=5)))

ggsave(filename = "plots/mas_partidos_gol.png", width = 4, height = 4, dpi = 300)
