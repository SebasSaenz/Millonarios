# Load library -----------------------------------------------------------------
library(tidyverse)
library(geofacet)
library(ggh4x)
library(ggtext)

# Load data adn set titles -----------------------------------------------------

df <- read_tsv("data/rendimiento_gonzales.txt")

title <- c("El Millonarios de González: rendimiento en la media de<br> los últimos cinco años")

subtitle <- "Con 27 puntos en 15 fechas, el Millonarios de González alcanza un 60 % de rendimiento"

caption <- "**Data: Wikipedia (19.04.2025) - @SaenzJohanS - GitHub: SebasSaenz**"

# Wrangle data -----------------------------------------------------------------

pivot_longer <- df %>% 
  pivot_longer(-fecha,
               names_to = "torneo",
               values_to = "puntos") %>% 
  filter(fecha <= 15) %>% 
  mutate(gozales = "2025_1" == torneo)

# Make plot --------------------------------------------------------------------
pivot_longer %>% 
  ggplot(aes(x=fecha, y=puntos, group=torneo, color=gozales)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept=0, 
             color = "black", linewidth=0.3) +
  annotate(geom = "text", x = 3, y = 36, label = "Puntos acumulados",
           size = 3, family = "Optima") +
  annotate(geom = "text", x = 20, y = 1, label = "Jornada",
           size = 3, family = "Optima") +
  annotate(geom = "text", x = 16.5, y = 27, label = "Gonzáles",
           size = 3, color = "blue", family = "Optima") +
  annotate(geom = "text", x = 17.5, y = 29, label = "Campeón 2023-1",
           size = 3, color = "grey", family = "Optima") +
  annotate(geom = "text", x = 16.2, y = 17, label = "2020-1",
           size = 3, color = "grey", family = "Optima") +
  annotate(geom = "text", x = 17.3, y = 32, label = "2021-2 & 2022-1",
           size = 3, color = "grey", family = "Optima") +
  scale_x_continuous(limits = c(1,20),
                     breaks = seq(1,20,5)) +
  scale_y_continuous(limits = c(0, 36),
                     breaks = seq(0, 36, 5)) +
  scale_color_manual(name =NULL,
                     breaks = c(T, F),
                     values = c("blue", "grey82")) +
  labs(x = NULL,
       y = NULL,
       title = title,
       caption = caption,
       subtitle = subtitle) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = 1, linewidth = 0.4),
        legend.position = "none",
        axis.text.y = element_text(vjust=-0.5
                                   ,margin = margin(l = 20, r = -10)),
        axis.text.x = element_text(size = 8,
                                   vjust = 6),
        axis.title.y = element_blank(),
        plot.background = element_rect(color = "white", fill = "white"),
        text = element_text(family = "Optima"),
        plot.title = element_markdown(family = "Optima", face = "bold"),
        plot.subtitle = element_markdown(size = 8),
        plot.caption = element_markdown(size = 7, hjust = 0))

# save plot --------------------------------------------------------------------
ggsave(filename = "plots/rendimiento_gonzales.png",
       width = 5,
       height = 4,
       dpi = 300)
