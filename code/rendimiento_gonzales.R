library(tidyverse)
library(geofacet)
library(ggh4x)
library(ggtext)

df <- read_tsv("data/rendimiento_gonzales.txt")

title <- c("El Millonarios de González: rendimiento en la media de<br> los últimos cinco años")

subtitle <- "Con 27 puntos en 15 fechas, el Millonarios de González alcanza un 60 % de rendimiento"

caption <- "**Data: Wikipedia (19.04.2025) - @SaenzJohanS - GitHub: SebasSaenz**"


pivot_longer <- df %>% 
  pivot_longer(-fecha,
               names_to = "torneo",
               values_to = "puntos") %>% 
  filter(fecha <= 15) %>% 
  mutate(gozales = "2025_1" == torneo)


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


ggsave(filename = "plots/rendimiento_gonzales.png",
       width = 5,
       height = 4,
       dpi = 300)


pivot_longer %>% 
  ggplot(aes(x = fecha)) +
  # One line for Cat rescues
  geom_line(aes(y = `2024_1`), color = "blue") +
  # Another line for Not_Cat rescues
  geom_line(aes(y = `2025_1`), color = "purple") +
  # stat_difference() from ggh4x package applies the conditional fill
  # based on which of Not_Cat and Cat is larger.
  stat_difference(aes(ymin = `2024_1`, ymax = `2025_1`), alpha = 0.3) +
  theme_minimal()



animal_rescues <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv"
) %>% 
  # Capitalize the type of animal
  mutate(animal_group_parent = str_to_sentence(animal_group_parent))

borough_names <- gb_london_boroughs_grid %>% 
  select(borough_code = code_ons, name)

rescues_borough <- animal_rescues %>% 
  # Keep rescues that happeend before 2021
  filter(cal_year < 2021) %>% 
  # We're interested on whether it is a Cat or another type of animal.
  mutate(animal_group_parent = if_else(animal_group_parent == "Cat", "Cat", "Not_Cat")) %>% 
  # Count the number of rescues per year, borough, and type of animal
  count(cal_year, borough_code, animal_group_parent) %>% 
  # Make the dataset wider.
  # * One column for the number of cat rescues
  # * Another column for the number of other animal rescues
  pivot_wider(names_from = animal_group_parent, values_from = n) %>% 
  # Merge the data with the info about the grid layout
  left_join(borough_names) %>% 
  # Drop entries with missing name
  filter(!is.na(name)) 
