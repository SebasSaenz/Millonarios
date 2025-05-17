library(tidyverse)
library(patchwork)


base_color <- c('#f1eef6', '#bdc9e1', '#74a9cf', '#0570b0')


df <- read_tsv("data/goal_time_2025.txt")

mean_gol <- df %>%
  filter(tiempo == 1) %>%
  group_by(gol, tiempo) %>%
  summarise(
    mean_time = median(minuto),
    q25 = quantile(minuto, 0.25),
    q75 = quantile(minuto, 0.75)
  )

primero <- df %>%
  filter(tiempo == 1) %>%
  ggplot(aes(x = minuto, y = fecha, fill = gol)) +
  geom_vline(
    data = mean_gol,
    aes(xintercept = mean_time, colour = gol),
    linetype = 2
  ) +
  geom_point(shape = 21, size = 5, color = "black") +
  scale_y_continuous(limits = c(1, 17), breaks = seq(1, 17, 1)) +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 5)) +
  scale_fill_manual(values = c('#0570b0', '#bdc9e1')) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~tiempo, scales = "free_x") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(
      linetype = 2,
      colour = "grey",
      size = 0.2
    ),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none"
  )

mean_gol2 <- df %>%
  filter(tiempo == 2) %>%
  group_by(gol, tiempo) %>%
  summarise(
    mean_time = median(minuto),
    q25 = quantile(minuto, 0.25),
    q75 = quantile(minuto, 0.75)
  )

segundo <- df %>%
  filter(tiempo == 2) %>%
  ggplot(aes(x = minuto, y = fecha, fill = gol)) +
  geom_vline(
    data = mean_gol2,
    aes(xintercept = mean_time, colour = gol),
    linetype = 2
  ) +
  geom_point(shape = 21, size = 5, color = "black") +
  scale_y_continuous(limits = c(1, 17), breaks = seq(1, 17, 1)) +
  scale_x_continuous(limits = c(45, 90), breaks = seq(45, 90, 5)) +
  scale_fill_manual(values = c('#0570b0', '#bdc9e1')) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~tiempo, scales = "free_x") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(
      linetype = 2,
      colour = "grey",
      size = 0.2
    ),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none"
  )


primero + segundo


df %>%
  ggplot(aes(x = minuto, y = gol, fill = gol)) +
  geom_density_ridges2(alpha = 1, rel_min_height = 0.01, scale = 0.9) +
  scale_fill_manual(values = c("green", "red")) +
  theme_ridges()
