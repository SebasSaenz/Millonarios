library(tidyverse)
library(plotly)
library(htmlwidgets)
library(htmltools)
library(ggtext)

df <- read_tsv("data/rendimiento_2026.txt")

rendimiento <- df %>%
  mutate(
    expected_points = partidos * 3,
    performance = 100 * (puntos / expected_points),
    label = factor(label, levels = c("Millonarios", "Primero", "Octavo"))
  )

p <- rendimiento %>%
  ggplot(aes(
    x = jornada,
    y = performance,
    colour = label,
    group = label,
    text = paste0(
      "Equipo: ",
      equipo,
      "<br>Fecha: ",
      jornada,
      "<br>Rendimiento: ",
      round(performance, 1),
      "%"
    )
  )) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1, 7, 1)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  scale_color_manual(values = c("blue", "#404040", "#bababa")) +
  labs(x = "Fecha", y = "Rendimiento (%)") +
  theme_bw() +
  theme(legend.title = element_blank())

# Responsive plotly
p_int <- ggplotly(p, tooltip = "text") %>%
  style(hoverinfo = "text") %>%
  layout(
    showlegend = TRUE,
    # IMPORTANT: no fixed width for mobile
    height = 520,
    legend = list(
      title = list(text = NULL),
      font = list(size = 12, family = "optima")
      # optional phone-friendly legend:
      # orientation = "h", x = 0.5, xanchor = "center", y = -0.18
    ),
    title = list(
      text = "Rendimiento de Millonarios comparado<br>al primero y octavo en cada fecha (2026-I)",
      x = 0.5,
      xanchor = "center",
      font = list(size = 28, family = "optima")
    ),
    margin = list(t = 120, r = 10, b = 120, l = 55),
    annotations = list(
      list(
        text = "<b>Data:</b> Tablas José O. Ascencio @josasc<br><b>Code:</b> @SaenzJohanS - GitHub: SebasSaenz",
        x = 0,
        y = -0.30,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        xanchor = "left",
        align = "left",
        font = list(size = 13, family = "optima")
      )
    )
  ) %>%
  config(responsive = TRUE)

# Mobile-friendly HTML page
page <- tagList(
  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    tags$title("Rendimiento Liga 2026"),
    tags$style(HTML(
      "
      body { margin: 0; padding: 12px; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif; }
      .wrap { max-width: 900px; margin: 0 auto; }
      .plotbox { width: 100%; }
      /* On phones: slightly smaller title space + keep plot tall enough */
      @media (max-width: 600px) {
        body { padding: 8px; }
        .plotbox { height: 75vh; }  /* plot height relative to screen */
      }
    "
    ))
  ),
  div(class = "wrap", div(class = "plotbox", p_int))
)

dir.create("docs", showWarnings = FALSE)
dir.create("docs/libs", showWarnings = FALSE, recursive = TRUE)
htmltools::save_html(page, file = "docs/index.html", libdir = "docs/libs")
