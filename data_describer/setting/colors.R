
LINE_COLOR <- "#005CAF"
POINT_COLOR <- "#0F2540"
BAR_COLOR <- "#005CAF"
OPACITY <- 0.7

FONT_FAMILY <- "Fira Mono"


MY_DASHBOARD <- fresh::create_theme(
  adminlte_color(
    light_blue = "#434C5E",
    aqua = "#81A1C1"
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  )
)
