ui <- dashboardPagePlus(
  header = dashboardHeaderPlus(
    title = "SHINY"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("dygraphs", tabName = "dygraphs"),
      menuItem("leaflet", tabName = "leaflet"),
      menuItem("networkD3", tabName = "networkD3"),
      menuItem("DataTables", tabName = "DataTables"),
      menuItem("threejs", tabName = "threejs"),
      menuItem("plotly", tabName = "plotly"),
      menuItem("shinyjs", tabName = "shinyjs")
    )
  ),
  body = dashboardBody(
    tabItems(
      source("ui/leaflet.R", local = TRUE)$value,
      source("ui/dygraphs.R", local = TRUE)$value,
      source("ui/networkD3.R", local = TRUE)$value,
      source("ui/DataTables.R", local = TRUE)$value,
      source("ui/threejs.R", local = TRUE)$value,
      source("ui/shinyjs.R", local = TRUE)$value
    )
  )
)
