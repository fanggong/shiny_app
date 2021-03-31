server <- function(input, output, session) {
  source("server/leaflet.R", local = TRUE)
  source("server/dygraphs.R", local = TRUE, encoding = "UTF-8")
  source("server/networkD3.R", local = TRUE)
  source("server/DataTables.R", local = TRUE)
  source("server/threejs.R", local = TRUE)
}
