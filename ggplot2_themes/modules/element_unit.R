UNITS <- c("npc", "cm", "inches", "mm", "points", "picas", "bigpts", "dida",
           "cicero", "scaledpts", "lines", "char", "native", "snpc", "strwidth",
           "strheight", "grobwidth", "grobheight")

element_unit_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      shinyWidgets::radioGroupButtons(
        ns("unit_type"), 
        label = "Length", 
        choices = .types(),
        selected = .get_attr_type(args),
        justified = TRUE, 
        width = "100%"
      ),
      fluidRow(
        column(6, numericInput(
          ns("value"), 
          label = NULL, 
          value = .set_default(as.numeric(stringr::str_match(as.character(args), "[0-9.]+")), 3)
        )),
        column(6, selectInput(
          ns("unit"), 
          label = NULL, 
          choices = UNITS, 
          selected = .set_default(stringr::str_match(as.character(args), "[^0-9.]+"), "points")
        ))
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = HEIGHT) %>% 
        shinycssloaders::withSpinner()
    )
  )
}

element_unit_server <- function(id, graph) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      attrs <- list(
        "unit_type" = c("value", "unit")
      )
      
      mapply(.toggle_controler, names(attrs), attrs, list(input = input))
      
      
      new_theme[[id]] <- reactive({
        .assign(names(attrs), input)
        element_unit(unit = unit)
      })
      
      output$plot <- renderCachedPlot({
        .get_plot(graph)
      }, cacheKeyExpr = .cache_key(graph))
    }
  )
}



