PLACEMENT <- c("inside", "outside")

element_placement_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      shinyWidgets::radioGroupButtons(
        ns("placement_type"), 
        label = "Placement", 
        choices = .types(),
        selected = .get_attr_type(args),
        justified = TRUE,
        width = "100%"
      ),
      selectInput(
        ns("placement"), 
        label = NULL, 
        choices = PLACEMENT,
        selected = .set_default(args, PLACEMENT[1]), 
        width = "100%"
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = HEIGHT) %>% 
        shinycssloaders::withSpinner()
    )
  )
}

element_placement_server <- function(id, graph) {
  moduleServer(
    id,
    function(input, output, session) {
      
      attrs <- list(
        "placement_type" = "placement"
      )
      
      mapply(.toggle_controler, names(attrs), attrs, list(input = input))
      
      new_theme[[id]] <- reactive({
        .assign(names(attrs), input)
        element_placement(placement = placement)
      })
      
      output$plot <- renderCachedPlot({
        .get_plot(graph)
      }, cacheKeyExpr = .cache_key(graph))
    }
  )
}