DIRECTION <- c("horizontal", "vertical")

element_direction_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      shinyWidgets::radioGroupButtons(
        ns("direction_type"), 
        label = "Direction", 
        choices = .types(-1),
        selected = .get_attr_type(args),
        justified = TRUE,
        width = "100%"
      ),
      selectInput(
        ns("direction"), 
        label = NULL, 
        choices = DIRECTION,
        selected = .set_default(args, DIRECTION[1]), 
        width = "100%"
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = HEIGHT) %>% 
        shinycssloaders::withSpinner()
    )
  )
}

element_direction_server <- function(id, graph) {
  moduleServer(
    id,
    function(input, output, session) {
      
      attrs <- list(
        "direction_type" = "direction"
      )
      
      mapply(.toggle_controler, names(attrs), attrs, list(input = input))
      
      new_theme[[id]] <- reactive({
        .assign(names(attrs), input)
        element_direction(direction = direction)
      })
      
      output$plot <- renderCachedPlot({
        .get_plot(graph)
      }, cacheKeyExpr = .cache_key(graph))
    }
  )
}