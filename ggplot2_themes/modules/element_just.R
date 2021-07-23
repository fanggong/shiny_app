JUST <- c("top", "bottom", "left", "right")

element_just_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      shinyWidgets::radioGroupButtons(
        ns("just_type"), 
        label = "Justification", 
        choices = .types(),
        selected = .get_attr_type(args),
        justified = TRUE, 
        width = "100%"
      ),
      selectInput(
        ns("just"), 
        label = NULL, 
        choices = JUST,
        selected = .set_default(args, JUST[1]), 
        width = "100%"
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = HEIGHT) %>% 
        shinycssloaders::withSpinner()
    )
  )
}

element_just_server <- function(id, graph) {
  moduleServer(
    id,
    function(input, output, session) {
      
      attrs <- list(
        "just_type" = "just"
      )
      
      mapply(.toggle_controler, names(attrs), attrs, list(input = input))
      
      new_theme[[id]] <- reactive({
        .assign(names(attrs), input)
        element_just(just = just)
      })
      
      
      output$plot <- renderCachedPlot({
        .get_plot(graph)
      }, cacheKeyExpr = .cache_key(graph))
    }
  )
}