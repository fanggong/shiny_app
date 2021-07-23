element_logical_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      shinyWidgets::materialSwitch(
        ns("logical"), 
        label = strong("Set the element to TRUE"),
        value = args, 
        status = "primary", 
        right = TRUE
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = HEIGHT) %>% 
        shinycssloaders::withSpinner()
    )
  )
}

element_logical_server <- function(id, graph) {
  moduleServer(
    id,
    function(input, output, session) {
      
      new_theme[[id]] <- reactive({
        element_logical(logical = input$logical)
      })
      
      output$plot <- renderCachedPlot({
        .get_plot(graph)
      }, cacheKeyExpr = .cache_key(graph))
    }
  )
}