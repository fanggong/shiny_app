element_align_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      shinyWidgets::radioGroupButtons(
        ns("align_type"), 
        label = "Alignment", 
        choices = .types(-1),
        selected = .get_attr_type(args),
        justified = TRUE, 
        width = "100%"
      ),
      numericInput(
        ns("align"), 
        label = NULL,
        step = 0.1, 
        value = .set_default(args, 0.5), 
        width = "100%"
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = HEIGHT) %>% 
        shinycssloaders::withSpinner()
    )
  )
}

element_align_server <- function(id, graph) {
  moduleServer(
    id,
    function(input, output, session) {
      
      attrs <- list(
        "align_type" = "align"
      )
      
      mapply(.toggle_controler, names(attrs), attrs, list(input = input))
      
      new_theme[[id]] <- reactive({
        .assign(names(attrs), input)
        element_align(align = align)
      })
      
      output$plot <- renderCachedPlot({
        .get_plot(graph)
      }, cacheKeyExpr = .cache_key(graph))
    }
  )
}