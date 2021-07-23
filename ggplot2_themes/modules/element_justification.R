element_justification_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  if (.get_attr_type(args) == .types(3) && args == "center") {
    args <- c(0.5, 0.5)
  }

  sidebarLayout(
    sidebarPanel = sidebarPanel(
      shinyWidgets::radioGroupButtons(
        ns("justification_type"), 
        label = "Anchor point / Justification", 
        choices = .types(-1),
        selected = .get_attr_type(args),
        justified = TRUE, 
        width = "100%"
      ),
      fluidRow(
        column(6, numericInput(
          ns("just_x"), 
          label = "horizontal", 
          step = 0.1, 
          value = as.numeric(.set_default(args[1], 0.5))
        )),
        column(6, numericInput(
          ns("just_y"), 
          label = "vertical", 
          step = 0.1, 
          value = as.numeric(.set_default(args[2], 0.5))
        ))
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = HEIGHT) %>% 
        shinycssloaders::withSpinner()
    )
  )
}

element_justification_server <- function(id, graph) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      attrs <- list(
        "justification_type" = c("just_x", "just_y")
      )
      
      mapply(.toggle_controler, names(attrs), attrs, list(input = input))

      new_theme[[id]] <- reactive({
        .assign(names(attrs), input)
        element_justification(justification = justification)
      })

      output$plot <- renderCachedPlot({
        .get_plot(graph)
      }, cacheKeyExpr = .cache_key(graph))
    }
  )
}



