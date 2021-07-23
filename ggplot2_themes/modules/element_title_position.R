TITLE_POS <- c("panel", "plot")

element_title_position_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      shinyWidgets::radioGroupButtons(
        ns("title_position_type"), 
        label = "Position", 
        choices = .types(),
        selected = .get_attr_type(args),
        justified = TRUE, 
        width = "100%"
      ),
      selectInput(
        ns("title_position"),
        label = NULL, 
        choices = TITLE_POS,
        selected = .set_default(args, TITLE_POS[1]), 
        width = "100%"
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = HEIGHT) %>% 
        shinycssloaders::withSpinner()
    )
  )
}

element_title_position_server <- function(id, graph) {
  moduleServer(
    id,
    function(input, output, session) {
      
      attrs <- list(
        "title_position_type" = "title_position"
      )
      
      mapply(.toggle_controler, names(attrs), attrs, list(input = input))
      
      new_theme[[id]] <- reactive({
        .assign(names(attrs), input)
        element_title_position(title_position = title_position)
      })
      
      output$plot <- renderCachedPlot({
        .get_plot(graph)
      }, cacheKeyExpr = .cache_key(graph))
    }
  )
}