element_logical_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fluidRow(
        column(12, shinyWidgets::materialSwitch(
          ns("logical"), label = strong("Set the element to TRUE"),
          value = args, status = "primary", right = TRUE
        ))
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = "600px") %>% shinycssloaders::withSpinner(),
      verbatimTextOutput(ns("theme"), placeholder = TRUE)
    )
  )
}

element_logical_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      new_theme[[id]] <- reactive({
        element_logical(logical = input$logical)
      })
      
      output$theme <- renderPrint({
        .reactiveValues_to_theme(new_theme)
      })
      
      output$plot <- renderCachedPlot({
        plot + .reactiveValues_to_theme(new_theme)
      }, cacheKeyExpr = .reactiveValues_to_theme(new_theme))
    }
  )
}