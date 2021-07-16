
element_direction_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("direction_type"), "Direction", choices = c("NULL", "Value"),
          selected = .get_attr_type(args),
          justified = TRUE, width = "100%"
        )),
        column(6, selectInput(
          ns("direction"), label = br(), choices = DIRECTION,
          selected = args, width = "100%"
        ))
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = "600px") %>% shinycssloaders::withSpinner(),
      verbatimTextOutput(ns("theme"), placeholder = TRUE)
    )
  )
}

element_direction_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$direction_type, {
        if (input$direction_type == "NULL") {
          shinyjs::disable("direction")
        } else {
          shinyjs::enable("direction")
        }
      })
      
      new_theme[[id]] <- reactive({
        if (input$direction_type == "NULL") {
          return(NULL)
        }
        element_direction(direction = input$direction)
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