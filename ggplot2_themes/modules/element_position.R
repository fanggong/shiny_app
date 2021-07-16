element_position_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("position_type"), "Position", choices = c("NULL", "Value"),
          selected = .get_attr_type(args),
          justified = TRUE, width = "100%"
        )),
        column(6, selectInput(
          ns("position"), label = br(), choices = POSITION,
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

element_position_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$position_type, {
        if (input$position_type == "NULL") {
          shinyjs::disable("position")
        } else {
          shinyjs::enable("position")
        }
      })
      
      new_theme[[id]] <- reactive({
        if (input$position_type == "NULL") {
          return(NULL)
        }
        element_position(position = input$position)
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