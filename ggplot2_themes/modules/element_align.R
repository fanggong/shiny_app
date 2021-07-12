element_align_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("align_type"), "Align", choices = c("NULL", "Value"),
          selected = .get_attr_type(args),
          justified = TRUE, width = "100%"
        )),
        column(6, numericInput(
          ns("align"), label = br(), min = 0, max = 1, step = 0.1, 
          value = ifelse(is.null(args), 0, args), width = "100%"
        ))
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = "600px") %>% shinycssloaders::withSpinner()
    )
  )
}

element_align_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$align_type, {
        if (input$align_type == "NULL") {
          shinyjs::disable("align")
        } else {
          shinyjs::enable("align")
        }
      })
      
      new_theme[[id]] <- reactive({
        if (input$align_type == "NULL") {
          return(NULL)
        }
        element_align(number = input$align)
      })
      
      output$plot <- renderCachedPlot({
        plot + .reactiveValues_to_theme(new_theme)
      }, cacheKeyExpr = .reactiveValues_to_theme(new_theme))
    }
  )
}