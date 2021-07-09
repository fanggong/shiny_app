
element_unit_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  if (!is.null(args)) {
    args <- as.character(args)
    value <- stringr::str_match(args, "[0-9.]+")
    unit <- stringr::str_match(args, "[^0-9.]+")
  } else {
    value <- 0
    unit <- "points"
  }
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fluidRow(
        column(5, shinyWidgets::radioGroupButtons(
          ns("unit_type"), "Length", choices = c("NULL", "Value"),
          selected = .get_attr_type(args),
          justified = TRUE, width = "100%"
        )),
        column(3, numericInput(
          ns("value"), label = br(), value = as.numeric(value)
        )),
        column(4, selectInput(
          ns("unit"), label = br(), choices = UNITS, selected = unit
        ))
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = "600px") %>% shinycssloaders::withSpinner(),
      verbatimTextOutput(ns("theme"), placeholder = TRUE)
    )
  )
}

element_unit_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      observeEvent(input$unit_type, {
        if (input$unit_type == "NULL") {
          shinyjs::disable("value")
          shinyjs::disable("unit")
        } else {
          shinyjs::enable("value")
          shinyjs::enable("unit")
        }
      })
      
      new_theme[[id]] <- reactive({
        if (input$unit_type == "NULL") {
          return(NULL)
        }
        unit(x = input$value, units = input$unit)
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



