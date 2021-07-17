element_justification_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  if (!is.null(args)) {
    if (args[1] == "center") {
      just_x <- 0.5
      just_y <- 0.5
    } else {
      just_x <- args[1]
      just_y <- args[2]
    }
  } else {
    just_x <- 0.5
    just_y <- 0.5
  }
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fluidRow(
        column(8, shinyWidgets::radioGroupButtons(
          ns("justification_type"), "Anchor point / Justification", 
          choices = c("NULL", "Value"),
          selected = .get_attr_type(args),
          justified = TRUE, width = "100%"
        ))
      ),
      fluidRow(
        column(5, numericInput(
          ns("just_x"), label = "Horizontal", step = 0.1, value = as.numeric(just_x)
        )),
        column(5, numericInput(
          ns("just_y"), label = "Vertical", step = 0.1, value = as.numeric(just_y)
        ))
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = "600px") %>% shinycssloaders::withSpinner(),
      verbatimTextOutput(ns("theme"), placeholder = TRUE)
    )
  )
}

element_justification_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      observeEvent(input$justification_type, {
        if (input$justification_type == "NULL") {
          shinyjs::disable("just_x")
          shinyjs::disable("just_y")
        } else {
          shinyjs::enable("just_x")
          shinyjs::enable("just_y")
        }
      })

      new_theme[[id]] <- reactive({
        if (input$justification_type == "NULL") {
          return(NULL)
        }
        element_justification(justification = c(input$just_x, input$just_y))
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



