element_margin_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  if (!is.null(args)) {
    args <- as.character(args)
    value <- stringr::str_match(args, "[0-9.]+")
    unit <- stringr::str_match(args, "[^0-9.]+")
  } else {
    value <- rep(0, 4)
    unit <- rep("points", 4)
  }
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fluidRow(
        column(7, shinyWidgets::radioGroupButtons(
          ns("margin_type"), "Margin", choices = c("NULL", "Value"),
          selected = .get_attr_type(args), justified = TRUE, width = "100%"
        )),
      ),
      fluidRow(
        column(4, numericInput(
          ns("value_top"), label = "Top", value = as.numeric(value[1])
        )),
        column(5, selectInput(
          ns("unit_top"), label = br(), choices = UNITS, selected = unit[1], width = "100%"
        ))
      ),
      fluidRow(
        column(4, numericInput(
          ns("value_bottom"), label = "Bottom", value = as.numeric(value[3])
        )),
        column(5, selectInput(
          ns("unit_bottom"), label = br(), choices = UNITS, selected = unit[3]
        ))
      ),
      fluidRow(
        column(4, numericInput(
          ns("value_left"), label = "Left", value = as.numeric(value[4])
        )),
        column(5, selectInput(
          ns("unit_left"), label = br(), choices = UNITS, selected = unit[4]
        ))
      ),
      fluidRow(
        column(4, numericInput(
          ns("value_right"), label = "Right", value = as.numeric(value[2])
        )),
        column(5, selectInput(
          ns("unit_right"), label = br(), choices = UNITS, selected = unit[2]
        ))
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = "600px") %>% shinycssloaders::withSpinner(),
      verbatimTextOutput(ns("theme"), placeholder = TRUE)
    )
  )
}

element_margin_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      directions <- c("top", "bottom", "left", "right")
      observeEvent(input$margin_type, {
        if (input$margin_type == "NULL") {
          for (d in directions) {
            shinyjs::disable(paste0("value_", d))
            shinyjs::disable(paste0("unit_", d))
          }
        } else {
          for (d in directions) {
            shinyjs::enable(paste0("value_", d))
            shinyjs::enable(paste0("unit_", d))
          }
        }
      })
      
      new_theme[[id]] <- reactive({
        if (input$margin_type == "NULL") {
          return(NULL)
        }
        element_margin(
          t = input$value_top, r = input$value_right,
          b = input$value_bottom, l = input$value_left,
          unit = c(input$unit_top, input$unit_right, input$unit_bottom, input$unit_left)
        )
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