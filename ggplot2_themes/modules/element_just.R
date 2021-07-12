element_just_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("just_type"), "Justification", choices = c("NULL", "Value"),
          selected = .get_attr_type(args),
          justified = TRUE, width = "100%"
        )),
        column(6, selectInput(
          ns("just"), label = br(), choices = JUST,
          selected = args, width = "100%"
        ))
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = "600px") %>% shinycssloaders::withSpinner()
    )
  )
}

element_just_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$just_type, {
        if (input$just_type == "NULL") {
          shinyjs::disable("just")
        } else {
          shinyjs::enable("just")
        }
      })
      
      new_theme[[id]] <- reactive({
        if (input$just_type == "NULL") {
          return(NULL)
        }
        element_just(just = input$just)
      })
      
      output$plot <- renderCachedPlot({
        plot + .reactiveValues_to_theme(new_theme)
      }, cacheKeyExpr = .reactiveValues_to_theme(new_theme))
    }
  )
}