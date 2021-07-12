element_placement_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("placement_type"), "Placement", choices = c("NULL", "Value"),
          selected = .get_attr_type(args),
          justified = TRUE, width = "100%"
        )),
        column(6, selectInput(
          ns("placement"), label = br(), choices = PLACEMENT,
          selected = args, width = "100%"
        ))
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = "600px") %>% shinycssloaders::withSpinner()
    )
  )
}

element_placement_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$placement_type, {
        if (input$placement_type == "NULL") {
          shinyjs::disable("placement")
        } else {
          shinyjs::enable("placement")
        }
      })
      
      new_theme[[id]] <- reactive({
        if (input$placement_type == "NULL") {
          return(NULL)
        }
        element_placement(placement = input$placement)
      })
      
      output$plot <- renderCachedPlot({
        plot + .reactiveValues_to_theme(new_theme)
      }, cacheKeyExpr = .reactiveValues_to_theme(new_theme))
    }
  )
}