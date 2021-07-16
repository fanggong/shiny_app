element_title_position_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("title_position_type"), "Title Position", choices = c("NULL", "Value"),
          selected = .get_attr_type(args),
          justified = TRUE, width = "100%"
        )),
        column(6, selectInput(
          ns("title_position"), label = br(), choices = TITLE_POS,
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

element_title_position_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$title_position_type, {
        if (input$title_position_type == "NULL") {
          shinyjs::disable("title_position")
        } else {
          shinyjs::enable("title_position")
        }
      })
      
      new_theme[[id]] <- reactive({
        if (input$title_position_type == "NULL") {
          return(NULL)
        }
        element_title_position(title_position = input$title_position)
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