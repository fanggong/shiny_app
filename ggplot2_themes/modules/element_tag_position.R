element_tag_position_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("tag_position_type"), "Tag Position", choices = c("NULL", "Value"),
          selected = .get_attr_type(args),
          justified = TRUE, width = "100%"
        )),
        column(6, selectInput(
          ns("tag_position"), label = br(), choices = TAG_POS,
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

element_tag_position_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$tag_position_type, {
        if (input$tag_position_type == "NULL") {
          shinyjs::disable("tag_position")
        } else {
          shinyjs::enable("tag_position")
        }
      })
      
      new_theme[[id]] <- reactive({
        if (input$tag_position_type == "NULL") {
          return(NULL)
        }
        element_tag_position(tag_position = input$tag_position)
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