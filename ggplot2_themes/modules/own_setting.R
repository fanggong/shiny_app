# SAMPLE_CODE <- "# enter code here to generate sample graph
# # uploaded data was assigned to variable \"data\"
# # for example:
# # ggplot(data, aes(x, y)) +
# #   geom_point()
# library(ggplot2)
# ggplot(data, aes(Sepal.Length, Sepal.Width)) +
#   geom_point()
# "

SAMPLE_CODE <- "# Not available for security reasons"

own_setting_ui <- function(id = "own_setting") {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fileInput(
        ns("data"),
        label = "Upload data",
        width = "100%"
      ),
      shinyAce::aceEditor(
        ns("code"),
        value = SAMPLE_CODE,
        tabSize = 2,
        readOnly = TRUE
      ),
      hr(),
      actionButton(
        ns("generate"),
        label = "Generate Sample Graph",
        width = "100%"
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = HEIGHT) %>% 
        shinycssloaders::withSpinner()
    )
  )
  
}

own_setting_server <- function(id = "own_setting") {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$generate, {
        req(input$data)
        output$plot <- renderCachedPlot({
          file <- input$data
          ext <- tools::file_ext(file$datapath)
          if (ext == "csv") {
            data <- read.csv(file$datapath)
          }
          own_plot$plot <- eval(parse(text = isolate(input$code)))
          .get_plot(function() "own")
        }, cacheKeyExpr = .cache_key())
      })
    }
  )
}