theme_print_ui <- function(id) {
  ns <- NS(id)
  verbatimTextOutput(ns("theme"), placeholder = TRUE)
}

theme_print_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$theme <- renderPrint({
        THEME
      })
      return(list(
        input = input,
        output = output
      ))
    }
  )
}