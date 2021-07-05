server <- function(input, output, session) {
  
  output$sample_data <- renderUI({
    element_rect_ui("rect", kk$input)
  })
  
  element_rect_server("rect")
  
}