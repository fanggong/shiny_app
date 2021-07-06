server <- function(input, output, session) {
  
  output$sample_data <- renderUI({
    element_rect_ui("rect")
  })
  
  element_rect_server("rect")
  
}