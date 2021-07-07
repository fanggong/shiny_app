server <- function(input, output, session) {
  
  for (ele in names(ele_config)) {
    if (ele_config[[ele]] == "element_rect") {
      eval(parse(text = sprintf(
        "output$%s <- renderUI({
          element_rect_server(\"%s\")
          element_rect_ui(\"%s\")
        })", ele, ele, ele
      )))
    }
  }

  # output$rect <- renderUI({
  #   element_rect_server("rect")
  #   element_rect_ui("rect")
  # })
  # 
  # output$panel.background <- renderUI({
  #   element_rect_server("panel.background")
  #   element_rect_ui("panel.background")
  # })
  # 
}