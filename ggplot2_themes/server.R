server <- function(input, output, session) {
  
  # TODO
  # output$own_data <- renderUI({
  #   h3("Developing...")
  # })
  # 
  # output$sample_data <- renderUI({
  #   h3("Developing...")
  # })
  
  for (ele in names(ele_config)) {
    if (ele_config[[ele]] %in% "element_rect") {
      eval(parse(text = sprintf(
        "output$%s <- renderUI({
          element_rect_server(\"%s\")
          element_rect_ui(\"%s\")
        })", ele, ele, ele
      )))
    } else if (ele_config[[ele]] %in% "element_unit") {
      eval(parse(text = sprintf(
        "output$%s <- renderUI({
          element_unit_server(\"%s\")
          element_unit_ui(\"%s\")
        })", ele, ele, ele
      )))
    }
  }

}