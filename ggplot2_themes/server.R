server <- function(input, output, session) {
  
  # output$own_data <- renderUI({
  #   h3("Developing...")
  # })
  # 
  # output$sample_data <- renderUI({
  #   h3("Developing...")
  # })
  
  for (ele in names(ele_config)) {
    eval(parse(text = sprintf(
      "output$%s <- renderUI({
        do.call(%s, args = list(id = \"%s\"))
        do.call(%s, args = list(id = \"%s\"))
      })", 
      ele, 
      paste0(ele_config[[ele]], "_server"), ele,
      paste0(ele_config[[ele]], "_ui"), ele
    )))
  }
}

