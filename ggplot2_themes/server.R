server <- function(input, output, session) {
  
  sample_graph <- header_server()
  default_setting_server()
  own_setting_server()
  
  for (ele in names(ele_config)) {
    eval(parse(text = sprintf(
      "output$%s <- renderUI({
        do.call(%s, args = list(id = \"%s\", graph = sample_graph))
        do.call(%s, args = list(id = \"%s\"))
      })",
      ele,
      paste0(ele_config[[ele]], "_server"), ele,
      paste0(ele_config[[ele]], "_ui"), ele
    )))
  }
}

