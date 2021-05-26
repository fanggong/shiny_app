server <- function(input, output, session) {
  output$plot_element <- renderUI({
    wellPanel(
      checkboxGroupInput(
        inputId = "plot_element", 
        label = "选择示例图形需要包含的元素：",
        choices = c(
          "图例（legend）" = "legend",
          "标题（title）" = "title",
          "副标题（subtitle）" = "subtitle",
          "说明（caption）" = "caption",
          "标签（tag）" = "tag",
          "分面（facet）" = "facet"
        ),
        selected = c("legend", "title", "subtitle", "caption", "tag", "facet"),
        inline = TRUE
      )
    )
  })
  
  output$plot <- renderPlot({
    plt <- ggplot(dat)
    
    if ("legend" %in% input$plot_element) {
      plt <- plt + geom_point(aes(x = long, y = lat, shape = shape, color = color))
    } else {
      plt <- plt + geom_point(aes(x = long, y = lat))
    }
    
    if ("title" %in% input$plot_element) {
      plt <- plt + labs(title = "THIS IS TITLE")
    }
    
    if ("subtitle" %in% input$plot_element) {
      plt <- plt + labs(subtitle = "THIS IS SUBTITLE")
    }
    
    if ("caption" %in% input$plot_element) {
      plt <- plt + labs(caption = "THIS IS CAPTION")
    }
    
    if ("tag" %in% input$plot_element) {
      plt <- plt + labs(tag = "THIS IS TAG")
    }
    
    if ("facet" %in% input$plot_element) {
      plt <- plt + facet_grid(rows = vars(facet_y), cols = vars(facet_x))
    }
    
    plt
  })
}