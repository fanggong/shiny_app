library(shiny)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(width = 2, numericInput("point_num", "输入点的数量", value = 100, step = 1)),
    column(width = 2, br(), actionButton("plot", "REPLOT"))
  ),
  fluidRow(
    column(width = 6, plotOutput("scatter_plot")),
    column(width = 6, plotOutput("rect_plot"))
  )
)

server <- function(input, output, session) {
  dat <- reactive({
    input$plot
    isolate(
      data.frame(
        long = rnorm(input$point_num),
        lat = rnorm(input$point_num)
      )
    )
  })
  
  output$scatter_plot <- renderPlot({
    input$plot
    ggplot(isolate(dat()), aes(x = long, y = lat)) +
      geom_point()
  })
  
  output$rect_plot <- renderPlot({
    input$plot
    ggplot(isolate(dat()), aes(x = long, y = lat)) + 
      geom_bin2d(bins = 10, drop = FALSE)
  })
  
}

shinyApp(ui, server)