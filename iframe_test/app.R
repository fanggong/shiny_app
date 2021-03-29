library(shiny)
library(ggplot2)

ui <- fluidPage(
  numericInput("point_num", "输入点的数量", value = 100, min = 10, max = 500, step = 1),
  plotOutput("scatter_plot")
)

server <- function(input, output, session) {
  dat <- reactive({
    data.frame(
      long = rnorm(input$point_num),
      lat = rnorm(input$point_num)
    )
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(dat(), aes(x = long, y = lat)) +
      geom_point()
  })
}

shinyApp(ui, server)