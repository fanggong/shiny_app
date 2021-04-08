library(shiny)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(width = 3, actionButton("plot", "点击重置散点图", width = "80%"))
  ),
  fluidRow(
    column(width = 8, plotOutput("scatter_plot"), offset = 2)
  )
)

server <- function(input, output, session) {
  dat <- eventReactive(input$plot, {
    num <- sample(1000, 1)
    data.frame(
      long = rnorm(num),
      lat = rnorm(num),
      pos = sample(c("E", "W", "N", "S"), num, replace = TRUE)
    )
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(dat(), aes(x = long, y = lat, color = pos)) +
      geom_point(size = 2) + 
      theme_bw()
  })
}

shinyApp(ui, server)