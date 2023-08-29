library(cranlogs)
library(shiny)
library(shinyWidgets)
library(data.table)
library(lubridate)
library(plotly)

src <- cran_downloads("okxAPI", from = as.character(Sys.Date() - 60))

ui <- fluidPage(
  fluidRow(
    column(
      width = 12,
      plotlyOutput("downloads_trend")
    )
  )
)

server <- function(input, output, session) {
  
  output$downloads_trend <- renderPlotly({
    plot_ly(opacity = 0.7) %>% 
      add_trace(
        x = src$date, y = src$count, type = "scatter", mode = "lines",
        line = list(color = "#3182BD"),
        hoverinfo = "text", hovertext = paste0(
          "</br> Date: ", src$date,
          "</br> Download Times: ", src$count
        )
      ) %>% 
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Download Times", zeroline = FALSE),
        showlegend = FALSE, hovermode = "x unified"
      )
  })
}

shinyApp(ui, server)


