library(cranlogs)
library(shiny)
library(shinyWidgets)
library(data.table)
library(lubridate)
library(plotly)

src <- cran_downloads("flipdownWidgets", from = "2020-01-01")
setDT(src)
src <- src[count != 0]
src[
  , `:=`(
    week = floor_date(as.Date(date), unit = "week", week_start = 1),
    month = floor_date(as.Date(date), unit = "month")
  )
]
day <- src[, .(count = sum(count)), .(period = date)]
week <- src[, .(count = sum(count)), .(period = week)]
month <- src[, .(count = sum(count)), .(period = month)]

ui <- fluidPage(
  fluidRow(
    column(
      width = 6,
      radioGroupButtons(
        "period", choices = c("Day" = "day", "Week" = "week", "Month" = "month"),
        direction = "horizontal", justified = TRUE, size = "normal"
      )
    ),
    column(
      width = 12,
      plotlyOutput("downloads_trend")
    )
  )
)

server <- function(input, output, session) {
  period <- reactive({
    input$period
  })
  
  output$test <- renderText({
    period()
  })
  
  output$downloads_trend <- renderPlotly({
    plot_ly(get(period()), type = "scatter", mode = "lines") %>%
      add_trace(
        x = ~period, y = ~count, name = period(),
        line = list(color = "#0F2540", width = 2), opacity = 0.5
      ) %>%
      layout(
        xaxis = list(title = "Date", rangeslider = list(visible = TRUE)),
        yaxis = list(title = "Download Times"),
        showlegend = FALSE,
        margin = list(l = 10, t = 0, r = 10, b = 0)
      )
  })
}

shinyApp(ui, server)


