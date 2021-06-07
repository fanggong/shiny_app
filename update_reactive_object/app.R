library(shiny)
library(ggplot2)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 5,
      fluidRow(
        column(6, numericInput("mean", "均值", value = 10)),
        column(6, numericInput("sd", "方差", value = 10))
      ), hr(),
      actionButton("generate", "生成随机数", width = "100%"), hr(),
      actionButton("normalize", "归一化", width = "100%"), hr(),
      actionButton("standardize", "标准化", width = "100%"), hr(),
      actionButton("square", "平方", width = "100%")
    ),
    mainPanel(
      width = 7,
      verbatimTextOutput("numbers", placeholder = TRUE),
      plotOutput("plot")
    )
    
  )
)

server <- function(input, output, session) {
  dat <- reactiveValues(numbers = NULL)

  observeEvent(input$generate, {
    req(input$mean, input$sd)
    dat$numbers <- reactive({
      rnorm(100, mean = isolate(input$mean), sd = isolate(input$sd))
    })
  })
  
  output$numbers <- renderPrint({
    req(dat$numbers)
    summary(dat$numbers())
  })
  
  output$plot <- renderPlot({
    req(dat$numbers)
    ggplot(data.frame(random_number = dat$numbers())) + 
      geom_histogram(aes(x = random_number), bins = 50, fill = "#4994C4")
  })
  
  observeEvent(input$normalize, {
    req(dat$numbers)
    temp <- (dat$numbers() - min(dat$numbers())) / (max(dat$numbers()) - min(dat$numbers()))
    dat$numbers <- reactive({temp})
  })
  
  observeEvent(input$standardize, {
    req(dat$numbers)
    temp <- (dat$numbers() - mean(dat$numbers())) / sd(dat$numbers())
    dat$numbers <- reactive({temp})
  })
  
  observeEvent(input$square, {
    req(dat$numbers)
    temp <- dat$numbers()^2
    dat$numbers <- reactive({temp})
  })
}

shinyApp(ui, server)

