
ui <- fluidPage(
  h1("ggtheme generator"),
  fluidRow(
    column(
      width = 6,
      tabsetPanel(
        id = "xx", type = "tabs",
        tabPanel("axis", wellPanel()),
        tabPanel("title", wellPanel()),
        tabPanel("legend", wellPanel())
      )
    ),
    column(
      width = 6,
      plotOutput("xxx")
    )
  )
)
