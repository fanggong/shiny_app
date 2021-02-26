notebook_ui <- function(id, tab_name, samples, output_func) {
  ns <- NS(id)
  tabItem(
    tabName = tab_name,
    fluidRow(
      column(width = 1, h5("选择示例")),
      column(width = 5, selectInput(ns("input"), NULL, choices = samples, width = "100%")),
      column(width = 2, actionButton(ns("eval"), strong("Evaluate"), width = "80%")),
      column(width = 2, actionButton(ns("reset"), strong("Reset"), width = "80%")),
      column(width = 2, downloadButton(ns("download"), strong("Download"), style = "width:80%"))
    ),
    fluidRow(
      column(width = 6, do.call(output_func, args = list(outputId = ns("output"), height = "80vh"))),
      column(width = 6,aceEditor(ns("code"), value = "", mode = "r", theme = "tomorrow_night", height = "80vh"))
    )
  )
}

notebook_server <- function(id, render_func, data) {
  moduleServer(
    id,
    function(input, output, session) {
      code <- reactive({
        file <- paste0("script/", id, "/", input$input, ".R")
        file <- scan(file, what = character(), sep = "\n")
        file <- paste(file, collapse = "\n")
        file
      })

      observeEvent(input$input, {
        updateAceEditor(session, "code", value = code())
        output$output <- do.call(
          render_func, args = list(expr = {eval(parse(text = code()))})
        )
      })

      observeEvent(input$eval, {
        output$output <- do.call(
          render_func, args = list(expr = {eval(parse(text = isolate(input$code)))})
        )
      })

      observeEvent(input$reset, {
        updateAceEditor(session, "code", value = code())
      })
    }
  )
}
