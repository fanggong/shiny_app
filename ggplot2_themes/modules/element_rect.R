.reactiveValuesToTheme <- function(x) {
  x <- reactiveValuesToList(x)
  for (each in names(x)) {
    if (inherits(x[[each]], "function")){
      x[[each]] <- x[[each]]()
    }
  }
  attr(x, "class") <- c("theme", "gg")
  attr(x, "")
  x
}

element_rect_ui <- function(id) {
  ns <- NS(id)
  args <- theme_init[[id]]
  sidebarLayout(
    sidebarPanel(
      colourInput(ns("fill"), label = "fill", value = args$fill),
      colourInput(ns("color"), label = "color", value = args$color),
      numericInput(ns("size"), label = "size", value = args$size, min = 0),
      numericInput(ns("linetype"), label = "linetype", value = args$linetype),
      selectInput(ns("inherit.blank"), label = "inherit.blank", 
                  choices = c(TRUE, FALSE), selected = args$inherit.blank)
    ),
    mainPanel(
      verbatimTextOutput(ns("theme")),
      plotOutput(ns("plot"))
    )
  )
}

element_rect_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      new_theme[[id]] <- reactive({
        element_rect(
          fill = input$fill,
          color = input$color,
          size = input$size,
          linetype = input$linetype,
          inherit.blank = as.logical(input$inherit.blank)
        )
      })
      # output$theme <- renderPrint({
      #   .enhanced_reactiveValuesToList(new_theme)
      # })
      output$plot <- renderPlot({
        ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
          geom_point() +
          .enhanced_reactiveValuesToList(new_theme)
      })
    }
  )
} 