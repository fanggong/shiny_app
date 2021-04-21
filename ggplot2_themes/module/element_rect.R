element_rect_ui <- function(id, args = theme[[id]]) {
  ns <- NS(id)
  wellPanel(
    textInput(ns("fill"), label = "fill", value = args$fill),
    textInput(ns("color"), label = "color", value = args$color),
    numericInput(ns("size"), label = "size", value = args$size, min = 0),
    selectizeInput(ns("line_type_format"), label = "line_type_format",
                   choices = c("number", "string", "hexadecimal digits"),
                   selected = "number"),
    uiOutput(ns("line_type")),
    selectizeInput(ns("inherit.blank"), label = "inherit.blank", 
                   choices = c(TRUE, FALSE), selected = args$inherit.blank)
  )
}

element_rect_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      output$line_type <- renderUI({
        switch(
          input$line_type_format,
          "number" = numericInput("line_type", label = "line_type", min = 0, max = 8, step = 1, value = 1),
          "string" = selectizeInput("line_type", label = "line_type", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")),
          "hexadecimal digits" = textInput("line_type", label = "line_type")
        )
      })
    }
  )
} 