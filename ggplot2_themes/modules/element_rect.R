element_rect_ui <- function(id, input) {
  ns <- NS(id)
  args <- THEME[[id]]
  sidebarLayout(
    sidebarPanel(
      colourInput(ns("fill"), label = "fill", value = args$fill),
      colourInput(ns("color"), label = "color", value = args$color),
      numericInput(ns("size"), label = "size", value = args$size, min = 0),
      selectInput(ns("line"), label = "line", choices = c("number", "string", "hexadecimal digits")),
      selectInput(ns("inherit.blank"), label = "inherit.blank", 
                  choices = c(TRUE, FALSE), selected = args$inherit.blank)
    ),
    mainPanel(
      input
    )
  )
}

element_rect_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      
    }
  )
} 