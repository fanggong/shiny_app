header_ui<- function(id = "header") {
  ns <- NS(id)
  
  fluidPage(
    column(6, shinyWidgets::prettyRadioButtons(
      ns("graph"),
      label = NULL,
      choices = c(
        "Default Sample Graph" = "default",
        "Your Own Sample Graph" = "own"
      ),
      selected = "default",
      inline = TRUE, 
      status = "danger",
      fill = TRUE
    )),
    column(2, downloadBttn(
      ns("download"),
      label = "Download Theme",
      size = "sm",
      style = "unite",
      block = TRUE
    ), offset = 4)
  )
}

header_server <- function(id = "header") {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$download <- downloadHandler(
        filename = "theme.rda",
        content = function(file) {
          theme <- .reactiveValues_to_theme(new_theme)
          save(theme, file = file)
        }
      )
      
      return(reactive({input$graph}))
    }
  )
}