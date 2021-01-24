library(shiny)
library(shinypanel)

ui <- fluidPage(
  sidebarPanel(
    selectizeInputWithButtons("SIwB","selectizeInputWithButtons"),
    selectizeInputWithValidation("SIwV", "selectizeInputWithValidation"),
    textInputWithButtons("TIwB", "textInputWithButtons"),
    textInputWithValidation("TIwV", "textInputWithValidation"),
    textAreaInputWithButtons("TAIwB", "textAreaInputWithButtons"),
  ),
  mainPanel(
    
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

