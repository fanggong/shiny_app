library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

ui <- dashboardPagePlus(
  header = dashboardHeaderPlus(
    title = "SHINY"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("shinyjs", tabName = "shinyjs")
    )
  ),
  body = dashboardBody(
    
  ),
)
