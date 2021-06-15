server <- function(input, output, session) {
  
  files <- reactiveValues(dat = NULL, info = NULL)
  output$numerical_description <- renderUI({
    validate(need(NULL, "No Data Provided"))
  })
  output$cate_ordi_description <- renderUI({
    validate(need(NULL, "No Data Provided"))
  })
  output$other_description <- renderUI({
    validate(need(NULL, "No Data Provided"))
  })
  
  source("./component/data_upload_server.R", local = TRUE, encoding = "UTF-8")
}

