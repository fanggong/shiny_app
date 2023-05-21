server <- function(input, output, session) {
  
  files <- reactiveValues(dat = NULL, info = NULL)
  
  output$data <- renderUI({ validate(need(NULL, "No Data Avaliable")) })
  
  output$des_plot <- renderUI({ validate(need(NULL, "No Data Avaliable")) })
  
  output$cor_plot <- renderUI({ validate(need(NULL, "No Data Avaliable")) })
  
  output$cor_res <- renderUI({ validate(need(NULL, "No Data Avaliable")) })
  
  output$diff_plot <- renderUI({ validate(need(NULL, "No Data Avaliable")) })
  
  output$diff_res <- renderUI({ validate(need(NULL, "No Data Avaliable")) })
  
  source("./component/configuration.R", local = TRUE, encoding = "UTF-8")
  
  source("./component/descriptive_analysis.R", local = TRUE, encoding = "UTF-8")
  
  source("./component/correlation_analysis.R", local = TRUE, encoding = "UTF-8")
  
  source("./component/difference_analysis.R", local = TRUE, encoding = "UTF-8")

}

