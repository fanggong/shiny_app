server <- function(input, output, session) {
  
  files <- reactiveValues(dat = NULL, info = NULL)
  
  source("./component/configuration.R", local = TRUE, encoding = "UTF-8")
  
  source("./component/descriptive_analysis.R", local = TRUE, encoding = "UTF-8")
  
  source("./component/correlation_analysis.R", local = TRUE, encoding = "UTF-8")
  
  source("./component/difference_analysis.R", local = TRUE, encoding = "UTF-8")

}

