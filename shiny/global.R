if (!require(pool)) install.packages("pool")
if (!require(jsonlite)) install.packages("jsonlite")
if (!require(RPostgreSQL)) install.packages("RPostgreSQL")

if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(shinydashboardPlus)) install.packages("shinydashboardPlus")
if (!require(leaflet)) install.packages("leaflet")
if (!require(dygraphs)) install.packages("dygraphs")
if (!require(shinyAce)) install.packages("shinyAce")
if (!require(dplyr)) install.packages("dplyr")

source("module/notebook.R")

db_info <- jsonlite::read_json("config.json")$postgres
pool <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  dbname = db_info$dbname,
  host = db_info$host,
  user = db_info$user,
  password = db_info$password
)

query <- "select date, open, high, low, close
          from index_history
          where code = '000001.XSHG'"
index_history <- dbGetQuery(pool, query)
index_history <- index_history %>% arrange(date)
index_history <- index_history[1:20, ]
row.names(index_history) <- index_history$date
index_history <- index_history[c("open", "high", "low", "close")]

