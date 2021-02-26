library(pool)
library(DT)
library(jsonlite)
library(RPostgreSQL)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(networkD3)
library(leaflet)
library(dygraphs)
library(shinyAce)
library(dplyr)
library(shinyjs)
library(threejs)
library(dashboardthemes)

source("module/notebook.R", encoding = "UTF-8")

db_info <- jsonlite::read_json("config.json")$postgres
pool <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  dbname = db_info$dbname,
  host = db_info$host,
  user = db_info$user,
  password = db_info$password
)


query <- "select date, open, high, low, close from test"
dat <- dbGetQuery(pool, query)
row.names(dat) <- dat$date
dat <- dat[c("open", "high", "low", "close")]

