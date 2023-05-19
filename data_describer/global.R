library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(openxlsx)
library(data.table)
library(sparkline)
library(formattable)
library(htmlwidgets)
library(dplyr)
library(shinycssloaders)
library(stringr)
library(echarts4r)
library(htmltools)
library(fresh)
library(DT)
library(plotly)
library(shinyAce)
library(infotheo)

source("./utils/utils.R")
source("./setting/colors.R")

options(shiny.reactlog = TRUE)

VAR_TYPE <- c(
  "numeric", "factor", "ordered"
)

COR_METHOD <- c(
  "Pearson correlation coefficient",
  "Kendall correlation coefficient",
  "Spearman correlation coefficient",
  "Mutual information"
)
