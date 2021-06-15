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

source("./utils/utils.R")
source("./setting/colors.R")

options(shiny.reactlog = TRUE)

TEST_MODE <- TRUE

VAR_TYPE <- c("Numerical", "Categorical", "Ordinal")

COR_METHOD <- c(
  "Pearson correlation coefficient",
  "Kendall correlation coefficient",
  "Spearman correlation coefficient",
  "Chi-squared test",
  "Mutual information"
)