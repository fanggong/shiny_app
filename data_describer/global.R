library(shiny)
library(fresh)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

library(openxlsx)
library(data.table)
library(tools)
library(infotheo)

library(plotly)
library(DT)
library(gt)

source("./utils/utils.R")
source("./setting/colors.R")

options(shiny.reactlog = TRUE)

VAR_TYPE <- c("numeric", "factor", "ordered")

GROUPINT_VAR_TYPE <- c("factor", "ordered")

ANALYZING_VAR_TYPE <- c("numeric", "factor", "ordered")

COR_METHOD <- c(
  "Pearson correlation coefficient",
  "Kendall correlation coefficient",
  "Spearman correlation coefficient",
  "Mutual information"
)
