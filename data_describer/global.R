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

library(nycflights13)
library(car)

source("./utils/utils.R")
source("./setting/colors.R")
source("./utils/stat_utils.R")

options(shiny.reactlog = TRUE)

VAR_TYPE <- c("numeric", "factor", "ordered")

GROUPINT_VAR_TYPE <- c("factor")

ANALYZING_VAR_TYPE <- c("numeric", "factor", "ordered")

COR_METHOD <- c(
  "Pearson correlation coefficient",
  "Kendall correlation coefficient",
  "Spearman correlation coefficient",
  "Mutual information"
)

ALL_HEIGHT <- 700


set.seed(2414)
flights <- flights[sample(nrow(flights), 888), ]
setDT(flights)
sample_dat <- flights[
  , .(
    DepartureTime = as.numeric(dep_time),
    ArrivalTime = as.numeric(arr_time),
    DepatureDelay = dep_delay,
    ArrivalDelay = arr_delay,
    Carrier = factor(carrier),
    Origin = factor(origin),
    Destination = factor(dest),
    Distance = distance,
    Time = time_hour,
    Add = factor(sample(c("JXWY", "SCCD"), 888, replace = TRUE))
  )
]
