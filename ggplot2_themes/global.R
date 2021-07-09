library(shiny)
library(ggplot2)
library(yaml)
library(colourpicker)
library(shinyWidgets)
library(stringr)
library(shinyjs)
library(shinycssloaders)

source("modules/element_rect.R")
source("modules/element_unit.R")
source("utils/elements.R")
source("utils/helpers.R")
source("init.R")

# theme <- theme_get()
# theme$rect$size <- 12
# theme$rect$colour <- "black"
# theme$rect$linetype <- 2
# 
# theme$panel.background$colour <- NULL
# theme$panel.background$size <- NA
# 
# theme$axis.ticks.length.x <- unit(10, "points")
# ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#   geom_point(aes(color = Species)) +
#   theme
