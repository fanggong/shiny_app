library(shiny)
library(shinyAce)
library(ggplot2)

# data
set.seed(2414)
dat <- data.frame(
  long = rnorm(100), 
  lat = rnorm(100),
  facet_x = sample(c("East", "West"), 100, replace = TRUE),
  facet_y = sample(c("Sourth", "North"), 100, replace = TRUE),
  shape = sample(c("ShapeX", "ShapeY", "ShapeZ"), 100, replace = TRUE),
  color = sample(c("SizeX", "SizeY", "SizeZ"), 100, replace = TRUE)
)

# init theme
theme <- theme_get()

# generate config file
# element / call what

# for each in names(theme)
# if isnull then find "call what" and init
# if not null then do nothing

source("module/element_rect.R")




