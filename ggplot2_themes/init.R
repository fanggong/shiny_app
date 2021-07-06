# initialize default data
set.seed(2414)
dat <- data.frame(
  long = rnorm(100), 
  lat = rnorm(100),
  facet_x = sample(c("East", "West"), 100, replace = TRUE),
  facet_y = sample(c("Sourth", "North"), 100, replace = TRUE),
  shape = sample(c("ShapeX", "ShapeY", "ShapeZ"), 100, replace = TRUE),
  color = sample(c("SizeX", "SizeY", "SizeZ"), 100, replace = TRUE)
)


# initialize theme
ele_config <- yaml::read_yaml("ele_config.yaml")
theme_init <- theme_get()
new_theme <- reactiveValues()

for (ele in names(theme_init)) {
  if (is.null(theme_init[[ele]])) {
    theme_init[ele] <- list(do.call(ele_config[[ele]], list()))
  }
  new_theme[[ele]] <- theme_init[[ele]]
}
rm(ele)


