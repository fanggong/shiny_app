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
THEME <- theme_get()

for (ele in names(THEME)) {
  if (is.null(THEME[[ele]])) {
    THEME[ele] <- list(do.call(ele_config[[ele]], list()))
  }
}
rm(ele)