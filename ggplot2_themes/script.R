set.seed(2414)
dat <- data.frame(
  long = rnorm(100), 
  lat = rnorm(100),
  facet_x = sample(c("East", "West"), 100, replace = TRUE),
  facet_y = sample(c("Sourth", "North"), 100, replace = TRUE),
  shape = sample(c("ShapeX", "ShapeY", "ShapeZ"), 100, replace = TRUE),
  color = sample(c("SizeX", "SizeY", "SizeZ"), 100, replace = TRUE)
)
plt <- ggplot(dat) + 
  geom_point(aes(x = long, y = lat)) +
  # geom_point(aes(x = long, y = lat, shape = shape, color = color)) +
  # labs(title = "THIS IS TITLE") + 
  # labs(subtitle = "THIS IS SUBTITLE") +
  # labs(caption = "THIS IS CAPTION") +
  # labs(tag = "THIS IS TAG") +
  facet_grid(rows = vars(facet_y), cols = vars(facet_x))
plt

theme <- theme_get()
theme$title <- element_text(family = 'Kai')

plt + theme

# save(theme, file = "theme.Rdata")
# 
# 
# load("theme.Rdata")

