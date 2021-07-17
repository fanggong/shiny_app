# initialize default data
set.seed(2414)
dat_src <- data.frame(
  long = rnorm(100), 
  lat = rnorm(100),
  facet_x = sample(c("East", "West"), 100, replace = TRUE),
  facet_y = sample(c("Sourth", "North"), 100, replace = TRUE),
  shape = sample(c("ShapeX", "ShapeY", "ShapeZ"), 100, replace = TRUE),
  color = sample(c("SizeX", "SizeY", "SizeZ"), 100, replace = TRUE)
)

# initialize default theme
plot <- ggplot(dat_src, aes(long, lat)) + 
  geom_point(aes(shape = shape, color = color)) +
  facet_grid(rows = vars(facet_x), cols = vars(facet_y)) + 
  labs(title = "This is a sample plot") + 
  labs(subtitle = "using a sample data") +
  labs(tag = "make some tags here") + 
  labs(caption = "make some captions here")

# initialize theme
ele_config <- yaml::read_yaml("ele_config.yaml")
theme_init <- theme_get()
ELEMENTS <- names(theme_init)
new_theme <- reactiveValues()
for (ele in names(theme_init)) {
  if (is.null(theme_init[[ele]])) {
    theme_init[ele] <- list(do.call(ele_config[[ele]], list()))
  }
  new_theme[[ele]] <- theme_init[[ele]]
}
rm(ele)

# initialize units
UNITS <- c("npc", "cm", "inches", "mm", "points", "picas", "bigpts", "dida",
           "cicero", "scaledpts", "lines", "char", "native", "snpc", "strwidth",
           "strheight", "grobwidth", "grobheight")

# initialize tag position
TAG_POS <- c("topleft", "top", "topright", "left", "right", "bottomleft", "bottom")

TITLE_POS <- c("panel", "plot")

DIRECTION <- c("horizontal", "vertical")

PLACEMENT <- c("inside", "outside")

JUST <- c("top", "bottom", "left", "right")

POSITION <- c("none", "left", "right", "bottom", "top")

LINEEND <- c("round", "butt", "square")

ARROW_ENDS <- c("first", "last", "both")

ARROW_TYPE <- c("open", "closed")

FONT_FACE <- c("plain", "italic", "bold", "bold.italic")
