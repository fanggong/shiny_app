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
theme$title <- element_text()

plt + theme

# save(theme, file = "theme.Rdata")

# load("theme.Rdata")

if (interactive()) {
  ui <- fluidPage(
    uiOutput("x_1"),
    uiOutput("x_2"),
    uiOutput("x_3")
  )
  server <- function(input, output, session) {
    for (i in 1:3) {
      output[[paste0("x_", i)]] <- renderUI({
        textInput(paste0("x_", i), label = paste0("x_", i), value = i)
      })
    }
  }
  shinyApp(ui, server)
}






