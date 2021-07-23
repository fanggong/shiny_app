default_setting_ui <- function(id = "default_setting") {
  ns <- NS(id) 
  DEFAULT_SETTINGS <- c(
    "Title" = "title", 
    "Subtitle" = "subtitle", 
    "Tag" = "tag",
    "Caption" = "caption"
  )
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      h6("Select the elements to show in default sample Graph:"),
      hr(),
      shinyWidgets::multiInput(
        ns("text_elements"),
        label = "Text Elements",
        choices = DEFAULT_SETTINGS,
        selected = DEFAULT_SETTINGS,
        options = list(enable_search = FALSE)
      ),
      shinyWidgets::radioGroupButtons(
        ns("legend"),
        label = "Number of Legends",
        choices = c(
          "None" = "none", 
          "Single" = "single", 
          "Multiple" = "multiple"
        ),
        selected = "multiple",
        justified = TRUE
      ),
      shinyWidgets::radioGroupButtons(
        ns("facet"),
        label = "Facet Type",
        choices = c(
          "No Facet" = "none",
          "Grid" = "grid",
          "Wrap" = "wrap"
        ),
        selected = "grid",
        justified = TRUE
      ),
      shinyWidgets::radioGroupButtons(
        ns("dimension"), 
        label = NULL, 
        choices = c(
          "Both" = "both",
          "Row" = "row", 
          "Col" = "col"
        ),
        selected = "both",
        justified = TRUE
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = HEIGHT) %>% 
        shinycssloaders::withSpinner()
    )
  )
}

default_setting_server <- function(id = "default_setting") {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$facet, {
        observeEvent(input$dimension, {
          if (input$facet == "none") {
            default_plot$facet <- NULL
          } else if (input$facet == "grid") {
            if ("both" %in% input$dimension) {
              default_plot$facet <- facet_grid(rows = vars(facet_x), cols = vars(facet_y))
            } else if ("row" %in% input$dimension) {
              default_plot$facet <- facet_grid(rows = vars(facet_x))
            } else if ("col" %in% input$dimension) {
              default_plot$facet <- facet_grid(cols = vars(facet_y))
            } else {
              default_plot$facet <- NULL
            }
          } else {
            if ("both" %in% input$dimension) {
              default_plot$facet <- facet_wrap(vars(facet_x, facet_y), nrow = 2)
            } else if ("row" %in% input$dimension) {
              default_plot$facet <- facet_wrap(vars(facet_x, facet_y), nrow = 4)
            } else if ("col" %in% input$dimension) {
              default_plot$facet <- facet_wrap(vars(facet_x, facet_y), nrow = 1)
            }
          }
        })
      })
      
      observeEvent(c(input$text_elements, "placeholder"), {
        if ("title" %in% input$text_elements) {
          default_plot$title <- labs(title = "This is a title placeholder")
        } else {
          default_plot$title <- NULL
        }
        
        if ("subtitle" %in% input$text_elements) {
          default_plot$subtitle <- labs(subtitle = "This is a subtitle placeholder")
        } else {
          default_plot$subtitle <- NULL
        }
        
        if ("tag" %in% input$text_elements) {
          default_plot$tag <- labs(tag = "This is a tag placeholder")
        } else {
          default_plot$tag <- NULL
        }
        
        if ("caption" %in% input$text_elements) {
          default_plot$caption <- labs(caption = "This is a caption placeholder")
        } else {
          default_plot$caption <- NULL
        }
      })
      
      observeEvent(input$legend, {
        if (input$legend == "none") {
          default_plot$legend <- geom_point()
        } else if (input$legend == "single") {
          default_plot$legend <- geom_point(aes(color = color))
        } else (
          default_plot$legend <- geom_point(aes(shape = shape, color = color))
        )
      })
      
      output$plot <- renderCachedPlot({
        .get_plot(function() "default")
      }, cacheKeyExpr = .cache_key())
    }
  )
}
