element_margin_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  value <- stringr::str_match(as.character(args), "[0-9.]+")
  unit <- stringr::str_match(as.character(args), "[^0-9.]+")
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      shinyWidgets::radioGroupButtons(
        ns("margin_type"), 
        label = "Margin", 
        choices = .types(),
        selected = .get_attr_type(args), 
        justified = TRUE,
        width = "100%"
      ),
      fluidRow(
        column(4, numericInput(
          ns("value_top"), 
          label = "top", 
          value = .set_default(value[1], 0)
        )),
        column(5, selectInput(
          ns("unit_top"), 
          label = br(), 
          choices = UNITS, 
          selected = .set_default(unit[1], "points"),
        ))
      ),
      fluidRow(
        column(4, numericInput(
          ns("value_bottom"), 
          label = "bottom", 
          value = .set_default(value[3], 0)
        )),
        column(5, selectInput(
          ns("unit_bottom"), 
          label = br(), 
          choices = UNITS, 
          selected = .set_default(unit[3], "points"),
        ))
      ),
      fluidRow(
        column(4, numericInput(
          ns("value_left"), 
          label = "left", 
          value = .set_default(value[4], 0)
        )),
        column(5, selectInput(
          ns("unit_left"), 
          label = br(), 
          choices = UNITS, 
          selected = .set_default(unit[4], "points"),
        ))
      ),
      fluidRow(
        column(4, numericInput(
          ns("value_right"), 
          label = "right", 
          value = .set_default(value[2], 0)
        )),
        column(5, selectInput(
          ns("unit_right"), 
          label = br(),
          choices = UNITS,
          selected = .set_default(unit[2], "points"),
        ))
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = HEIGHT) %>% 
        shinycssloaders::withSpinner()
    )
  )
}

element_margin_server <- function(id, graph) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      attrs <- list(
        "margin_type" = c("value_top", "unit_top", "value_bottom", "unit_bottom",
                          "value_left", "unit_left", "value_right", "unit_right")
      )
      
      mapply(.toggle_controler, names(attrs), attrs, list(input = input))
      
      new_theme[[id]] <- reactive({
        .assign(names(attrs), input)
        element_margin(margin = margin)
      })
      
      output$plot <- renderCachedPlot({
        .get_plot(graph)
      }, cacheKeyExpr = .cache_key(graph))
      
    }
  )
}