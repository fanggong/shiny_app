POSITION <- c("none", "left", "right", "bottom", "top")

element_position_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  position <- args
  coord_str <- args
  coord_num <- args
  if (.get_attr_type(args) == .types(3)) {
    if (length(args) == 1) {
      position <- "string"
      coord_num <- NULL
    } else {
      position <- "numeric"
      coord_str <- NULL
    }
  }
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      shinyWidgets::radioGroupButtons(
        ns("position_type"), 
        label = "Position", 
        choices = .types(),
        selected = .get_attr_type(args),
        justified = TRUE, 
        width = "100%"
      ),
      selectInput(
        ns("position"), 
        label = NULL, 
        choices = c("use one-element string" = "string", 
                    "use two-element numeric" = "numeric"),
        selected = .set_default(position, "string"), 
        width = "100%"
      ),
      conditionalPanel(
        condition = "input.position == 'string'", ns = ns,
        selectInput(
          ns("position_xy"),
          label = NULL,
          choices = POSITION,
          selected = .set_default(coord_str, POSITION[1])
        )
      ),
      conditionalPanel(
        condition = "input.position == 'numeric'", ns = ns,
        fluidRow(
          column(6, numericInput(
            ns("position_x"),
            label = "horizontal",
            value = .set_default(coord_num[1], 0.5)
          )),
          column(6, numericInput(
            ns("position_y"),
            label = "vertical",
            value = .set_default(coord_num[2], 0.5)
          ))
        )
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = HEIGHT) %>% 
        shinycssloaders::withSpinner()
    )
  )
}

element_position_server <- function(id, graph) {
  moduleServer(
    id,
    function(input, output, session) {
      
      attrs <- list(
        "position_type" = c("position", "position_x", "position_y", "position_xy")
      )
      
      mapply(.toggle_controler, names(attrs), attrs, list(input = input))
      
      new_theme[[id]] <- reactive({
        .assign(names(attrs), input)
        element_position(position = position)
      })
      
      output$plot <- renderCachedPlot({
        .get_plot(graph)
      }, cacheKeyExpr = .cache_key(graph))
    }
  )
}