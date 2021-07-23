LINEEND <- c("round", "butt", "square")
ARROW_ENDS <- c("first", "last", "both")
ARROW_TYPE <- c("open", "closed")

element_line_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  if (!inherits(args$arrow, "arrow")) {
    arrow <- arrow()
  } else {
    arrow <- args$arrow
  }
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      shinyWidgets::materialSwitch(
        ns("set_to_blank"), 
        label = strong("Assigns no space to the element"),
        value = inherits(args, "element_blank"),
        status = "primary", 
        right = TRUE
      ),
      hr(),
      # colour ----
      shinyWidgets::radioGroupButtons(
        ns("colour_type"), 
        label = "Line Colour", 
        choices = .types(),
        selected = .get_attr_type(args$colour),
        justified = TRUE, 
        width = "100%"
      ),
      colourpicker::colourInput(
        ns("colour"),
        label = NULL, 
        value = .set_default(args$colour, "#000000")
      ),
      hr(),
      # size ----
      shinyWidgets::radioGroupButtons(
        ns("size_type"), 
        label = "Line Size", 
        choices = .types(),
        selected = .get_attr_type(args$size), 
        justified = TRUE, 
        width = "100%"
      ),
      fluidRow(
        column(8, selectInput(
          ns("size_unit"),
          label = NULL,
          choices = c(
            "Relative Value" = "rel",
            "Absolute Value in mm" = "identity"
          ),
          selected = ifelse(inherits(args$size, "rel"), "rel", "identity")
        )),
        column(4, numericInput(
          ns("size_value"), 
          label = NULL, 
          value = .set_default(args$size, 1), 
          min = 0, 
          step = 0.1,
        ))
      ),
      hr(),
      # linetype ----
      shinyWidgets::radioGroupButtons(
        ns("linetype_type"), 
        label = "Line Type", 
        choices = .types(),
        selected = .get_attr_type(args$linetype),
        justified = TRUE, 
        width = "100%"
      ),
      textInput(
        ns("linetype"), 
        label = NULL,
        value = .set_default(args$linetype, 0)
      ),
      hr(),
      # lineend ----
      shinyWidgets::radioGroupButtons(
        ns("lineend_type"), 
        label = "Line End Style", 
        choices = .types(),
        selected = .get_attr_type(args$lineend),
        justified = TRUE, 
        width = "100%"
      ),
      selectInput(
        ns("lineend"), 
        label = NULL, 
        choices = LINEEND, 
        selected = .set_default(args$lineend, LINEEND[1]), 
      ),
      hr(),
      # arrow ----
      shinyWidgets::radioGroupButtons(
        ns("arrow_type"), 
        label = "Arrow", 
        choices = .types(),
        selected = .get_attr_type(args$arrow),
        justified = TRUE,
        width = "100%"
      ),
      fluidRow(
        column(4, numericInput(
          ns("angle"), 
          label = "angle", 
          value = arrow$angle,
          step = 1
        )),
        column(4, selectInput(
          ns("ends"), 
          label = "ends", 
          choices = ARROW_ENDS, 
          selected = arrow$ends
        )),
        column(4, selectInput(
          ns("type"), 
          label = "type", 
          choices = ARROW_TYPE, 
          selected = arrow$type
        ))
      ),
      fluidRow(
        column(4, numericInput(
          ns("value"), 
          label = "length", 
          value = as.numeric(stringr::str_match(as.character(arrow$length), "[0-9.]+"))
        )),
        column(4, selectInput(
          ns("unit"), 
          label = br(), 
          choices = UNITS, 
          selected = stringr::str_match(as.character(arrow$length), "[^0-9.]+")
        ))
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = HEIGHT) %>% 
        shinycssloaders::withSpinner()
    )
  )
}

element_line_server <- function(id, graph) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      attrs <- list(
        "colour_type" = "colour",
        "size_type" = c("size_unit", "size_value"),
        "linetype_type" = "linetype",
        "lineend_type" = "lineend",
        "arrow_type" = c("angle", "ends", "type", "value", "unit")
      )
      
      observeEvent(input$set_to_blank, {
        if (input$set_to_blank) {
          for (each in c(names(attrs), unlist(attrs)))  {
            shinyjs::disable(each)
          }
        } else {
          for (controler in names(attrs)) {
            shinyjs::enable(controler)
            if (input[[controler]] == .types(3)) {
              for (element in attrs[[controler]]) {
                shinyjs::enable(element)
              }
            }
          }
        }
      })
      
      mapply(.toggle_controler, names(attrs), attrs, list(input = input))
      
      new_theme[[id]] <- reactive({
        if (input$set_to_blank) {
          return(element_blank())
        }
        
        .assign(names(attrs), input)
        
        element_line(
          colour = colour,
          size = size,
          linetype = linetype,
          lineend = lineend,
          arrow = arrow
        )
      })
      
      output$theme <- renderPrint({
        .reactiveValues_to_theme(new_theme)
      })
      
      output$plot <- renderCachedPlot({
        .get_plot(graph)
      }, cacheKeyExpr = .cache_key(graph))
    }
  )
} 