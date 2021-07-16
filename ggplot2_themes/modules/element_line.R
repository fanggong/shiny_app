
element_line_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  if (inherits(args, "arrow")) {
    arrow_angle <- args$arrow$angle
    arrow_length <- as.character(args$arrow$length)
    arrow_length_value <- stringr::str_match(arrow_length, "[0-9.]+")
    arrow_length_unit <- stringr::str_match(arrow_length, "[^0-9.]+")
    arrow_ends <- args$arrow$ends
    arrow_type <- args$arrow$type
  } else {
    arrow_angle <- 30
    arrow_length_value <- 0.25
    arrow_length_unit <- "inches"
    arrow_ends <- "last"
    arrow_type <- "open"
  }
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      shinyWidgets::prettyCheckbox(
        ns("set_to_blank"), label = strong("Assigns no space to the element"),
        value = inherits(args, "element_blank"),
        status = "primary", shape = "round"
      ),
      hr(),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("colour_type"), "Line Colour", choices = c("NA", "NULL", "Value"),
          selected = .get_attr_type(args$colour),
          justified = TRUE, width = "100%"
        )),
        column(6, colourpicker::colourInput(
          ns("colour"), label = br(), value = args$colour
        ))
      ),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("size_type"), "Line Size", choices = c("NA", "NULL", "Value"),
          selected = .get_attr_type(args$size), 
          justified = TRUE, width = "100%"
        )),
        column(6, shinyWidgets::numericInputIcon(
          ns("size"), label = br(), 
          value = args$size, 
          min = 0, step = 0.1,
          icon = list("mm")
        ))
      ),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("linetype_type"), "Line Type", choices = c("NA", "NULL", "Value"),
          selected = .get_attr_type(args$linetype),
          justified = TRUE, width = "100%"
        )),
        column(6, textInput(
          ns("linetype"), label = br(), value = args$linetype
        ))
      ),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("lineend_type"), "Line End Style", choices = c("NA", "NULL", "Value"),
          selected = .get_attr_type(args$lineend),
          justified = TRUE, width = "100%"
        )),
        column(6, selectInput(
          ns("lineend"), label = br(), choices = LINEEND, selected = args$lineend, 
        ))
      ),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("arrow_type"), "Arrow", choices = c("NA", "NULL", "Value"),
          selected = .get_attr_type(args$arrow),
          justified = TRUE, width = "100%"
        ))
      ),
      fluidRow(
        column(4, numericInput(
          ns("angle"), "angle", value = arrow_angle, step = 1
        )),
        column(4, numericInput(
          ns("value"), "length", value = as.numeric(arrow_length_value)
        )),
        column(4, selectInput(
          ns("unit"), label = br(), choices = UNITS, selected = arrow_length_unit
        ))
      ),
      fluidRow(
        column(4, selectInput(
          ns("ends"), "ends", choices = ARROW_ENDS, selected = arrow_ends
        )),
        column(4, selectInput(
          ns("type"), "type", choices = ARROW_TYPE, selected = arrow_type
        ))
      ),
      shinyWidgets::prettyCheckbox(
        ns("inherit.blank"), label = strong("inherit blank from parents"),
        value = args$inherit.blank,
        status = "primary", shape = "round"
      ),
      width = 4
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = "600px") %>% shinycssloaders::withSpinner(),
      verbatimTextOutput(ns("theme"), placeholder = TRUE)
    )
  )
}

element_line_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      attrs <- c("colour", "size", "linetype", "lineend")
      attrs_type <- paste0(attrs, "_type")
      append_attrs <- c("arrow_type", "angle", "value", "unit", "ends", "type")
      
      # enable and disable all the attrs and attrs_type
      observeEvent(input$set_to_blank, {
        if (input$set_to_blank) {
          for (each in c(attrs, attrs_type, append_attrs))  {
            shinyjs::disable(each)
          }
          shinyjs::disable("inherit.blank")
        } else {
          for (each in attrs_type)  {
            shinyjs::enable(each)
            if (input[[each]] == "Value") {
              shinyjs::enable(attrs[which(attrs_type == each)])
            }
          }
          shinyjs::enable(append_attrs[1])
          if (input[[append_attrs[1]]] == "Value") {
            for (each in append_attrs[-1]) {
              shinyjs::enable(each)
            }
          }
          shinyjs::enable("inherit.blank")
        }
      })
      
      # enable and disable all the attrs according to attrs_type
      lapply(1:length(attrs), function(idx) {
        observeEvent(input[[attrs_type[idx]]], {
          if (input[[attrs_type[idx]]] %in% c("NULL", "NA")) {
            shinyjs::disable(attrs[idx])
          } else {
            shinyjs::enable(attrs[idx])
          }
        })
      })
      observeEvent(input[[append_attrs[1]]], {
        if (input[[append_attrs[1]]] %in% c("NULL", "NA")) {
          for (each in append_attrs[-1]) {
            shinyjs::disable(each)
          }
        } else {
          for (each in append_attrs[-1]) {
            shinyjs::enable(each)
          }
        }
      })
      
      # reactive new theme
      new_theme[[id]] <- reactive({
        if (input$set_to_blank) {
          return(element_blank())
        }

        for (idx in 1:length(attrs)) {
          if (input[[attrs_type[idx]]] == "NULL") {
            assign(attrs[idx], NULL)
          } else if (input[[attrs_type[idx]]] == "NA") {
            assign(attrs[idx], NA)
          } else {
            assign(attrs[idx], input[[attrs[idx]]])
          }
        }

        if (!is.null(linetype)) {
          if (linetype %in% as.character(0:8)) {
            linetype <- as.numeric(linetype)
          }
        }
        
        result <- element_line(
          colour = colour,
          size = size,
          linetype = linetype,
          lineend = lineend,
          inherit.blank = input$inherit.blank
        )
        
        if (input$arrow_type == "NULL") {
          result["arrow"] <- list(NULL)
        } else if (input$arrow_type == "NA") {
          result["arrow"] <- list(FALSE)
        } else {
          result["arrow"] <- list(arrow(
            angle = input$angle, length = unit(input$value, input$unit),
            ends = input$ends, type = input$type
          ))
        }
        
        result
      })
      
      output$theme <- renderPrint({
        .reactiveValues_to_theme(new_theme)
        # arrow(
        #   angle = input$angle, length = unit(input$value, input$unit),
        #   ends = input$ends, type = input$type
        # )
      })
      
      output$plot <- renderCachedPlot({
        plot + .reactiveValues_to_theme(new_theme)
      }, cacheKeyExpr = .reactiveValues_to_theme(new_theme))
    }
  )
} 