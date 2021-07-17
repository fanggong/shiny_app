element_text_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  if (!is.null(args$margin)) {
    margin <- as.character(args$margin)
    value <- stringr::str_match(margin, "[0-9.]+")
    unit <- stringr::str_match(margin, "[^0-9.]+")
  } else {
    value <- rep(0, 4)
    unit <- rep("points", 4)
  }
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      shinyWidgets::materialSwitch(
        ns("set_to_blank"), label = strong("Assigns no space to the element"),
        value = inherits(args, "element_blank"),
        status = "primary", right = TRUE
      ),
      hr(),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("family_type"), "Font Family", choices = c("NA", "NULL", "Value"),
          selected = .get_attr_type(args$family),
          justified = TRUE, width = "100%"
        )),
        column(6, textInput(
          ns("family"), label = br(), value = args$family
        ))
      ),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("face_type"), "Font Face", choices = c("NULL", "Value"),
          selected = .get_attr_type(args$face),
          justified = TRUE, width = "100%"
        )),
        column(6, selectInput(
          ns("face"), label = br(), choices = FONT_FACE, selected = args$face
        ))
      ),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("colour_type"), "Text Colour", choices = c("NA", "NULL", "Value"),
          selected = .get_attr_type(args$colour),
          justified = TRUE, width = "100%"
        )),
        column(6, colourpicker::colourInput(
          ns("colour"), label = br(), value = args$colour
        ))
      ),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("size_type"), "Text Size", choices = c("NULL", "Value"),
          selected = .get_attr_type(args$size), 
          justified = TRUE, width = "100%"
        )),
        column(6, shinyWidgets::numericInputIcon(
          ns("size"), label = br(), 
          value = args$size, 
          min = 0, step = 0.1,
          icon = list("pts")
        ))
      ),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("hjust_type"), "Justification (H)", choices = c("NA", "NULL", "Value"),
          selected = .get_attr_type(args$hjust),
          justified = TRUE, width = "100%"
        )),
        column(6, numericInput(
          ns("hjust"), label = br(), value = args$hjust, step = 0.1, min = 0, max = 1
        ))
      ),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("vjust_type"), "Justification (V)", choices = c("NA", "NULL", "Value"),
          selected = .get_attr_type(args$vjust),
          justified = TRUE, width = "100%"
        )),
        column(6, numericInput(
          ns("vjust"), label = br(), value = args$vjust, step = 0.1, min = 0, max = 1
        ))
      ),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("angle_type"), "Angle", choices = c("NA", "NULL", "Value"),
          selected = .get_attr_type(args$angle),
          justified = TRUE, width = "100%"
        )),
        column(6, numericInput(
          ns("angle"), label = br(), value = args$angle, step = 1, min = 0, max = 360
        ))
      ),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("lineheight_type"), "Line Height", choices = c("NA", "NULL", "Value"),
          selected = .get_attr_type(args$lineheight),
          justified = TRUE, width = "100%"
        )),
        column(6, numericInput(
          ns("lineheight"), label = br(), value = args$lineheight, step = 0.1, min = 0
        ))
      ),
      fluidRow(
        column(7, shinyWidgets::radioGroupButtons(
          ns("margin_type"), "Margin", choices = c("NULL", "Value"),
          selected = .get_attr_type(args$margin), justified = TRUE, width = "100%"
        )),
      ),
      fluidRow(
        column(4, numericInput(
          ns("value_top"), label = "top", value = as.numeric(value[1])
        )),
        column(5, selectInput(
          ns("unit_top"), label = br(), choices = UNITS, selected = unit[1], width = "100%"
        ))
      ),
      fluidRow(
        column(4, numericInput(
          ns("value_bottom"), label = "bottom", value = as.numeric(value[3])
        )),
        column(5, selectInput(
          ns("unit_bottom"), label = br(), choices = UNITS, selected = unit[3]
        ))
      ),
      fluidRow(
        column(4, numericInput(
          ns("value_left"), label = "left", value = as.numeric(value[4])
        )),
        column(5, selectInput(
          ns("unit_left"), label = br(), choices = UNITS, selected = unit[4]
        ))
      ),
      fluidRow(
        column(4, numericInput(
          ns("value_right"), label = "right", value = as.numeric(value[2])
        )),
        column(5, selectInput(
          ns("unit_right"), label = br(), choices = UNITS, selected = unit[2]
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

element_text_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      attrs <- c("family", "face", "colour", "size", "hjust", "vjust", "angle", "lineheight")
      attrs_type <- paste0(attrs, "_type")
      append_attrs <- c("margin_type", "value_top", "unit_top", 
                        "value_bottom", "unit_bottom", "value_left",
                        "unit_left", "value_right", "unit_right")
      
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
      
        
        result <- element_text(
          family = family,
          face = face,
          colour = colour,
          size = size,
          hjust = hjust,
          vjust = vjust,
          angle = angle,
          lineheight = lineheight,
          inherit.blank = input$inherit.blank
        )
        
        if (input$margin_type == "NULL") {
          result["margin_type"] <- list(NULL)
        } else if (input$margin_type == "NA") {
          result["margin_type"] <- list(FALSE)
        } else {
          result["margin_type"] <- list(margin(
            t = input$value_top, r = input$value_right,
            b = input$value_bottom, l = input$value_left,
            unit = c(input$unit_top, input$unit_right, input$unit_bottom, input$unit_left)
          ))
        }
        
        result
      })
      
      output$theme <- renderPrint({
        .reactiveValues_to_theme(new_theme)
      })
      
      output$plot <- renderCachedPlot({
        plot + .reactiveValues_to_theme(new_theme)
      }, cacheKeyExpr = .reactiveValues_to_theme(new_theme))
    }
  )
} 