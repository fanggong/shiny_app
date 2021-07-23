FONT_FACE <- c("plain", "italic", "bold", "bold.italic")

element_text_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  value <- stringr::str_match(as.character(args$margin), "[0-9.]+")
  unit <- stringr::str_match(as.character(args$margin), "[^0-9.]+")
  
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
      # family ----
      shinyWidgets::radioGroupButtons(
        ns("family_type"), 
        label = "Font Family", 
        choices = .types(),
        selected = .get_attr_type(args$family),
        justified = TRUE, 
        width = "100%"
      ),
      textInput(
        ns("family"), 
        label = NULL, 
        value = .set_default(args$family, "")
      ),
      hr(),
      # face ----
      shinyWidgets::radioGroupButtons(
        ns("face_type"), 
        label = "Font Face", 
        choices = .types(),
        selected = .get_attr_type(args$face),
        justified = TRUE, 
        width = "100%"
      ),
      selectInput(
        ns("face"), 
        label = NULL, 
        choices = FONT_FACE, 
        selected = .set_default(args$face, FONT_FACE[1])
      ),
      hr(),
      # colour ----
      shinyWidgets::radioGroupButtons(
        ns("colour_type"), 
        label = "Text Colour", 
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
        label = "Text Size", 
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
            "Absolute Value in pts" = "identity"
          ),
          selected = ifelse(inherits(args$size, "rel"), "rel", "identity")
        )),
        column(4, numericInput(
          ns("size_value"), 
          label = NULL, 
          value = .set_default(args$size, 3), 
          min = 0, 
          step = 0.1,
        ))
      ),
      hr(),
      # hjust ----
      shinyWidgets::radioGroupButtons(
        ns("hjust_type"), 
        label = "Justification (H)", 
        choices = .types(),
        selected = .get_attr_type(args$hjust),
        justified = TRUE, 
        width = "100%"
      ),
      numericInput(
        ns("hjust"), 
        label = NULL, 
        value = .set_default(args$hjust, 0.5), 
        step = 0.1, 
        min = 0, 
        max = 1
      ),
      hr(),
      # vjust ----
      shinyWidgets::radioGroupButtons(
        ns("vjust_type"), 
        label = "Justification (V)", 
        choices = .types(),
        selected = .get_attr_type(args$vjust),
        justified = TRUE,
        width = "100%"
      ),
      numericInput(
        ns("vjust"), 
        label = NULL, 
        value = .set_default(args$vjust, 0.5), 
        step = 0.1, 
        min = 0, 
        max = 1
      ),
      hr(),
      # angle ----
      shinyWidgets::radioGroupButtons(
        ns("angle_type"), 
        label = "Angle", 
        choices = .types(),
        selected = .get_attr_type(args$angle),
        justified = TRUE,
        width = "100%"
      ),
      numericInput(
        ns("angle"), 
        label = NULL, 
        value = .set_default(args$angle, 0), 
        step = 1, 
        min = 0, 
        max = 360
      ),
      hr(),
      # lineheight ----
      shinyWidgets::radioGroupButtons(
        ns("lineheight_type"), 
        label = "Line Height", 
        choices = .types(),
        selected = .get_attr_type(args$lineheight),
        justified = TRUE,
        width = "100%"
      ),
      numericInput(
        ns("lineheight"), 
        label = NULL,
        value = .set_default(args$lineheight, 1), 
        step = 0.1, 
        min = 0
      ),
      hr(),
      # margin ----
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

element_text_server <- function(id, graph) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      attrs <- list(
        "family_type" = "family",
        "face_type" = "face",
        "colour_type" = "colour",
        "size_type" = c("size_unit", "size_value"),
        "hjust_type" = "hjust",
        "vjust_type" = "vjust",
        "angle_type" = "angle",
        "lineheight_type" = "lineheight", 
        "margin_type" = c("value_top", "unit_top", "value_bottom", "unit_bottom",
                          "value_left", "unit_left", "value_right", "unit_right")
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
        
        element_text(
          family = family,
          face = face,
          colour = colour,
          size = size,
          hjust = hjust,
          vjust = vjust,
          angle = angle,
          lineheight = lineheight,
          margin = margin
        )
      })
      
      output$plot <- renderCachedPlot({
        .get_plot(graph)
      }, cacheKeyExpr = .cache_key(graph))
    }
  )
} 