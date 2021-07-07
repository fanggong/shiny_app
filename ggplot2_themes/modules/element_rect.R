
element_rect_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
  choices_null_attrs <- c("fill", "colour", "size", "linetype")
  selected_null_attrs <- NULL
  for (attr in choices_null_attrs) {
    if (is.null(args[[attr]])) {
      selected_null_attrs <- c(selected_null_attrs, attr)
    } 
    # else if (is.na(args[[attr]])) {
    #   selected_null_attrs <- c(selected_null_attrs, attr)
    # }
  }
  names(choices_null_attrs) <- str_to_title(choices_null_attrs)

  sidebarLayout(
    sidebarPanel = sidebarPanel(
      shinyWidgets::prettyCheckbox(
        ns("set_to_blank"), label = strong("Assigns no space (Set to element_blank)"),
        value = inherits(args, "element_blank"), status = "primary", shape = "round"
      ),
      br(),
      shinyWidgets::checkboxGroupButtons(
        ns("null_attrs"), label = "Select the attribute(s) set to NULL:",
        choices = choices_null_attrs, selected = selected_null_attrs, 
        individual = TRUE, justified = TRUE, size = "sm",
        checkIcon = list(yes = icon("circle", style = "color: steelblue"),
                         no = icon("circle-o", style = "color: steelblue"))
      ),
      colourpicker::colourInput(ns("fill"), label = "Fill", value = args$fill),
      colourpicker::colourInput(ns("colour"), label = "Colour", value = args$colour),
      shinyWidgets::numericInputIcon(
        ns("size"), label = "Size", value = args$size, min = 0, step = 0.1,
        icon = list("mm")
      ),
      textInput(ns("linetype"), "LineType", value = args$linetype),
      shinyWidgets::prettyCheckbox(
        ns("inherit.blank"), label = strong("inherit blank from parents"),
        value = args$inherit.blank, status = "primary", shape = "round"
      ),
      width = 4
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot")) %>% shinycssloaders::withSpinner(),
      verbatimTextOutput(ns("theme"), placeholder = TRUE)
    )
  )
}

element_rect_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {

      attrs <- c("fill", "colour", "size", "linetype")
      
      # 这两个observe有点蠢
      observeEvent(input$set_to_blank, {
        if (input$set_to_blank) {
          shinyjs::disable("null_attrs")
          for (each in attrs) {
            shinyjs::disable(each)
          }
          shinyjs::disable("inherit.blank")
        } else {
          shinyjs::enable("null_attrs")
          for (each in attrs) {
            if (each %in% input$null_attrs) {
              shinyjs::disable(each)
            } else {
              shinyjs::enable(each)
            }
          }
          shinyjs::enable("inherit.blank")
        }
      })
      
      observe({
        for (each in attrs) {
          if (each %in% input$null_attrs) {
            shinyjs::disable(each)
          } else {
            shinyjs::enable(each)
          }
        }
      })
      
      new_theme[[id]] <- reactive({
        if (input$set_to_blank) {
          return(element_blank())
        }
        
        for (each in attrs) {
          if (each %in% input$null_attrs) {
            assign(each, NULL)
          } else {
            assign(each, input[[each]])
          }
        }
        
        if (!is.null(linetype)) {
          if (linetype %in% as.character(0:8)) {
            linetype <- as.numeric(linetype)
          }
        }
        
        element_rect(
          fill = fill,
          colour = colour,
          size = size,
          linetype = linetype,
          inherit.blank = input$inherit.blank
        )
      })
      
      output$theme <- renderPrint({
        .reactiveValues_to_theme(new_theme)
      })
      
      output$plot <- renderCachedPlot({
        ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
          geom_point(aes(color = Species)) +
          .reactiveValues_to_theme(new_theme)
      }, cacheKeyExpr = .reactiveValues_to_theme(new_theme))
    }
  )
} 