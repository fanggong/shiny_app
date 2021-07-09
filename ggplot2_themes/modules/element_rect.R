
element_rect_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
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
          ns("fill_type"), "Fill Colour", choices = c("NA", "NULL", "Value"),
          selected = .get_attr_type(args$fill),
          justified = TRUE, width = "100%"
        )),
        column(6, colourpicker::colourInput(
          ns("fill"), label = br(), value = args$fill
        ))
      ),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("colour_type"), "Border Colour", choices = c("NA", "NULL", "Value"),
          selected = .get_attr_type(args$colour),
          justified = TRUE, width = "100%"
        )),
        column(6, colourpicker::colourInput(
          ns("colour"), label = br(), value = args$colour
        ))
      ),
      fluidRow(
        column(6, shinyWidgets::radioGroupButtons(
          ns("size_type"), "Border Size", choices = c("NA", "NULL", "Value"),
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
          ns("linetype_type"), "Border LineType", choices = c("NA", "NULL", "Value"),
          selected = .get_attr_type(args$linetype),
          justified = TRUE, width = "100%"
        )),
        column(6, textInput(
          ns("linetype"), label = br(), value = args$linetype
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

element_rect_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      attrs <- c("fill", "colour", "size", "linetype")
      attrs_type <- paste0(attrs, "_type")
    
      # enable and disable all the attrs and attrs_type
      observeEvent(input$set_to_blank, {
        if (input$set_to_blank) {
          for (each in c(attrs, attrs_type))  {
            shinyjs::disable(each)
          }
          shinyjs::disable("inherit.blank")
        } else {
          # TODO
          for (each in attrs_type)  {
            shinyjs::enable(each)
            if (input[[each]] == "Value") {
              shinyjs::enable(attrs[which(attrs_type == each)])
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
        
        element_rect(
          fill = fill,
          colour = colour,
          size = size,
          linetype = linetype,
          inherit.blank = input$inherit.blank
        )
      })
      
      output$plot <- renderCachedPlot({
        plot + .reactiveValues_to_theme(new_theme)
      }, cacheKeyExpr = .reactiveValues_to_theme(new_theme))
    }
  )
} 