
element_rect_ui <- function(id) {
  ns <- NS(id)
  
  args <- theme_init[[id]]
  
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
      # fill ----
      shinyWidgets::radioGroupButtons(
        ns("fill_type"), 
        label = "Fill Colour", 
        choices = .types(),
        selected = .get_attr_type(args$fill),
        justified = TRUE, 
        width = "100%"
      ),
      colourpicker::colourInput(
        ns("fill"), 
        label = NULL, 
        value = .set_default(args$fill, "#000000")
      ),
      hr(),
      # colour ----
      shinyWidgets::radioGroupButtons(
        ns("colour_type"), 
        label = "Border Colour", 
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
        label = "Border Size", 
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
        label = "Border LineType", 
        choices = .types(),
        selected = .get_attr_type(args$linetype),
        justified = TRUE, 
        width = "100%"
      ),
      textInput(
        ns("linetype"), 
        label = NULL, 
        value = .set_default(args$linetype, 0)
      )
    ),
    mainPanel = mainPanel(
      plotOutput(ns("plot"), height = HEIGHT) %>% 
        shinycssloaders::withSpinner()
    )
  )
}

element_rect_server <- function(id, graph) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      attrs <- list(
        "fill_type" = "fill",
        "colour_type" = "colour",
        "size_type" = "size",
        "linetype_type" = "linetype"
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
        
        element_rect(
          fill = fill,
          colour = colour,
          size = size,
          linetype = linetype
        )
      })
      
      output$plot <- renderCachedPlot({
        .get_plot(graph)
      }, cacheKeyExpr = .cache_key(graph))
    }
  )
} 