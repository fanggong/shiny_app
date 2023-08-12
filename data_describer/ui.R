ui <- dashboardPage(
  freshTheme = MY_DASHBOARD, scrollToTop = TRUE,
  # header ------------
  header = dashboardHeader(
    title = "Developing"
  ),
  # sidebar ----------------
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Configuration", tabName = "configuration", icon = icon("gears")),
      menuItem("Descriptive Analysis", tabName = "descriptive_analysis", icon = icon("table")),
      menuItem("Correlation Analysis", tabName = "correlation_analysis", icon = icon("braille")),
      menuItem("Test of Significance", tabName = "difference_analysis", icon = icon("vials")),
      shinyWidgets::prettyCheckbox("use_test_data", "Use Test Data?", value = TRUE, status = "primary")
    )
  ),
  # body ------------------
  body = dashboardBody(
    includeCSS("www/my_dashboard.css"),
    tabItems(
      ## configuration ----------
      tabItem(
        tabName = "configuration",
        verbatimTextOutput("tmp"),
        fluidRow(
          column(5, fileInput("src_file", label = "Data", width = "100%")),
          column(5, textInput("api_key_openai", label = "OpenAI API Key", width = "100%")),
          column(2, actionButton("read", label = "Read", width = "100%")),
          tags$style("#read {height: 60px}")
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL, title = "Data Preview", status = "info",
              solidHeader = TRUE, collapsible = TRUE, id = "preview",
              DTOutput("data")
            )
          )
        )
      ),
      
      ## descriptive analysis ----------
      tabItem(
        tabName = "descriptive_analysis",
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL, status = "info", solidHeader = FALSE, title = "Options",
              id = "des_option_box",
              div(
                style = paste0("height: ", ALL_HEIGHT, "px; overflow-y: auto;"),
                selectizeInput(
                  "var_x", label = "X axis", choices = NULL, multiple = FALSE,
                  width = "100%"
                ),
                shinyWidgets::radioGroupButtons(
                  "var_x_type", label = NULL, choices = VAR_TYPE,
                  direction = "horizontal", justified = TRUE, size = "sm"
                ),
                
                selectizeInput(
                  "var_y", label = "Y axis", choices = NULL, multiple = FALSE,
                  width = "100%"
                ),
                shinyWidgets::radioGroupButtons(
                  "var_y_type", label = NULL, choices = VAR_TYPE,
                  direction = "horizontal", justified = TRUE, size = "sm"
                ),
                
                actionButton("des_cal", "Plot & Analysis", width = "100%"),
                tags$style("#des_cal {height: 60px}")
              )
            )
          ),
          column(
            width = 8, 
            box(
              width = NULL, status = "info", solidHeader = FALSE, title = "Plot",
              id = "des_plot_box", 
              plotlyOutput("des_plot", height = paste0(ALL_HEIGHT, "px"))
            )
          )
        )
      ),
      ## correlation analysis -------
      tabItem(
        tabName = "correlation_analysis",
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL, status = "info", solidHeader = FALSE, title = "Options",
              id = "cor_option_box", 
              div(
                style = paste0("height: ", ALL_HEIGHT-240-41.5-20-20, "px; overflow-y: auto;"),
                selectizeInput(
                  "cor_var_x", label = "Variable X", choices = NULL, multiple = FALSE,
                  width = "100%"
                ),
                shinyWidgets::radioGroupButtons(
                  "cor_var_x_type", label = NULL, choices = VAR_TYPE,
                  direction = "horizontal", justified = TRUE, size = "sm"
                ),
                
                selectizeInput(
                  "cor_var_y", label = "Variable Y", choices = NULL, multiple = FALSE,
                  width = "100%"
                ),
                shinyWidgets::radioGroupButtons(
                  "cor_var_y_type", label = NULL, choices = VAR_TYPE,
                  direction = "horizontal", justified = TRUE, size = "sm"
                ),
                
                shinyWidgets::awesomeRadio(
                  "cor_method", label = "Methods", choices = COR_METHOD,
                  status = "default", checkbox = FALSE, width = "100%"
                ),
                
                actionButton("cor_cal", "Calculate", width = "100%"),
                tags$style("#cor_cal {height: 60px}")
              )
            ),
            
            box(
              width = NULL, status = "info", solidHeader = FALSE, title = "Result",
              id = "cor_res_box", 
              div(
                style = "height: 240px; overflow-y: auto;",
                verbatimTextOutput("cor_res", placeholder = TRUE)
              )
            ),
            tags$style("#cor_res {background-color: #FFF; border: 0px; pading: 0; font-family: 'Fira Mono'}")
          ),
          column(
            width = 8, 
            box(
              width = NULL, status = "info", solidHeader = FALSE, title = "Plot",
              id = "cor_plot_box", 
              plotlyOutput("cor_plot", height = paste0(ALL_HEIGHT, "px"))
            )
          )
        )
      ),
      ## difference analysis -------
      tabItem(
        tabName = "difference_analysis",
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL, status = "info", solidHeader = FALSE, title = "Options",
              id = "diff_option_box",
              div(
                style = "height: 310px; overflow-y: auto;",
                selectizeInput(
                  "diff_var_x", label = "Grouping Variable", choices = NULL, multiple = FALSE,
                  width = "100%"
                ),
                shinyWidgets::radioGroupButtons(
                  "diff_var_x_type", label = NULL, choices = GROUPINT_VAR_TYPE, disabled = TRUE,
                  direction = "horizontal", justified = TRUE, size = "sm", selected = GROUPINT_VAR_TYPE
                ),
                
                selectizeInput(
                  "diff_var_y", label = "Analyzing Variable", choices = NULL, multiple = FALSE,
                  width = "100%"
                ),
                shinyWidgets::radioGroupButtons(
                  "diff_var_y_type", label = NULL, choices = ANALYZING_VAR_TYPE,
                  direction = "horizontal", justified = TRUE, size = "sm"
                ),
                
                actionButton("diff_cal", "Calculate", width = "100%"),
                tags$style("#diff_cal {height: 60px}")
              )
            )
          ),
          column(
            width = 8,
            box(
              width = NULL, status = "info", solidHeader = FALSE, title = "Result",
              id = "diff_res_box",
              shinyWidgets::radioGroupButtons(
                "diff_res_label", label = NULL, choices = c("^ 3 ^"),
                direction = "horizontal", justified = TRUE, size = "sm"
              ),
              div(
                style = "height: 265px; overflow-y: auto;",
                gt_output("diff_res")
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL, status = "info", solidHeader = FALSE, title = "Plot",
              id = "diff_plot_box", 
              plotlyOutput("diff_plot", height = paste0(ALL_HEIGHT-310-20-41.5-20, "px"))
            )
          )
        )
      )
    )
  )
)

