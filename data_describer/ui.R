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
      prettyCheckbox("use_test_data", "Use Test Data?", value = TRUE, status = "primary")
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
              htmlOutput("data")
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
            selectizeInput(
              "var_x", label = "X axis", choices = NULL, multiple = FALSE,
              width = "100%"
            ),
            radioGroupButtons(
              "var_x_type", label = NULL, choices = VAR_TYPE, disabled = TRUE,
              direction = "horizontal", justified = TRUE, size = "sm"
            ),
            # htmlOutput("var_x_levels"),
            
            selectizeInput(
              "var_y", label = "Y axis", choices = NULL, multiple = FALSE,
              width = "100%"
            ),
            radioGroupButtons(
              "var_y_type", label = NULL, choices = VAR_TYPE, disabled = TRUE,
              direction = "horizontal", justified = TRUE, size = "sm"
            ),

            fluidRow(
              column(12, actionButton("des_cal", "Plot & Analysis", width = "100%")),
              tags$style("#des_cal {height: 60px}")
            )
          ),
          column(
            width = 8, 
            box(
              width = NULL, status = "info", solidHeader = FALSE, title = "Distribution Diagram",
              id = "des_plot_box", plotlyOutput("des_plot", height = "630px")
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
            selectizeInput(
              "cor_var_x", label = "Variable X", choices = NULL, multiple = FALSE,
              width = "100%"
            ),
            radioGroupButtons(
              "cor_var_x_type", label = NULL, choices = VAR_TYPE, disabled = TRUE,
              direction = "horizontal", justified = TRUE, size = "sm"
            ),
            
            selectizeInput(
              "cor_var_y", label = "Variable Y", choices = NULL, multiple = FALSE,
              width = "100%"
            ),
            radioGroupButtons(
              "cor_var_y_type", label = NULL, choices = VAR_TYPE, disabled = TRUE,
              direction = "horizontal", justified = TRUE, size = "sm"
            ),
            
            awesomeRadio(
              "cor_method", label = "Methods", choices = COR_METHOD,
              status = "default", checkbox = FALSE, width = "100%"
            ),
            
            fluidRow(
              column(12, actionButton("cor_cal", "Calculate", width = "100%")),
              tags$style("#cor_cal {height: 60px}")
            ),

            fluidRow(
              column(12, verbatimTextOutput("cor_ana", placeholder = TRUE)),
              tags$style("#cor_ana {margin-top: 10px; height: 235px; background-color: #FFF}")
            )
          ),
          column(
            width = 8, 
            box(
              width = NULL, status = "info", solidHeader = FALSE, title = "Correlation Analysis",
              id = "cor_plot_box", plotlyOutput("cor_plot", height = "630px")
            )
          )
        )
      )
    )
  )
)

