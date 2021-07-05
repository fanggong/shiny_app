ui <- dashboardPage(
  freshTheme = MY_DASHBOARD, scrollToTop = TRUE,
  # header ------------
  header = dashboardHeader(
    title = "Developing"
  ),
  # sidebar ----------------
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "data_upload", icon = icon("file-upload")),
      menuItem("Correlation Analysis", tabName = "correlation_analysis", icon = icon("braille"))
    )
  ),
  # body ------------------
  body = dashboardBody(
    includeCSS("www/my_dashboard.css"),
    tabItems(
      ## data upload ----------
      tabItem(
        tabName = "data_upload",
        fluidRow(
          column(5, fileInput("file", label = "Select Data", width = "100%")),
          column(5, fileInput("info", label = "Select Description File", width = "100%")),
          column(2, actionButton("read", "Read Data", width = "100%"),
                 prettyCheckbox("use_test_data", "Use Test Data?", value = TRUE, status = "primary"))
        ),
        box(
          width = NULL, title = "Numerical Variables", status = "info",
          solidHeader = TRUE, collapsible = TRUE, id = "numerical_variables_box",
          htmlOutput("numerical_description") %>% .withSpinner()
        ),
        box(
          width = NULL, title = "Categorical/Ordinal Variables", status = "info",
          solidHeader = TRUE, collapsible = TRUE, id = "cate_ordi_variables_box",
          htmlOutput("cate_ordi_description") %>% .withSpinner()
        ),
        box(
          width = NULL, title = "Other Variables", status = "info",
          solidHeader = TRUE, collapsible = TRUE, id = "other_variables_box",
          htmlOutput("other_description") %>% .withSpinner()
        )
      ),
      ## correlation analysis -------
      tabItem(
        tabName = "correlation_analysis",
        fluidRow(
          column(
            width = 4,
            radioGroupButtons(
              "cor_var_x_type", "Select Variable X", choices = VAR_TYPE,
              direction = "horizontal", justified = TRUE, size = "sm"
            ),
            selectizeInput(
              "cor_var_x_sel", label = NULL, choices = letters, multiple = TRUE,
              width = "100%"
            ),
            radioGroupButtons(
              "cor_var_y_type", "Select Variable Y", choices = VAR_TYPE,
              direction = "horizontal", justified = TRUE, size = "sm"
            ),
            selectizeInput(
              "cor_var_y_sel", label = NULL, choices = NULL, multiple = TRUE,
              width = "100%"
            ),
            awesomeRadio(
              "cor_method", label = "Methods", choices = COR_METHOD,
              status = "default", checkbox = FALSE, width = "100%"
            ),
            fluidRow(
              column(6, actionButton("cor_cal", "Calculate", width = "100%")),
              column(6, actionButton("cor_reset", "Reset", width = "100%"))
            )
          ),
          column(width = 8, box(title = "correlation plot", width = NULL))
        )
      )
    )
  )
)

