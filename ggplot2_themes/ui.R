ui <- navbarPage(
  title = "Theme Generator For ggplot2",
  header = header_ui(),
  # Sample Plot ----
  tabPanel(
    "Sample Graph Setting",
    tabsetPanel(
      tabPanel("Default Sample Graph", default_setting_ui()),
      tabPanel("Your Own Sample Graph", own_setting_ui())
    )
  ),
  # Basic ----
  tabPanel(
    "Global",
    tabsetPanel(
      tabPanel("All line elements", uiOutput("line")),
      tabPanel("All text elements", uiOutput("text")),
      tabPanel("All title elements", uiOutput("title")),
      tabPanel("All rectangular elements", uiOutput("rect"))
    )
  ),
  # Axis ----
  navbarMenu(
    "Axis",
    ## Axis - Axis Labels ----
    tabPanel(
      "Axis Labels",
      tabsetPanel(
        tabPanel("All axes", uiOutput("axis.title")),
        tabPanel("Both X-axes", uiOutput("axis.title.x")),
        tabPanel("Both Y-axes", uiOutput("axis.title.y")),
        tabPanel("Top X-axis", uiOutput("axis.title.x.top")),
        tabPanel("Bottom X-axis", uiOutput("axis.title.x.bottom")),
        tabPanel("Left Y-axis", uiOutput("axis.title.y.left")),
        tabPanel("Right Y-axis", uiOutput("axis.title.y.right"))
      )
    ),
    ## Axis - Axis Lines ----
    tabPanel(
      "Axis Lines",
      tabsetPanel(
        tabPanel("All axes", uiOutput("axis.line")),
        tabPanel("Both X-axes", uiOutput("axis.line.x")),
        tabPanel("Both Y-axes", uiOutput("axis.line.y")),
        tabPanel("Top X-axis", uiOutput("axis.line.x.top")),
        tabPanel("Bottom X-axis", uiOutput("axis.line.x.bottom")),
        tabPanel("Left Y-axis", uiOutput("axis.line.y.left")),
        tabPanel("Right Y-axis", uiOutput("axis.line.y.right"))
      )
    ),
    ## Axis - Tick Labels ----
    tabPanel(
      "Tick Labels",
      tabsetPanel(
        tabPanel("All axes", uiOutput("axis.text")),
        tabPanel("Both X-axes", uiOutput("axis.text.x")),
        tabPanel("Both Y-axes", uiOutput("axis.text.y")),
        tabPanel("Top X-axis", uiOutput("axis.text.x.top")),
        tabPanel("Bottom X-axis", uiOutput("axis.text.x.bottom")),
        tabPanel("Left Y-axis", uiOutput("axis.text.y.left")),
        tabPanel("Right Y-axis", uiOutput("axis.text.y.right"))
      )
    ),
    ## Axis - Tick Marks Style ----
    tabPanel(
      "Tick Marks Style",
      tabsetPanel(
        tabPanel("All axes", uiOutput("axis.ticks")),
        tabPanel("Both X-axes", uiOutput("axis.ticks.x")),
        tabPanel("Both Y-axes", uiOutput("axis.ticks.y")),
        tabPanel("Top X-axis", uiOutput("axis.ticks.x.top")),
        tabPanel("Bottom X-axis", uiOutput("axis.ticks.x.bottom")),
        tabPanel("Left Y-axis", uiOutput("axis.ticks.y.left")),
        tabPanel("Right Y-axis", uiOutput("axis.ticks.y.right"))
      )
    ),
    ## Axis - Tick Marks Length ----
    tabPanel(
      "Tick Marks Length",
      tabsetPanel(
        tabPanel("All axes", uiOutput("axis.ticks.length")),
        tabPanel("Both X-axes", uiOutput("axis.ticks.length.x")),
        tabPanel("Both Y-axes", uiOutput("axis.ticks.length.y")),
        tabPanel("Top X-axis", uiOutput("axis.ticks.length.x.top")),
        tabPanel("Bottom X-axis", uiOutput("axis.ticks.length.x.bottom")),
        tabPanel("Left Y-axis", uiOutput("axis.ticks.length.y.left")),
        tabPanel("Right Y-axis", uiOutput("axis.ticks.length.y.right"))
      )
    )
  ),
  # Legend ----
  navbarMenu(
    "Legend",
    ## Legend - Legend Area ----
    tabPanel(
      "Legend Area",
      tabsetPanel(
        tabPanel("Position", uiOutput("legend.position")),
        tabPanel("Anchor Point / Justification", uiOutput("legend.justification")),
        tabPanel("Background", uiOutput("legend.box.background")),
        tabPanel("Spacing", uiOutput("legend.box.spacing")),
        tabPanel("Margin", uiOutput("legend.box.margin"))
      )
    ),
    ## Legend - Single Legend ----
    tabPanel(
      "Single Legend",
      tabsetPanel(
        tabPanel("Legend Background", uiOutput("legend.background")),
        tabPanel("Legend Margin", uiOutput("legend.margin"))
      )
    ),
    ## Legend - Multiple Legends ----
    tabPanel(
      "Multiple Legends",
      tabsetPanel(
        tabPanel("Direction", uiOutput("legend.box")),
        tabPanel("Justification", uiOutput("legend.box.just")),
        tabPanel("Both Direction Spacing", uiOutput("legend.spacing")),
        tabPanel("Horizontal Spacing", uiOutput("legend.spacing.x")),
        tabPanel("Vertical Spacing", uiOutput("legend.spacing.y"))
      )
    ),
    ## Legend - Legend Text ----
    tabPanel(
      "Legend Text",
      tabsetPanel(
        tabPanel("Title Style", uiOutput("legend.title")),
        tabPanel("Title Alignment", uiOutput("legend.title.align")),
        tabPanel("Labels Style", uiOutput("legend.text")),
        tabPanel("Labels Alignment", uiOutput("legend.text.align"))
      )
    ),
    ## Legend - Legend Keys ----
    tabPanel(
      "Legend Keys",
      tabsetPanel(
        tabPanel("Background", uiOutput("legend.key")),
        tabPanel("Direction", uiOutput("legend.direction")),
        tabPanel("Size", uiOutput("legend.key.size")),
        tabPanel("Height", uiOutput("legend.key.height")),
        tabPanel("Width", uiOutput("legend.key.width"))
      )
    )
  ),
  # Panel ----
  navbarMenu(
    "Panel",
    ## Panel - Background and Border ----
    tabPanel(
      "Background and Border",
      tabsetPanel(
        tabPanel("Background", uiOutput("panel.background")),
        tabPanel("Border", uiOutput("panel.border")),
        tabPanel("Set the Panel On Top", uiOutput("panel.ontop"))
      )
    ),
    ## Panel - Grid Lines ----
    tabPanel(
      "Grid Lines",
      tabsetPanel(
        tabPanel("All", uiOutput("panel.grid")),
        tabPanel("Major", uiOutput("panel.grid.major")),
        tabPanel("Minor", uiOutput("panel.grid.minor")),
        tabPanel("Vertical Major", uiOutput("panel.grid.major.x")),
        tabPanel("Horizontal Major", uiOutput("panel.grid.major.y")),
        tabPanel("Vertical Minor", uiOutput("panel.grid.minor.x")),
        tabPanel("Horizontal Minor", uiOutput("panel.grid.minor.y"))
      )
    ),
    ## Panel - Spacing ----
    tabPanel(
      "Spacing",
      tabsetPanel(
        tabPanel("Both Directions", uiOutput("panel.spacing")),
        tabPanel("Horizontal", uiOutput("panel.spacing.x")),
        tabPanel("Vertical", uiOutput("panel.spacing.y"))
      )
    )
  ),
  # Plot ----
  navbarMenu(
    "Plot",
    ## Plot - Background and Margin ----
    tabPanel(
      "Background and Margin",
      tabsetPanel(
        tabPanel("Background", uiOutput("plot.background")),
        tabPanel("Margin", uiOutput("plot.margin"))
      )
    ),
    ## Plot - Text ----
    tabPanel(
      "Text",
      tabsetPanel(
        tabPanel("Title Style", uiOutput("plot.title")),
        tabPanel("Subtitle Style", uiOutput("plot.subtitle")),
        tabPanel("Caption Style", uiOutput("plot.caption")),
        tabPanel("Tag Style", uiOutput("plot.tag")),
        tabPanel("Title / Subtitle Position", uiOutput("plot.title.position")),
        tabPanel("Caption Position", uiOutput("plot.caption.position")),
        tabPanel("Tag Position", uiOutput("plot.tag.position"))
      )
    )
  ),
  # Facet Labels ----
  navbarMenu(
    "Facet Labels",
    ## Strip - Placement ----
    tabPanel(
      "Placement",
      tabsetPanel(
        tabPanel("Placement", uiOutput("strip.placement")),
        tabPanel("Spacing for Grid", uiOutput("strip.switch.pad.grid")),
        tabPanel("Spacing for Wrap", uiOutput("strip.switch.pad.wrap"))
      )
    ),
    ## Strip - Background ----
    tabPanel(
      "Background",
      tabsetPanel(
        tabPanel("Both Directions", uiOutput("strip.background")),
        tabPanel("Horizontal", uiOutput("strip.background.x")),
        tabPanel("Vertical", uiOutput("strip.background.y"))
      )
    ),
    ## Strip - Text ----
    tabPanel(
      "Text",
      tabsetPanel(
        tabPanel("Both Directions", uiOutput("strip.text")),
        tabPanel("Horizontal", uiOutput("strip.text.x")),
        tabPanel("Vertical", uiOutput("strip.text.y"))
      )
    )
  ),
  includeCSS("www/my_navbar.css"),
  useShinyjs()
)


