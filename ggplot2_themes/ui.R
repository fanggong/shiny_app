

ui <- navbarPage(
  title = "Theme Generator For ggplot2",
  # Sample Plot ----
  tabPanel(
    "Graph Elements",
    tabsetPanel(
      tabPanel("Use Default Sample Data", uiOutput("sample_data")),
      tabPanel("Use Your Own Data", uiOutput("own_data"))
    )
  ),
  # Basic ----
  tabPanel(
    "Basic",
    tabsetPanel(
      tabPanel("All lines", uiOutput("line")),
      tabPanel("All text", uiOutput("text")),
      tabPanel("All titles", uiOutput("title")),
      tabPanel("All borders and backgroupd", uiOutput("rect"))
    )
  ),
  # Axis ----
  navbarMenu(
    "Axis",
    ## Axis - Title ----
    tabPanel(
      "Title",
      tabsetPanel(
        tabPanel("All axes", uiOutput("axis.title")),
        tabPanel("X-axis", uiOutput("axis.title.x")),
        tabPanel("Y-axis", uiOutput("axis.title.y")),
        tabPanel("Top X-axis", uiOutput("axis.title.x.top")),
        tabPanel("Bottom X-axis", uiOutput("axis.title.x.bottom")),
        tabPanel("Left Y-axis", uiOutput("axis.title.y.left")),
        tabPanel("Right Y-axis", uiOutput("axis.title.y.right"))
      )
    ),
    ## Axis - Text ----
    tabPanel(
      "Text",
      tabsetPanel(
        tabPanel("All axes", uiOutput("axis.text")),
        tabPanel("X-axis", uiOutput("axis.text.x")),
        tabPanel("Y-axis", uiOutput("axis.text.y")),
        tabPanel("Top X-axis", uiOutput("axis.text.x.top")),
        tabPanel("Bottom X-axis", uiOutput("axis.text.x.bottom")),
        tabPanel("Left Y-axis", uiOutput("axis.text.y.left")),
        tabPanel("Right Y-axis", uiOutput("axis.text.y.right"))
      )
    ),
    ## Axis - Ticks Style ----
    tabPanel(
      "Ticks Style",
      tabsetPanel(
        tabPanel("All axes", uiOutput("axis.ticks")),
        tabPanel("X-axis", uiOutput("axis.ticks.x")),
        tabPanel("Y-axis", uiOutput("axis.ticks.y")),
        tabPanel("Top X-axis", uiOutput("axis.ticks.x.top")),
        tabPanel("Bottom X-axis", uiOutput("axis.ticks.x.bottom")),
        tabPanel("Left Y-axis", uiOutput("axis.ticks.y.left")),
        tabPanel("Right Y-axis", uiOutput("axis.ticks.y.right"))
      )
    ),
    ## Axis - Ticks Length ----
    tabPanel(
      "Ticks Length",
      tabsetPanel(
        tabPanel("All axes", uiOutput("axis.ticks.length")),
        tabPanel("X-axis", uiOutput("axis.ticks.length.x")),
        tabPanel("Y-axis", uiOutput("axis.ticks.length.y")),
        tabPanel("Top X-axis", uiOutput("axis.ticks.length.x.top")),
        tabPanel("Bottom X-axis", uiOutput("axis.ticks.length.xbottom")),
        tabPanel("Left Y-axis", uiOutput("axis.ticks.length.y.left")),
        tabPanel("Right Y-axis", uiOutput("axis.ticks.length.y.right"))
      )
    ),
    ## Axis - Lines ----
    tabPanel(
      "Lines",
      tabsetPanel(
        tabPanel("All axes", uiOutput("axis.line")),
        tabPanel("X-axis", uiOutput("axis.line.x")),
        tabPanel("Y-axis", uiOutput("axis.line.y")),
        tabPanel("Top X-axis", uiOutput("axis.line.x.top")),
        tabPanel("Bottom X-axis", uiOutput("axis.line.x.bottom")),
        tabPanel("Left Y-axis", uiOutput("axis.line.y.left")),
        tabPanel("Right Y-axis", uiOutput("axis.line.y.right"))
      )
    )
  ),
  # Legend ----
  navbarMenu(
    "Legend",
    ## Legend - Spacing ----
    tabPanel(
      "Spacing",
      tabsetPanel(
        tabPanel("All", uiOutput("legend.spacing")),
        tabPanel("X", uiOutput("legend.spacing.x")),
        tabPanel("Y", uiOutput("legend.spacing.y"))
      )
    ),
    ## Legend - Key ----
    tabPanel(
      "Key",
      tabsetPanel(
        tabPanel("Background", uiOutput("legend.key")),
        tabPanel("Size", uiOutput("lengend.key.size")),
        tabPanel("Height", uiOutput("legend.key.height")),
        tabPanel("Width", uiOutput("legend.ley.width"))
      )
    ),
    ## Legend - Box ----
    tabPanel(
      "Box",
      tabsetPanel(
        tabPanel("Arrangement", uiOutput("legend.box")),
        tabPanel("Justification", uiOutput("legend.box.just")),
        tabPanel("Margin", uiOutput("legend.box.margin")),
        tabPanel("Background", uiOutput("legend.box.background")),
        tabPanel("Spacing", uiOutput("legend.box.spacing"))
      )
    ),
    ## Legend - Text ----
    tabPanel(
      "Text",
      tabsetPanel(
        tabPanel("Style", uiOutput("legend.text")),
        tabPanel("Align", uiOutput("legend.text.align"))
      )
    ),
    ## Legend - Title ----
    tabPanel(
      "Title",
      tabsetPanel(
        tabPanel("Style", uiOutput("legend.title")),
        tabPanel("Align", uiOutput("legend.title.align"))
      )
    ),
    ## Legend - Overall ----
    tabPanel(
      "Overall",
      tabsetPanel(
        tabPanel("Background", uiOutput("legend.background")),
        tabPanel("Margin", uiOutput("legend.margin")),
        tabPanel("Position", uiOutput("legend.position")),
        tabPanel("Direction", uiOutput("legend.direction")),
        tabPanel("Justification", uiOutput("legend.justification"))
      )
    )
  ),
  # Panel ----
  navbarMenu(
    "Panel",
    ## Panel - Overall ----
    tabPanel(
      "Overall",
      tabsetPanel(
        tabPanel("Background", uiOutput("panel.background")),
        tabPanel("Border", uiOutput("panel.border")),
        tabPanel("Ontop", uiOutput("panel.ontop"))
      )
    ),
    ## Panel - Spacing ----
    tabPanel(
      "Spacing",
      tabsetPanel(
        tabPanel("All", uiOutput("panel.spacing")),
        tabPanel("X", uiOutput("panel.spacing.x")),
        tabPanel("Y", uiOutput("panel.spacing.y"))
      )
    ),
    ## Panel - Grid ----
    tabPanel(
      "Grid Lines",
      tabsetPanel(
        tabPanel("All", uiOutput("panel.grid")),
        tabPanel("Major", uiOutput("panel.grid.major")),
        tabPanel("Minor", uiOutput("panel.grid.minor")),
        tabPanel("X-axis Major", uiOutput("panel.grid.major.x")),
        tabPanel("Y-axis Major", uiOutput("panel.grid.major.y")),
        tabPanel("X-axis Minor", uiOutput("panel.grid.minor.x")),
        tabPanel("Y-axis Minor", uiOutput("panel.grid.minor.y"))
      )
    )
  ),
  # Plot ----
  navbarMenu(
    "Plot",
    ## Plot - Title ----
    tabPanel(
      "Title",
      tabsetPanel(
        tabPanel("Title Style", uiOutput("plot.title")),
        tabPanel("Subtitle Style", uiOutput("plot.subtitle")),
        tabPanel("Position", uiOutput("plot.title.position"))
      )
    ),
    ## Plot - Caption ----
    tabPanel(
      "Caption",
      tabsetPanel(
        tabPanel("Style", uiOutput("plot.caption")),
        tabPanel("Position", uiOutput("plot.caption.position"))
      )
    ),
    ## Plot - Tag ----
    tabPanel(
      "Tag",
      tabsetPanel(
        tabPanel("Style", uiOutput("plot.tag")),
        tabPanel("Position", uiOutput("plot.tag.position"))
      )
    ),
    ## Plot - Overall ----
    tabPanel(
      "Overall",
      tabsetPanel(
        tabPanel("Margin", uiOutput("plot.margin")),
        tabPanel("Background", uiOutput("plot.background"))
      )
    )
  ),
  # Strip ----
  navbarMenu(
    "Strip",
    ## Strip - Text ----
    tabPanel(
      "Text",
      tabsetPanel(
        tabPanel("All", uiOutput("strip.text")),
        tabPanel("X", uiOutput("strip.text.x")),
        tabPanel("Y", uiOutput("strip.text.y"))
      )
    ),
    ## Strip - Background ----
    tabPanel(
      "Background",
      tabsetPanel(
        tabPanel("All", uiOutput("strip.background")),
        tabPanel("X", uiOutput("strip.background.x")),
        tabPanel("Y", uiOutput("strip.background.y"))
      )
    ),
    ## Strip - Pad ----
    tabPanel(
      "Pad",
      tabsetPanel(
        tabPanel("Grid", uiOutput("strip.switch.pad.grid")),
        tabPanel("Wrap", uiOutput("strip.switch.pad.wrap"))
      )
    ),
    ## Strip - Placement ----
    tabPanel(
      "Placement",
      tabsetPanel(
        tabPanel("Placement", uiOutput("strip.placement"))
      )
    )
  ),
  # footer = column(width = 8, offset = 2, plotOutput("plot", height = "500px")),
  includeCSS("www/my_navbar.css")
)


