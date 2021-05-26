

ui <- fluidPage(
  navbarPage(
    "ggplot2主题生成器",
    tabPanel("示例图形", uiOutput("plot_element")),
    navbarMenu(
      "全局",
      tabPanel("背景与边框", uiOutput("rect")),
      tabPanel("线条", uiOutput("line")),
      tabPanel("文本", uiOutput("text")),
      tabPanel("标题", uiOutput("title"))
    ),
    navbarMenu(
      "坐标轴",
      tabPanel(
        "标题",
        tabsetPanel(
          tabPanel("全局", uiOutput("axis.title")),
          tabPanel("X轴-全局", uiOutput("axis.title.x")),
          tabPanel("Y轴-全局", uiOutput("axis.title.y")),
          tabPanel("X轴-上方", uiOutput("axis.title.x.top")),
          tabPanel("X轴-下方", uiOutput("axis.title.x.bottom")),
          tabPanel("Y轴-左侧", uiOutput("axis.title.y.left")),
          tabPanel("Y轴-右侧", uiOutput("axis.title.y.right"))
        )
      ),
      tabPanel(
        "文本",
        tabsetPanel(
          tabPanel("X轴-全局"),
          tabPanel("Y轴-全局"),
          tabPanel("X轴-上方"),
          tabPanel("X轴-下方"),
          tabPanel("Y轴-左侧"),
          tabPanel("Y轴-右侧")
        )
      ),
      tabPanel(
        "刻度",
        tabsetPanel(
          tabPanel("X轴-全局"),
          tabPanel("Y轴-全局"),
          tabPanel("X轴-上方"),
          tabPanel("X轴-下方"),
          tabPanel("Y轴-左侧"),
          tabPanel("Y轴-右侧")
        )
      )
    ),
    navbarMenu(
      "图例"
    ),
    navbarMenu(
      "面板"
    ),
    navbarMenu(
      "图像"
    ),
    navbarMenu(
      "分面之间"
    )
  ),
  hr(),
  column(width = 8, offset = 2, plotOutput("plot", height = "500px"))
)
