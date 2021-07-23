# 将reactiveValues对象转换成一个ggtheme对象
.reactiveValues_to_theme <- function(x) {
  x <- reactiveValuesToList(x)
  for (ele in ELEMENTS) {
    if (inherits(x[[ele]], "function")){
      if (is.null(x[[ele]]())) {
        x[ele] <- list(NULL)
      } else {
        x[[ele]] <- x[[ele]]()
      }
    }
  }
  attr(x, "class") <- c("theme", "gg")
  attr(x, "complete") <- TRUE
  attr(x, "validate") <- TRUE
  return(x)
}

# 生成一个值为属性可能取值的字符向量
# 我也没想明白为什么要用一个函数而不直接使用一个向量
# 就是感觉这样写起来很顺手
.types <- function(del = NULL) {
  type_choices <- c("Remove", "Inherit", "Assign")
  if (missing(del)) {
    return(type_choices)
  } else {
    return(type_choices[del])
  }
}

# 获取属性的类型
.get_attr_type <- function(x) {
  types <- .types()
  x <- x[1]
  if (is.null(x)) {
    return(types[2])
  } else if (is.na(x)) {
    return(types[1]) 
  } else if (inherits(x, "logical")) {
    if (x) {
      return(types[3])
    } else {
      return(types[1])
    }
  } else {
    return(types[3])
  }
}

# 切换属性下元素的输入控件是否可用
.toggle_controler <- function(controler, elements, input) {
  observeEvent(input[[controler]], {
    if (input[[controler]] != .types(3)) {
      for (each in elements) {
        shinyjs::disable(each)
      }
    } else {
      for (each in elements) {
        shinyjs::enable(each)
      }
    }
  })
}

# 设置默认值
.set_default <- function(args, default) {
  ifelse(.get_attr_type(args) == .types(3), args, default)
}

# 对属性下的各元素进行赋值
# 对于一个名为xx_type的属性，会对xx进行赋值供elemeng_xx(xx = NULL)函数使用
.assign <- function(x, input) {
  for (controler in x) {
    ele <- stringr::str_split(controler, "_type", simplify = TRUE)[1]
    if (input[[controler]] == .types(1)) {
      assign(ele, NA, envir = parent.env(environment()))
    } else if (input[[controler]] == .types(2)) {
      assign(ele, NULL, envir = parent.env(environment()))
    } else {
      if (ele == "size") {
        assign(
          ele, do.call(input$size_unit, list(input$size_value)),
          envir = parent.env(environment())
        )
      } else if (ele == "arrow") {
        assign(
          ele, arrow(input$angle, unit(input$value, input$unit), input$ends, input$type),
          envir = parent.env(environment())
        )
      } else if (ele == "justification") {
        assign(
          ele, c(input$just_x, input$just_y), envir = parent.env(environment())
        )
      } else if (ele == "linetype") {
        assign(
          ele, ifelse(input$linetype %in% as.character(0:8), as.numeric(input$linetype), input$linetype),
          envir = parent.env(environment())
        )
      } else if (ele == "margin") {
        assign(
          ele, 
          margin(
            t = input$value_top, b = input$value_bottom, l = input$value_left, r = input$value_right,
            unit = c(input$unit_top, input$unit_bottom, input$unit_left, input$unit_right)
          ),
          envir = parent.env(environment())
        )
      } else if (ele == "position") {
        if (input$position == "string") {
          assign(ele, input$position_xy, envir = parent.env(environment()))
        } else {
          assign(
            ele, c(input$position_x, input$position_y), envir = parent.env(environment())
          )
        }
      } else if (ele == "unit") {
        assign(
          ele, unit(input$value, input$unit), envir = parent.env(environment())
        )
      } else {
        assign(
          ele, input[[ele]], envir = parent.env(environment())
        )
      }
    }
  }
}

# 生成reactive的图
.get_plot <- function(graph) {
  if (graph() == "default") {
    plot <- reactiveValuesToList(default_plot)
    plot <- plot$base + 
      plot$legend + 
      plot$facet +
      plot$title + 
      plot$subtitle +
      plot$caption +
      plot$tag
  } else if (graph() == "own") {
    plot <- reactiveValuesToList(own_plot)
    plot <- plot$plot
  }
  plot + .reactiveValues_to_theme(new_theme)
}

# 返回renderCachedPlot()的key
.cache_key <- function(graph = NULL) {
  list(
    .reactiveValues_to_theme(new_theme),
    reactiveValuesToList(default_plot),
    reactiveValuesToList(own_plot),
    ifelse(is.null(graph), 1, graph())
  )
}
