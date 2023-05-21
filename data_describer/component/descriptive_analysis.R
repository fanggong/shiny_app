observe({
  updateSelectizeInput(session, "var_x", choices = c(names(files$dat), "None"), server = TRUE)
  updateSelectizeInput(session, "var_y", choices = c("None", names(files$dat)), server = TRUE)
})

observeEvent(input$var_x, {
  if (input$var_x == "None") {
    shinyWidgets::updateRadioGroupButtons(session, "var_x_type", disabled = TRUE)
  } else {
    shinyWidgets::updateRadioGroupButtons(
      session, "var_x_type", disabled = FALSE,
      selected = files$info[input$var_x]
    )
  }
})
observeEvent(input$var_x_type, {
  files$info[input$var_x] <- input$var_x_type
})

observeEvent(input$var_y, {
  if (input$var_y == "None") {
    shinyWidgets::updateRadioGroupButtons(session, "var_y_type", disabled = TRUE)
  } else {
    shinyWidgets::updateRadioGroupButtons(
      session, "var_y_type", disabled = FALSE,
      selected = files$info[input$var_y]
    )
  }})
observeEvent(input$var_y_type, {
  files$info[input$var_y] <- input$var_y_type
})


observeEvent(input$des_cal, {
  req(files$dat)
  output$des_plot <- renderPlotly({
    
    var_x <- isolate(input$var_x)
    var_y <- isolate(input$var_y)
    
    var_x_type <- isolate(input$var_x_type)
    var_y_type <- isolate(input$var_y_type)
    
    if (var_x == "None" && var_y != "None") {
      y <- .format_data(files$dat[[var_y]], var_y_type)
    } else if (var_x != "None" && var_y == "None") {
      x <- .format_data(files$dat[[var_x]], var_x_type)
    } else if (var_x != "None" && var_y != "None") {
      x <- .format_data(files$dat[[var_x]], var_x_type)
      y <- .format_data(files$dat[[var_y]], var_y_type)
    }
    
    if (var_x == var_y && var_x != "None") {
      var_x <- paste0(var_x, "_x")
      var_y <- paste0(var_y, "_y")
    }
    
    if (var_x == var_y && var_x == "None") {
      plt <- plotly_empty()
    } 
    else if (var_y == "None") {
      if (var_x_type == "numeric") {
        plt <- plot_ly(
          x = x, type = "violin", points = "all", y0 = var_x, name = "",
          opacity = OPACITY,
          marker = list(color = POINT_COLOR),
          line = list(color = LINE_COLOR),
          box = list(visible = TRUE),
          hoverlabel = list(font = list(family = FONT_FAMILY))
        ) %>% 
        layout(
          xaxis = list(zeroline = FALSE),
          yaxis = list(zeroline = FALSE),
          font = list(family = FONT_FAMILY)
        )
      } 
      else if (var_x_type %in% c("factor", "ordered")) {
        tmp <- as.data.frame(table(x))
        names(tmp) <- c(var_x, "count")
        plt <- plot_ly(
          x = tmp[[var_x]], y = tmp[["count"]], type = "bar",
          opacity = OPACITY, hoverinfo = "text",
          hovertext = paste0("</br> ", var_x, ": ", tmp[[var_x]], 
                             "</br> ", "Count", ": ", tmp[["count"]]),
          hoverlabel = list(font = list(family = FONT_FAMILY))
        ) %>% 
        layout(
          xaxis = list(zeroline = FALSE, title = var_x),
          yaxis = list(zeroline = FALSE, title = "Count"),
          font = list(family = FONT_FAMILY)
        )
      }
    }
    else if (var_x == "None") {
      if (var_y_type == "numeric") {
        plt <- plot_ly(
          y = y, type = "violin", points = "all", x0 = var_y, name = "",
          opacity = OPACITY,
          marker = list(color = POINT_COLOR),
          line = list(color = LINE_COLOR),
          box = list(visible = TRUE),
          hoverlabel = list(font = list(family = FONT_FAMILY))
        ) %>% 
        layout(
          xaxis = list(zeroline = FALSE),
          yaxis = list(zeroline = FALSE),
          font = list(family = FONT_FAMILY)
        )
      } 
      else if (var_y_type %in% c("factor", "ordered")) {
        tmp <- as.data.frame(table(y))
        names(tmp) <- c(var_y, "count")
        plt <- plot_ly(
          y = tmp[[var_y]], x = tmp[["count"]], type = "bar",
          opacity = OPACITY, hoverinfo = "text",
          hovertext = paste0("</br> ", var_y, ": ", tmp[[var_y]], 
                             "</br> ", "Count", ": ", tmp[["count"]]),
          hoverlabel = list(font = list(family = FONT_FAMILY))
        ) %>% 
        layout(
          yaxis = list(zeroline = FALSE, title = var_y),
          xaxis = list(zeroline = FALSE, title = "Count"),
          font = list(family = FONT_FAMILY)
        )
      }
    } 
    else {
      if (var_x_type == "numeric" && var_y_type %in% c("factor", "ordered")) {
        plt <- plot_ly(
          y = y, x = x, type = "violin", points = "all", orientation = "h",
          opacity = OPACITY,
          marker = list(color = POINT_COLOR, size = 3),
          line = list(color = LINE_COLOR),
          box = list(visible = TRUE),
          hoverlabel = list(font = list(family = FONT_FAMILY))
        ) %>% 
        layout(
          xaxis = list(zeroline = FALSE, title = var_x),
          yaxis = list(zeroline = FALSE, title = var_y),
          font = list(family = FONT_FAMILY)
        )
      } 
      else if (var_x_type == "numeric" && var_y_type == "numeric") {
        plt <- subplot(
          plot_ly(
            x = x, type = "histogram", opacity = OPACITY, name = var_x,
            hoverlabel = list(font = list(family = FONT_FAMILY)),
            marker = list(color = BAR_COLOR), nbinsx = 20,
            hovertemplate = "</br>Range: %{x} </br>Count: %{y}"
          ) %>% 
          layout(
            xaxis = list(showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(zeroline = FALSE),
            font = list(family = FONT_FAMILY)
          ),
          plotly_empty(),
          plot_ly(
            x = x, y = y, type = "scatter", mode = "markers", 
            opacity = OPACITY, hoverinfo = "text",
            text = paste0("</br> ", var_x, ": ", x, "</br> ", var_y, ": ", y),
            marker = list(color = POINT_COLOR), 
            hoverlabel = list(font = list(family = FONT_FAMILY))
          ) %>% 
          layout(
            xaxis = list(title = var_x, zeroline = FALSE),
            yaxis = list(title = var_y, zeroline = FALSE),
            font = list(family = FONT_FAMILY)
          ),
          plot_ly(
            y = y, type = "histogram", opacity = OPACITY, name = var_y,
            hoverlabel = list(font = list(family = FONT_FAMILY)),
            marker = list(color = BAR_COLOR), nbinsy = 20,
            hovertemplate = "</br>Range: %{y} </br>Count: %{x}"
          ) %>% 
          layout(
            xaxis = list(zeroline = FALSE),
            yaxis = list(showticklabels = FALSE, zeroline = FALSE),
            font = list(family = FONT_FAMILY)
          ),
          nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2),
          titleX = TRUE, titleY = TRUE
        ) %>% 
        layout(
          showlegend = FALSE
        )
      } 
      else if (var_x_type %in% c("factor", "ordered") && var_y_type %in% c("factor", "ordered")) {
        tmp <- as.data.frame(table(x, y))
        names(tmp) <- c(var_x, var_y, "count")
        colors_dis <- colorRampPalette(c(BAR_COLOR, "#CCCCCC"))(length(levels(y)))
        names(colors_dis) <- levels(y)
        plt <- plot_ly(
          x = tmp[[var_x]], y = tmp[["count"]], color = tmp[[var_y]], type = "bar",
          opacity = OPACITY, colors = colors_dis, hoverinfo = "text",
          text = paste0("</br> ", var_x, ": ", tmp[[var_x]], 
                        "</br> ", var_y, ": ", tmp[[var_y]],
                        "</br> ", "Count", ": ", tmp[["count"]]),
          hoverlabel = list(font = list(family = FONT_FAMILY))
        ) %>% 
        layout(
          xaxis = list(zeroline = FALSE, title = var_x),
          yaxis = list(zeroline = FALSE, title = "Count"),
          legend = list(title = list(text = var_y)),
          font = list(family = FONT_FAMILY)
        )
      } 
      else if (var_x_type %in% c("factor", "ordered") && var_y_type == "numeric") {
        plt <- plot_ly(
          y = y, x = x, type = "violin", points = "all",
          opacity = OPACITY,
          marker = list(color = POINT_COLOR, size = 3),
          line = list(color = LINE_COLOR),
          box = list(visible = TRUE),
          hoverlabel = list(font = list(family = FONT_FAMILY))
        ) %>% 
        layout(
          xaxis = list(zeroline = FALSE, title = var_x),
          yaxis = list(zeroline = FALSE, title = var_y),
          font = list(family = FONT_FAMILY)
        )
      }
    }
    
    plt
    
  })
})