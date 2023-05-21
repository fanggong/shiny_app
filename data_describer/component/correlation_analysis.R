observe({
  updateSelectizeInput(session, "cor_var_x", choices = names(files$dat), server = TRUE)
  updateSelectizeInput(session, "cor_var_y", choices = names(files$dat), server = TRUE)
})


observeEvent(input$cor_var_x, {
  shinyWidgets::updateRadioGroupButtons(
    session, "cor_var_x_type", disabled = FALSE,
    selected = files$info[input$cor_var_x]
  )
})
observeEvent(input$cor_var_x_type, {
  files$info[input$cor_var_x] <- input$cor_var_x_type
})


observeEvent(input$cor_var_y, {
  shinyWidgets::updateRadioGroupButtons(
    session, "cor_var_y_type", disabled = FALSE,
    selected = files$info[input$cor_var_y]
  )
})
observeEvent(input$cor_var_y_type, {
  files$info[input$cor_var_y] <- input$cor_var_y_type
})


observeEvent(input$cor_cal, {
  req(files$dat)
  output$cor_plot <- renderPlotly({
    output$cor_res <- renderPrint({
      cat("")
    })
    method <- isolate(input$cor_method)
    
    var_x <- isolate(input$cor_var_x)
    var_y <- isolate(input$cor_var_y)
    
    var_x_type <- isolate(input$cor_var_x_type)
    var_y_type <- isolate(input$cor_var_y_type)
    
    x <- .format_data(files$dat[[var_x]], var_x_type)
    y <- .format_data(files$dat[[var_y]], var_y_type)
    
    if (var_x == var_y) {
      var_x <- paste0(var_x, "_x")
      var_y <- paste0(var_y, "_y")
    }
    
    if (method == "Pearson correlation coefficient") {
      validate(need(
        var_x_type == "numeric" && var_y_type == "numeric", 
        "Variable X and Y must both be numeric variables"
      ))
      
      plt <- plot_ly(
        x = x, y = y, type = "scatter", mode = "markers", 
        opacity = OPACITY, hoverinfo = "text",
        text = paste0("</br> ", var_x, ": ", x, 
                      "</br> ", var_y, ": ", y),
        marker = list(color = POINT_COLOR), 
        hoverlabel = list(font = list(family = FONT_FAMILY))
      ) %>% 
      layout(
        xaxis = list(title = var_x, zeroline = FALSE),
        yaxis = list(title = var_y, zeroline = FALSE),
        font = list(family = FONT_FAMILY)
      )
      output$cor_res <- renderPrint({
        tmp <- data.frame(x = x, y = y)
        names(tmp) <- c(var_x, var_y)
        code <- sprintf("cor.test(~%s+%s,data=tmp,method='pearson')", var_x, var_y)
        eval(parse(text = code))
      })
    }
    else if (method == "Kendall correlation coefficient") {
      validate(need(
        var_x_type == "numeric" && var_y_type == "numeric", 
        "Variable X and Y must both be numeric variables"
      ))
      
      plt <- plot_ly(
        x = x, y = y, type = "scatter", mode = "markers", 
        opacity = OPACITY, hoverinfo = "text",
        text = paste0("</br> ", var_x, ": ", x, 
                      "</br> ", var_y, ": ", y),
        marker = list(color = POINT_COLOR), 
        hoverlabel = list(font = list(family = FONT_FAMILY))
      ) %>% 
        layout(
          xaxis = list(title = var_x, zeroline = FALSE),
          yaxis = list(title = var_y, zeroline = FALSE),
          font = list(family = FONT_FAMILY)
        )
      output$cor_res <- renderPrint({
        tmp <- data.frame(x = x, y = y)
        names(tmp) <- c(var_x, var_y)
        code <- sprintf("cor.test(~%s+%s,data=tmp,method='kendall',exact=FALSE)", var_x, var_y)
        eval(parse(text = code))
      })
    }
    else if (method == "Spearman correlation coefficient") {
      validate(need(
        var_x_type == "ordered" && var_y_type == "ordered", 
        "Variable X and Y must both be ordered variables"
      ))
      
      tmp <- as.data.frame(table(x, y))
      names(tmp) <- c(var_x, var_y, "count")
      colors_dis <- colorRampPalette(c(BAR_COLOR, "#CCCCCC"))(length(levels(y)))
      names(colors_dis) <- levels(y)
      plt <- plot_ly(
        x = tmp[[var_x]], y = tmp[["count"]], color = tmp[[var_y]], type = "bar",
        opacity = OPACITY, colors = colors_dis, hoverinfo = "text",
        text = paste0("</br> ", var_x, ": ", tmp[[var_x]], 
                      "</br> ", var_y, ": ", tmp[[var_y]],
                      "</br> ", "Count", ":", tmp[["count"]]),
        hoverlabel = list(font = list(family = FONT_FAMILY))
      ) %>% 
      layout(
        xaxis = list(zeroline = FALSE, title = var_x),
        yaxis = list(zeroline = FALSE, title = "Count"),
        legend = list(title = list(text = var_y)),
        font = list(family = FONT_FAMILY)
      )
      
      output$cor_res <- renderPrint({
        tmp <- data.frame(x = as.integer(x), y = as.integer(y))
        names(tmp) <- c(var_x, var_y)
        code <- sprintf("cor.test(~%s+%s,data=tmp,method='spearman',exact=FALSE)", var_x, var_y)
        eval(parse(text = code))
      })
    }
    else if (method == "Mutual information") {
      validate(need(
        var_x_type %in% c("factor", "ordered") && var_y_type %in% c("factor", "ordered"),
        "Variable X and Y must both be factor/ordered variables"
      ))
      
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
      
      output$cor_res <- renderPrint({
        cat("\n")
        cat(strwrap("Mutual information", prefix = "\t"), sep = "\n")
        cat("\n")
        cat(sprintf("data: %s and %s\n", var_x, var_y))
        cat("method: the entropy of the empirical probability distribution\n")
        cat("sample estimates:\n")
        res <- c(mi = infotheo::mutinformation(x, y))
        print(res)
      })
    }
    
    plt
  })
})