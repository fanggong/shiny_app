observeEvent(input$read, {
  updateSelectizeInput(session, "diff_var_x", choices = names(files$dat), server = TRUE)
  updateSelectizeInput(session, "diff_var_y", choices = names(files$dat), server = TRUE)
})


observeEvent(input$diff_var_y, {
  updateRadioGroupButtons(
    session, "diff_var_y_type",
    selected = files$info[input$diff_var_y]
  )
})
observeEvent(input$diff_var_y_type, {
  files$info[input$diff_var_y] <- input$diff_var_y_type
})


observeEvent(input$diff_cal, {
  var_x <- isolate(input$diff_var_x)
  var_y <- isolate(input$diff_var_y)
  
  if (var_x == var_y) {
    output$diff_res <- render_gt({
      validate(
        need(var_x != var_y, "The grouping variable and the analysis variable should not be identical.")
      )
    })
    output$diff_plot <- renderPlotly({
      validate(
        need(var_x != var_y, "The grouping variable and the analysis variable should not be identical.")
      )
    })
    return()
  }
  
  var_x_type <- "factor"
  var_y_type <- isolate(input$diff_var_y_type)
  
  x <- .format_data(files$dat[[var_x]], var_x_type)
  y <- .format_data(files$dat[[var_y]], var_y_type)
  tmp <- data.frame(group = x, analyze = y)
  
  if (var_y_type == "numeric") {
    split_tmp <- split(tmp$analyze, tmp$group)
    # 正态性检验
    ks_res <- rbindlist(lapply(split_tmp, ks_test), idcol = var_x)
    sw_res <- rbindlist(lapply(split_tmp, sw_test), idcol = var_x)
    
    # 方差齐性检验
    levene_res <- levene_test(tmp)
    
    if (nlevels(x) == 2) {
      updateRadioGroupButtons(
        session, "diff_res_label", 
        choices = c("T-test", "Approximate T-test", "Wilcoxon Rank Sum Test")
      )
      plt <- plotly_empty()
      output$diff_res <- render_gt({
        if (input$diff_res_label == "T-test") {
          res <- t_test(tmp)
          norm_test(ks_res, sw_res, var_x, var_y)
        }
        else if (input$diff_res_label == "Approximate T-test") {
          
        }
        else if (input$diff_res_label == "Wilcoxon Rank Sum Test") {
          data.frame(z = c(4:6))
        }
      })
    }
    else if (nlevels(x) > 2) {
      updateRadioGroupButtons(
        session, "diff_res_label", 
        choices = c("Analysis of Variance", "Kruskal-Wallis Rank Sum Test")
      )
    }
  }
  else if (var_y_type == "factor") {
    if (nlevels(x) == 2) {
      updateRadioGroupButtons(
        session, "diff_res_label", 
        choices = c("Chi-squared Test", "Continuity Correction Chi-squared Test", "Fisher Test")
      )
    }
    else if (nlevels(x) > 2) {
      updateRadioGroupButtons(
        session, "diff_res_label", 
        choices = c("Chi-squared Test", "Fisher Test")
      )
    }
  }
  else if (var_y_type == "ordered") {
    updateRadioGroupButtons(
      session, "diff_res_label", 
      choices = c("Ridit Analysis", "Wilcoxon Rank Sum Test")
    )
  }
  
  output$diff_plot <- renderPlotly({
    plt
  })
  
    
    
    
    
    
    
    # 
    # if (var_y_type == "numeric") {
    #   plt <- plotly_empty()
    #   output$diff_res <- render_gt({
    #     if (nlevels(x) == 2) {
    #       t_res <- t.test(analyze ~ group, data = tmp, var.equal = TRUE)
    #       t_appro_res <- t.test(analyze ~ group, data = tmp, var.equal = FALSE)
    #       w_rank_res <- wilcox.test(analyze ~ group, data = tmp)
    #     }
    #     else if (nlevels(x) > 2) {
    #       anova_res <- aov(analyze ~ group, data = tmp)
    #       k_w_rank_res <- kruskal.test(analyze ~ group, data = tmp)
    #     }
    #   })
    # }
    # else if (var_y_type == "factor") {
    #   plt <- plotly_empty()
    #   output$diff_res <- render_gt({
    #     if (nlevels(x) == 2) {
    #       chisq_res <- chisq.test(y, x, correct = FALSE)
    #       
    #       chisq_conti_res <- chisq.test(y, x, correct = TRUE)
    #       
    #       fisher_res <- fisher.test(y, x)
    #     }
    #     else if (nlevels(x) > 2) {
    #       
    #     }
    #   })
    # }
    
})