observe({
  updateSelectizeInput(session, "diff_var_x", choices = names(files$dat), server = TRUE)
  updateSelectizeInput(session, "diff_var_y", choices = names(files$dat), server = TRUE)
})


observeEvent(input$diff_var_x, {
  updateRadioGroupButtons(
    session, "diff_var_x_type", disabled = FALSE,
    selected = files$info[input$diff_var_x]
  )
})
observeEvent(input$diff_var_x_type, {
  files$info[input$diff_var_x] <- input$diff_var_x_type
})


observeEvent(input$diff_var_y, {
  updateRadioGroupButtons(
    session, "diff_var_y_type", disabled = FALSE,
    selected = files$info[input$diff_var_y]
  )
})
observeEvent(input$diff_var_y_type, {
  files$info[input$diff_var_y] <- input$diff_var_y_type
})


observeEvent(input$diff_cal, {
  output$diff_res <- render_gt({
    sp500 |>
      dplyr::filter(date >= start_date & date <= end_date) |>
      dplyr::select(-adj_close) |>
      gt() |>
      tab_header(
        title = "S&P 500",
        subtitle = glue::glue("{start_date} to {end_date}")
      ) |>
      fmt_currency() |>
      fmt_date(columns = date, date_style = "wd_m_day_year") |>
      fmt_number(columns = volume, suffixing = TRUE) %>% 
      tab_options(
        container.overflow.x = TRUE, container.overflow.y = TRUE,
        container.height = "210px"
      )
  })
})