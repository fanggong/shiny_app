observeEvent(input$config_var_type, {
  showModal(
    modalDialog(
      title = "Edit Variable Types",
      renderUI({
        lapply(names(files$dat), function(colname) {
          radioGroupButtons(
            paste0(colname, "_type"), colname, choices = VAR_TYPE, 
            selected = .convert_var_type(files$info[colname]),
            direction = "horizontal", justified = TRUE, size = "sm"
          )
        })
      }),
      footer = tagList(
        actionButton("apply_types", "Apply"),
        modalButton("Cancel")
      ),
      fade = TRUE, size = "l"
    )
  )
})

output$other_description <- renderUI({
  other_info <- info()[!info()$var_type %in% c("Categorical", "Ordinal", "Numerical"), ]
  other_dat <- dat()[other_info$var_name]
  
  validate(
    need(nrow(other_info) != 0, "No Other (except Categorical, Ordinal, Numerical) Variables")
  )
  
  tbl <- data.frame(
    Variable = names(other_dat),
    MissingValue = sapply(other_dat, function(t) sum(is.na(t))),
    MissingValueRate = sapply(other_dat, function(t) sum(is.na(t)) / length(t)),
    nUniqueValue = sapply(other_dat, function(t) uniqueN(t))
  )
  tbl <- format_table(
    tbl, 
    list(
      MissingValue = formatter(.tag = "span", style = function(x) style(
        display = "inline-block", `background-color` = MISSING_VALUE, 
        width = paste0(100*.proportion(x, nrow(dat())),"px")
      )),
      MissingValueRate = percent
    ),
    align = c("c", "r", "c", "c"),
    row.names = FALSE
  )
  spk_add_deps(div(htmltools::HTML(tbl)))
})
  