observeEvent(input$read, {
  if (input$use_test_data) {
    iris$ordinal <- sample(letters[1:5], nrow(iris), replace = TRUE)
    iris$nothing <- rnorm(nrow(iris))
    info <- reactive(data.frame(
      var_name = names(iris),
      var_type = c("Numerical", "Numerical", "Numerical", "Numerical", "Categorical",
                   "Ordinal", "None"),
      range = c("(0,6)", "(0,3)", "(0,4)", "(0,1.8)", NA, NA, NA),
      levels = c(NA, NA, NA, NA, "setosa,virginica", "e<d<b<a", NA)
    ))
    dat <- reactive(.format_data(iris, info()))
  } else {
    req(input$file, input$info)
    
    dat_file <- isolate(input$info)
    ext <- tools::file_ext(dat_file$datapath)
    if (ext == "csv") {
      info <- reactive({data.table::fread(dat_file$datapath)})
    } else if (ext == "xlsx") {
      info <- reactive({openxlsx::read.xlsx(dat_file$datapath)})
    }
    
    info_file <- isolate(input$file)
    ext <- tools::file_ext(info_file$datapath)
    if (ext == "csv") {
      dat <- reactive({.format_data(data.table::fread(info_file$datapath), info())})
    } else if (ext == "xlsx") {
      dat <- reactive({.format_data(openxlsx::read.xlsx(info_file$datapath), info())})
    }
  }
  
  output$numerical_description <- renderUI({
    
    numerical_info <- info()[info()$var_type == "Numerical", ]
    numerical_dat <- dat()[numerical_info$var_name]
    
    validate(
      need(nrow(numerical_info) != 0, "No Numerical Variables")
    )
    
    tbl <- data.frame(
      Variable = names(numerical_dat),
      nMissingValue = sapply(numerical_dat, function(t) sum(is.na(t))),
      MissingValueRate = sapply(numerical_dat, function(t) sum(is.na(t)) / length(t)),
      Mean = sapply(numerical_dat, mean, na.rm = TRUE),
      StandardError = sapply(numerical_dat, sd, na.rm = TRUE),
      ValidRange = sapply(numerical_info$range, .convert_range),
      Distribution = sapply(numerical_dat, function(t) .spk_chr_box(t))
    )
    tbl <- format_table(
      tbl, 
      list(
        nMissingValue = formatter(.tag = "span", style = function(x) style(
          display = "inline-block", `background-color` = MISSING_VALUE, 
          width = paste0(100*.proportion(x, nrow(dat())),"px")
        )),
        MissingValueRate = percent
      ),
      align = c("c", "r", "c", "c", "c", "c", "c"), 
      row.names = FALSE, digits = 6
    )
    spk_add_deps(div(htmltools::HTML(tbl)))
  })
  
  output$cate_ordi_description <- renderUI({
    req(dat)
    cate_ordi_info <- info()[info()$var_type %in% c("Categorical", "Ordinal"), ]
    cate_ordi_dat <- dat()[cate_ordi_info$var_name]
    
    validate(
      need(nrow(cate_ordi_info) != 0, "No Categorical or Ordinal Variables")
    )
    
    tbl <- data.frame(
      Variable = names(cate_ordi_dat),
      nMissingValue = sapply(cate_ordi_dat, function(t) sum(is.na(t))),
      MissingValueRate = sapply(cate_ordi_dat, function(t) sum(is.na(t)) / length(t)),
      nLevels = sapply(cate_ordi_dat, nlevels),
      Levels = sapply(cate_ordi_dat, function(t) paste(
        levels(t), collapse = ifelse(inherits(t, "ordered"), " < ", ", ")
      )),
      Distribution = sapply(cate_ordi_dat, function(t) .spk_chr_bar(t))
    )
    tbl <- format_table(
      tbl, 
      list(
        nMissingValue = formatter(.tag = "span", style = function(x) style(
          display = "inline-block", `background-color` = MISSING_VALUE, 
          width = paste0(100*.proportion(x, nrow(dat())),"px")
        )),
        MissingValueRate = percent
      ),
      align = c("c", "r", "c", "c", "l", "c"),
      row.names = FALSE
    )
    spk_add_deps(div(htmltools::HTML(tbl)))
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
  
  files$dat <- dat()
  files$info <- info()
})