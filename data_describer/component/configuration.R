observeEvent(input$read, {
  if (input$use_test_data) {
    dat <- reactive(sample_dat)
  } else {
    req(input$src_file)
    dat_file <- isolate(input$src_file)
    ext <- tools::file_ext(dat_file$datapath)
    if (ext == "csv") {
      dat <- reactive({data.table::fread(dat_file$datapath)})
    } else if (ext == "xlsx") {
      dat <- reactive({openxlsx::read.xlsx(dat_file$datapath)})
    }
  }
  info <- reactive(sapply(dat(), class))
  
  output$data <- renderDT(
    { dat() },
    extensions = 'Buttons', 
    options = list(
      scrollX = TRUE,
      dom = 'Bfrtip', buttons = I('colvis')
    )
  )
  
  files$dat <- dat()
  files$info <- info()
})


