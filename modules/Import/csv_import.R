csv_import_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "import",
    shiny::uiOutput(
      outputId = ns("file_input")
    ),
    shiny::uiOutput(
      outputId = ns("file_type_not_supported")
    ),
    shiny::uiOutput(
      outputId = ns("delim")
    ),
    shiny::uiOutput(
      outputId = ns("name")
    ),
    shiny::uiOutput(
      outputId = ns("name_error")
    ),
    shiny::uiOutput(
      outputId = ns("data_error")
    ),
    shiny::uiOutput(
      outputId = ns("preview")
    ),
    shiny::uiOutput(
      outputId = ns("finish")
    )
  )
}

csv_import <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  reset_rv <- shiny::reactiveVal(0)
  file_input_outdated_rv <- shiny::reactiveVal(TRUE)
  
  output$file_input <- shiny::renderUI({
    reset_rv()
    shiny::fileInput(
      inputId = ns("file"),
      label = "Upload an csv file"
    )
  })
  
  shiny::observeEvent(input$file, {
    file_input_outdated_rv(FALSE)
  })
  
  file_ending_r <- shiny::reactive({
    stringr::str_match(shiny::req(input$file$datapath), "\\.([^\\.]+)$")[1,2]
  })
  
  file_type_supported_r <- shiny::reactive({
    file_ending_r() == "csv"
  })
  
  output$file_type_not_supported <- shiny::renderUI({
    if (!file_type_supported_r()) {
      paste0("File type .", file_ending_r(), " not supported. Please upload an csv file.")
    } else {
      NULL
    }
  })
  
  output$delim <- shiny::renderUI({
    shiny::req(
      !file_input_outdated_rv(),
      file_type_supported_r()
    )
    
    shiny::selectInput(
      inputId = ns("delim"),
      label = "Field delimiter",
      choices = c(",", ";")
    )
  })
  
  output$name <- shiny::renderUI({
    shiny::req(
      !file_input_outdated_rv(),
      file_type_supported_r()
    )
    
    shiny::textInput(
      inputId = ns("name"),
      label = "Name",
      value = input$file$name
    )
  })
  
  data_r <- shiny::reactive({
    shiny::req(
      !file_input_outdated_rv(),
      file_type_supported_r(),
      input$delim
    )
    
    switch(
      input$delim,
      "," = readr::read_csv(input$file$datapath),
      ";" = readr::read_csv2(input$file$datapath)
    )
  })
  
  output$finish <- shiny::renderUI({
    shiny::req(!file_input_outdated_rv())
    
    shiny::validate(
      shiny::need(
        !input$name == "" && !purrr::is_null(input$name), 
        "Name must contain at least one character."
      ),
      shiny::need(
        is.data.frame(data_r()), "Uploaded csv file must contain a data frame."
      )
    )
    
    import_finish_ui(
      id = ns("id_import_finish")
    )
  })
  
  shiny::callModule(
    module = import_finish,
    id = "id_import_finish",
    .values = .values,
    data_r = data_r,
    file_name_r = shiny::reactive(input$file$name),
    name_r = shiny::reactive(input$name),
    reset_rv = reset_rv,
    file_input_outdated_rv = file_input_outdated_rv
  )
}