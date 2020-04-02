mutate_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "mutate-op-container",
    shiny::textInput(
      inputId = ns("name"),
      label = NULL,
      placeholder = "Name of new column"
    ),
    htmltools::div(
      class = "grid-center",
      "="
    ),
    shiny::uiOutput(
      outputId = ns("operator")
    ),
    htmltools::div(
      class = "grid-center",
      "("
    ),
    shiny::uiOutput(
      outputId = ns("details"),
      class = "mutate-op-details"
    ),
    htmltools::div(
      class = "grid-center",
      ")"
    )
  )
}

mutate_operation <- function(
  input, output, session, .values, data_r
) {
  
  ns <- session$ns
  
  column_names_r <- shiny::reactive({
    names(data_r())
  })
  
  column_types_r <- shiny::reactive({
    purrr::map_chr(column_names_r(), function(col) {
      pillar::type_sum(data_r()[[col]])
    }) 
  })
  
  output$operator <- shiny::renderUI({
    shiny::selectInput(
      inputId = ns("operator"),
      label = NULL,
      choices = helper_mutate_operator_choices(.values)
    )
  })
  
  operator_type_r <- shiny::reactive({
    helper_mutate_operator_type(shiny::req(input$operator), .values)
  })
  
  mutate_fun_r <- shiny::reactive({
    helper_mutate_fun(shiny::req(input$operator), .values)
  })
  
  allowed_column_types_r <- shiny::reactive({
    helper_mutate_allowed_column_types(shiny::req(input$operator), .values)
  })
  
  allowed_column_names_r <- shiny::reactive({
    column_names_r()[column_types_r() %in% allowed_column_types_r()]
  })
  
  output$details <- shiny::renderUI({
    shiny::validate(
      shiny::need(
        allowed_column_names_r(),
        "Dataset has no column with an appropriate type for the selected 
        mutate function."
      ),
      errorClass = "grid-center"
    )
    
    multiple <- if (operator_type_r() == "single") FALSE else TRUE
    
    shiny::selectInput(
      inputId = ns("col"),
      label = NULL,
      choices = allowed_column_names_r(),
      multiple = multiple
    )
  })
  
  name_r <- shiny::reactive({
    shiny::req(input$name)
  })
  
  is_valid_number_of_cols_r <- shiny::reactive({
    switch(
      operator_type_r(),
      "single" = length(input$col) == 1,
      "multiple" = TRUE
    )
  })
  
  mutated_data_r <- shiny::reactive({
    shiny::req(allowed_column_names_r(), input$col, is_valid_number_of_cols_r())
    
    col <- purrr::map(input$col, function(col) {
      sym(col)
    })
    fun <- mutate_fun_r()
    
    fml <- list(quo((!!fun)(!!!col)))
    names(fml) <- name_r()
    
    data_r() %>%
      dplyr::mutate(!!!fml)
  })
  
  return_list <- list(
    data_r = mutated_data_r
  )
  
  return(return_list)
}