filter_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "filter-op-container grid-gap",
    shiny::uiOutput(
      outputId = ns("column")
    ),
    shiny::uiOutput(
      outputId = ns("operator")
    ),
    shiny::uiOutput(
      outputId = ns("value")
    )
  )
}

filter_operation <- function(
  input, output, session, .values, data_r
) {
  
  ns <- session$ns
  
  # UI Related -----------------------------------------------------------------
  choices_r <- shiny::reactive({
    names(data_r())
  })
  
  output$column <- shiny::renderUI({
    ui <- shiny::selectInput(
      inputId = ns("column"),
      label = NULL,
      choices = list(
        "Select a column" = as.list(choices_r())
      ),
      selected = fallback(shiny::isolate(input$column), NULL)
    )
    
    ui
  })
  
  column_r <- shiny::reactive({
    data_r()[[shiny::req(input$column)]]
  })
  
  column_type_r <- shiny::reactive({
    pillar::type_sum(column_r())
  })
  
  column_type_name_r <- shiny::reactive({
    helper_column_type_name(column_type_r(), .values)
  })
  
  filter_class_r <- shiny::reactive({
    helper_filter_class(column_type_r(), .values)
  })
  
  output$operator <- shiny::renderUI({
    shiny::validate(
      shiny::need(
        filter_class_r() != "missing",
        paste("Column type", column_type_name_r(), "is not supported by the
              filter operation.")
      )
    )
    
    choices <- .values$FILTER_OPERATORS[[filter_class_r()]]
    
    shiny::selectInput(
      inputId = ns("operator"),
      label = NULL,
      choices = list(
        "Operator" = as.list(choices)
      ),
      selected = fallback(shiny::isolate(input$operator), NULL)
    )
  })
  
  output$value <- shiny::renderUI({
    switch(
      filter_class_r(),
      "logical" = logical_value_ui_r(),
      "numeric" = numeric_value_ui_r(),
      "character" = character_value_ui_r(),
      "factor" = factor_value_ui_r(),
      "date" = date_value_ui_r()
    )
  })
  
  logical_value_ui_r <- shiny::reactive({
    shiny::selectInput(
      inputId = ns("logical_value"),
      label = NULL,
      choices = c(true = "true", false = "false"),
      selected = fallback(input$logical_value, "true")
    )
  })
  
  numeric_value_ui_r <- shiny::reactive({
    min <- min(data_r()[[shiny::req(input$column)]])
    max <- max(data_r()[[shiny::req(input$column)]])
    
    if (shiny::req(input$operator) == "bw") {
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::numericInput(
            inputId = ns("numeric_value_start"),
            label = NULL,
            value = fallback(input$numeric_value_start, min),
            min = min,
            max = max
          )
        ),
        shiny::column(
          width = 6,
          shiny::numericInput(
            inputId = ns("numeric_value_end"),
            label = NULL,
            value = fallback(input$numeric_value_end, max),
            min = min,
            max = max
          )
        )
      )
    } else {
      shiny::numericInput(
        inputId = ns("numeric_value"),
        label = NULL,
        value = fallback(input$numeric_value, min),
        min = min,
        max = max
      )
    }
  })
  
  character_value_ui_r <- shiny::reactive({
    if (shiny::req(input$operator) == "in") {
      shiny::selectInput(
        inputId = ns("character_value_in"),
        label = NULL,
        choices = unique(column_r()),
        multiple = TRUE,
        selected = fallback(input$character_value_in, NULL)
      )
    } else {
      shiny::textInput(
        inputId = ns("character_value_text"),
        label = NULL,
        value = fallback(input$character_value_text, NULL)
      )
    }
  })
  
  date_value_ui_r <- shiny::reactive({
    min <- min(column_r())
    max <- max(column_r())
    
    if (shiny::req(input$operator) == "eq") {
      shiny::dateInput(
        inputId = ns("date_value"),
        label = NULL,
        value = fallback(input$date_value, min),
        min = min,
        max = max
      )
    } else {
      shiny::dateRangeInput(
        inputId = ns("date_value_bw"),
        label = NULL,
        start = fallback(input$date_value_bw[1], min),
        end = fallback(input$date_value_bw[2], max),
        min = min,
        max = max
      )
    }
  })
  
  factor_value_ui_r <- shiny::reactive({
    if (shiny::req(input$operator) == "in") {
      multiple <- TRUE
      selected <- levels(column_r())
    } else {
      multiple <- FALSE
      selected <- levels(column_r())[1]
    }
    
    shiny::selectInput(
      inputId = ns("factor_value"),
      label = NULL,
      choices = levels(column_r()),
      multiple = multiple,
      selected = levels(column_r())
    )
  })
  
  # Logic related --------------------------------------------------------------
  value_r <- shiny::reactive({
    switch(
      filter_class_r(),
      "logical" = logical_value_r(),
      "numeric" = numeric_value_r(),
      "character" = character_value_r(),
      "date" = date_value_r(),
      "factor" = factor_value_r()
    )
  })
  
  logical_value_r <- shiny::reactive({
    as.logical(shiny::req(input$logical_value))
  })
  
  numeric_value_r <- shiny::reactive({
    if (shiny::req(input$operator) == "bw") {
      c(
        safe_numeric_input_value(shiny::req(input$numeric_value_start)),
        safe_numeric_input_value(shiny::req(input$numeric_value_end))
      )
    } else {
      safe_numeric_input_value(shiny::req(input$numeric_value))
    }
  })
  
  character_value_r <- shiny::reactive({
    if (shiny::req(input$operator) == "in") {
      shiny::req(input$character_value_in)
    } else {
      shiny::req(input$character_value_text)
    }
  })
  
  date_value_r <- shiny::reactive({
    if (shiny::req(input$operator) == "eq") {
      shiny::req(input$date_value)
    } else {
      shiny::req(input$date_value_bw)
    }
  })
  
  factor_value_r <- shiny::reactive({
    shiny::req(input$factor_value)
  })
  
  operator_fun_r <- shiny::reactive({
    switch(
      filter_class_r(),
      "logical" = logical_operator_fun_r(),
      "numeric" = numeric_operator_fun_r(),
      "character" = character_operator_fun_r(),
      "date" = date_operator_fun_r(),
      "factor" = factor_operator_fun_r()
    )
  })
  
  logical_operator_fun_r <- shiny::reactive({
    switch(
      shiny::req(input$operator),
      "eq" = `==`,
      "ne" = `!=`
    )
  })
  
  numeric_operator_fun_r <- shiny::reactive({
    switch(
      shiny::req(input$operator),
      "eq" = `==`,
      "le" = `<=`,
      "ge" = `>=`,
      "lt" = `<`,
      "gt" = `>`,
      "ne" = `!=`,
      "bw" = function(x, value) x >= value[1] & x <= value[2]
    )
  })
  
  character_operator_fun_r <- shiny::reactive({
    switch(
      shiny::req(input$operator),
      "eq" = `==`,
      "in" = `%in%`,
      "re" = function(x, value) stringr::str_detect(x, value)
    )
  })
  
  date_operator_fun_r <- shiny::reactive({
    switch(
      shiny::req(input$operator),
      "eq" = `==`,
      "bw" = function(x, value) x >= value[1] & x <= value[2]
    )
  })
  
  factor_operator_fun_r <- shiny::reactive({
    switch(
      shiny::req(input$operator),
      "eq" = `==`,
      "in" = `%in%`
    )
  })
  
  predicate_fun_r <- shiny::reactive({
    value <- value_r()
    function(x) {
      operator_fun_r()(x, value)
    }
  })
  
  filtered_data_r <- shiny::reactive({
    dplyr::filter_at(
      data_r(),
      .vars = shiny::req(input$column),
      .vars_predicate = predicate_fun_r()
    )
  })
  
  return_list <- list(
    data_r = filtered_data_r
  )
  
  return(return_list)
}