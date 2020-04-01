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
  
  # Currently unused
  rvs <- shiny::reactiveValues(
    column = NULL,
    operator = NULL,
    logical_value = NULL,
    numeric_value = NULL,
    numeric_value_start = NULL,
    numeric_value_end = NULL,
    character_value_in = NULL,
    character_value_text = NULL,
    date_value = NULL,
    date_value_bw = NULL
  )
  
  # UI Related -----------------------------------------------------------------
  choices_r <- shiny::reactive({
    names(data_r())
  })
  
  input_ids <- c(
    "column", "operator", "logical_value", "numeric_value",
    "character_value_in", "character_value_text", "numeric_value_end",
    "numeric_value_start", "date_value", "date_value_bw"
  )
  
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
  
  column_type_r <- shiny::reactive({
    cls <- class(data_r()[[shiny::req(input$column)]])[1]
    
    shiny::req(cls != "NULL")
    
    if (cls %in% c("integer", "factor")) cls <- "numeric"
    
    cls
  })
  
  output$operator <- shiny::renderUI({
    choices <- switch(
      column_type_r(),
      "logical" = logical_operator_choices_r(),
      "numeric" = numeric_operator_choices_r(),
      "character" = character_operator_choices_r(),
      "Date" = date_operator_choices_r()
    )
    
    ui <- shiny::selectInput(
      inputId = ns("operator"),
      label = NULL,
      choices = list(
        "Operator" = as.list(choices)
      ),
      selected = fallback(shiny::isolate(input$operator), NULL)
    )
    
    ui
  })
  
  logical_operator_choices_r <- shiny::reactive({
    c("=" = "eq", "!=" = "ne")
  })
  
  numeric_operator_choices_r <- shiny::reactive({
    c("=" = "eq", "<=" = "le", ">=" = "ge", "<" = "lt", ">" = "gt", "!=" = "ne", "zwischen" = "bw")
  })
  
  character_operator_choices_r <- shiny::reactive({
    c("=" = "eq", "in" = "in", "~" = "re")
  })
  
  date_operator_choices_r <- shiny::reactive({
    c("=" = "eq", "zwischen" = "bw")
  })
  
  output$value <- shiny::renderUI({
    ui <- switch(
      column_type_r(),
      "logical" = logical_value_ui_r(),
      "numeric" = numeric_value_ui_r(),
      "character" = character_value_ui_r(),
      "Date" = date_value_ui_r()
    )
    
    ui
  })
  
  logical_value_ui_r <- shiny::reactive({
    shiny::selectInput(
      inputId = ns("logical_value"),
      label = NULL,
      choices = c(true = "true", false = "false"),
      selected = fallback(rvs$logical_value, "true")
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
            value = fallback(rvs$numeric_value_start, min),
            min = min,
            max = max
          )
        ),
        shiny::column(
          width = 6,
          shiny::numericInput(
            inputId = ns("numeric_value_end"),
            label = NULL,
            value = fallback(rvs$numeric_value_end, max),
            min = min,
            max = max
          )
        )
      )
    } else {
      shiny::numericInput(
        inputId = ns("numeric_value"),
        label = NULL,
        value = fallback(rvs$numeric_value, min),
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
        choices = unique(data_r()[[shiny::req(input$column)]]),
        multiple = TRUE,
        selected = fallback(rvs$character_value_in, NULL)
      )
    } else {
      shiny::textInput(
        inputId = ns("character_value_text"),
        label = NULL,
        value = fallback(rvs$character_value_text, NULL)
      )
    }
  })
  
  date_value_ui_r <- shiny::reactive({
    min <- min(data_r()[[shiny::req(input$column)]])
    max <- max(data_r()[[shiny::req(input$column)]])
    
    if (shiny::req(input$operator) == "eq") {
      shiny::dateInput(
        inputId = ns("date_value"),
        label = NULL,
        value = fallback(rvs$date_value, min),
        min = min,
        max = max
      )
    } else {
      shiny::dateRangeInput(
        inputId = ns("date_value_bw"),
        label = NULL,
        start = fallback(rvs$date_value_bw[1], min),
        end = fallback(rvs$date_value_bw[2], max),
        min = min,
        max = max
      )
    }
  })
  
  # Logic related --------------------------------------------------------------
  value_r <- shiny::reactive({
    switch(
      column_type_r(),
      "logical" = logical_value_r(),
      "numeric" = numeric_value_r(),
      "character" = character_value_r(),
      "Date" = date_value_r()
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
  
  operator_fun_r <- shiny::reactive({
    switch(
      column_type_r(),
      "logical" = logical_operator_fun_r(),
      "numeric" = numeric_operator_fun_r(),
      "character" = character_operator_fun_r(),
      "Date" = date_operator_fun_r()
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