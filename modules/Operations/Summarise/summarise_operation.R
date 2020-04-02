summarise_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  choices <- list(
    "Center" = list("mean", "median"),
    "Spread" = list("sd", "IQR", "mad"),
    "Range" = list("min", "max"),
    "Position" = list("first", "last"),
    "Count" = list("n", "n_distinct"),
    "Logical" = list("any", "all")
  )
  
  htmltools::div(
    class = "summarise-op-container grid-gap",
    shiny::textInput(
      inputId = ns("name"),
      label = NULL,
      placeholder = "Name of summary column"
    ),
    htmltools::div(
      class = "grid-center",
      "="
    ),
    shiny::selectInput(
      inputId = ns("summary_fun"),
      label = NULL,
      choices = choices
    ),
    htmltools::div(
      class = "grid-center",
      "("
    ),
    shiny::uiOutput(
      outputId = ns("summary_col")
    ),
    htmltools::div(
      class = "grid-center",
      ")"
    )
  )
}

summarise_operation <- function(
  input, output, session, .values, data_r
) {
  
  ns <- session$ns
  
  column_names_r <- shiny::reactive({
    names(data_r())
  })
  
  column_classes_r <- shiny::reactive({
    purrr::map_chr(data_r(), function(col) {
      helper_summarise_class(pillar::type_sum(col), .values)
    })
  })
  
  summary_fun_r <- shiny::reactive({
    helper_summarise_fun(shiny::req(input$summary_fun), .values)
  })
  
  allowed_column_classes_r <- shiny::reactive({
    helper_summarise_allowed_classes(shiny::req(input$summary_fun), .values)
  })
  
  allowed_column_names_r <- shiny::reactive({
    if (any(allowed_column_classes_r() == "__ALL__")) {
      allowed <- c("logical", "numeric", "character", "date", "factor")
    } else {
      allowed <- allowed_column_classes_r()
    }
    
    column_names_r()[column_classes_r() %in% allowed]
  })
  
  output$summary_col <- shiny::renderUI({
    shiny::validate(
      shiny::need(
        input$summary_fun != "n",
        "Summary function n does not need a column to be applied to."
      ),
      errorClass = "grid-center"
    )
    
    shiny::validate(
      shiny::need(
        length(allowed_column_names_r()) > 0,
        "Dataset has no column with an appropriate type for the selected 
        summary function."
      ),
      errorClass = "grid-center"
    )
    
    shiny::selectInput(
      inputId = ns("summary_col"),
      label = NULL,
      choices = list(
        "Select column to summarise" = as.list(allowed_column_names_r())
      ),
      selected = fallback(input$summary_col, NULL)
    )
  })
  
  summary_col_r <- shiny::reactive({
    shiny::req(input$summary_col)
  })
  
  name_r <- shiny::reactive({
    shiny::req(input$name)
  })
  
  summarised_data_r <- shiny::reactive({
    shiny::req(allowed_column_names_r())
    
    fun <- summary_fun_r()
    
    if (input$summary_fun == "n") {
      fml <- list(quo((!!fun)()))
    } else {
      col <- sym(summary_col_r())
      fml <- list(quo((!!fun)(!!col)))
    }
    
    names(fml) <- name_r()
    
    data_r() %>% 
      dplyr::summarise(!!!fml)
  })
  
  return_list <- list(
    data_r = summarised_data_r
  )
  
  return(return_list)
}