aes_subrow_ui <- function(id, row_index) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::div(
      id = ns("aes_subrow"),
      class = "plot-subrow-content",
      shiny::uiOutput(
        outputId = ns("index"),
        class = "grid-center"
      ),
      shiny::uiOutput(
        outputId = ns("content"),
        class = "grid-gap aes-content"
      )
    ),
    m_toggle_ui(
      id = ns("id_m_toggle"),
      shiny::uiOutput(
        outputId = ns("subrows"),
        class = "subrows-container"
      )
    )
  )
}

aes_subrow <- function(
  input, output, session, .values, data_r, row_index, geom_r, n_var_r
) {
  
  ns <- session$ns
  
  aes_return_env <- new.env()
  
  choices_r <- shiny::reactive({
    names(data_r())
  })
  
  is_discrete_column_r <- shiny::reactive({
    purrr::map_lgl(choices_r(), function(col) {
      is_discrete(data_r()[[col]])
    })
  })
  
  is_positive_column_r <- shiny::reactive({
    purrr::map_lgl(choices_r(), function(col) {
      x <- data_r()[[col]]
      !is_discrete(x) && all(x >= 0)
    })
  })
  
  discrete_choices_r <- shiny::reactive({
    choices_r()[is_discrete_column_r()]
  })
  
  continuous_choices_r <- shiny::reactive({
    choices_r()[!is_discrete_column_r()]
  })
  
  positive_choices_r <- shiny::reactive({
    choices_r()[is_positive_column_r()]
  })
  
  geom_allowed_r <- shiny::reactive({
    helper_aes_geom_allowed(geom_r(), .values)
  })
  
  purrr::pmap(.values$plot$AES, function(...) {
    aes_row <- list(...)
    aes_return_env[[aes_row$name]] <- shiny::callModule(
      module = aes_subsubrow,
      id = aes_row$name %_% "subsubrow",
      .values = .values,
      aes_row = aes_row,
      choices_r = choices_r,
      discrete_choices_r = discrete_choices_r,
      continuous_choices_r = continuous_choices_r,
      positive_choices_r = positive_choices_r,
      geom_allowed_r = geom_allowed_r
    )
  })
  
  # Outputs --------------------------------------------------------------------
  subrow_index <- paste(row_index, 1, sep = ".")
  
  output$index <- shiny::renderUI({
    subrow_index
  })
  
  all_aes_r <- shiny::reactive({
    all_aes(geom_r(), n_var_r(), .values)
  })
  
  aes_names_r <- shiny::reactive({
    unlist(all_aes_r())
  })
  
  req_opt_names_r <- shiny::reactive({
    unlist(all_aes_r()[c("required", "optional_class")])
  })
  
  output$subrows <- shiny::renderUI({
    purrr::map2(req_opt_names_r(), seq_along(req_opt_names_r()), function(aes, index) {
      htmltools::div(
        class = "subrow-container grid-gap m-index",
        htmltools::div(
          class = "subrow-index grid-center",
          paste(subrow_index, index, sep = ".")
        ),
        htmltools::div(
          class = "subrow-content aes-subrow-content grid-gap",
          htmltools::div(
            class = "grid-vertical-center",
            aes
          ),
          aes_subsubrow_ui(
            id = ns(aes %_% "subsubrow")
          )
        ),
        htmltools::div(
          class = "subrow-remove"
        )
      )
    })
  })
  
  output$content <- shiny::renderUI({
    ui <- if (toggle_return$open_r()) {
      htmltools::tagList(
        htmltools::div(
          class = "aes-title-help grid-gap",
          htmltools::tags$b(
            class = "grid-vertical-center",
            "Aesthetic"
          ),
          htmltools::div(
            class = "grid-center",
            help_button(ns("help_plot_aes"))
          )
        ),
        htmltools::tags$b(
          class = "grid-vertical-center",
          "Column"
        )
      )
    } else {
      aes_name_val <- purrr::map2_chr(req_opt_names_r(), selected_aes_vals_r(), function(aes, val) {
        paste(aes, val, sep = ": ")
      })
      
      htmltools::tagList(
        htmltools::div(
          class = "aes-title-help grid-gap",
          htmltools::tags$b(
            class = "grid-vertical-center",
            "Aesthetic"
          ),
          htmltools::div(
            class = "grid-center",
            help_button(ns("help_plot_aes"))
          )
        ),
        htmltools::div(
          class = "grid-vertical-center",
          paste(aes_name_val, collapse = ", ")
        )
      )
    }
    
    htmltools::tagList(
      m_toggle_button(
        id = ns("id_m_toggle"),
        class = "grid-center"
      ),
      ui
    )
  })
  
  selected_aes_vals_r <- shiny::reactive({
    purrr::map_chr(req_opt_names_r(), function(aes) {
      aes_return_env[[aes]]$value_r()
    })
  })
  
  non_null_aes_names_r <- shiny::reactive({
    x <- req_opt_names_r()[selected_aes_vals_r() != "NULL"]
    setNames(x, x)
  })
  
  free_aes_names_r <- shiny::reactive({
    setdiff(
      union(
        req_opt_names_r()[selected_aes_vals_r() == "NULL"],
        all_aes_r()$optional_geom
      ),
      c("group")
    )
  })
  
  aes_r <- shiny::reactive({
    aes_list <- purrr::map(non_null_aes_names_r(), function(aes) {
      aes_return_env[[aes]]$value_r()
    })
    
    do.call(ggplot2::aes_string, aes_list)
  })
  
  shiny::observeEvent(input$help_plot_aes, {
    .values$help$open("plot_aes")
  })
  
  message_r <- shiny::reactive({
    if (length(choices_r()) == 0) return(
      "At least one column is required for plotting."
    )
    
    if (any(is.na(selected_aes_vals_r()))) return(
      "At least one required aesthetic can't be set, because there is no
      column with appropriate column type."
    )
    
    NULL
  })
  
  toggle_return <- shiny::callModule(
    module = m_toggle,
    id = "id_m_toggle",
    .values = .values
  )
  
  return_list <- list(
    aes_r = aes_r,
    free_aesthetics_r = free_aes_names_r,
    message_r = message_r
  )
  
  return(return_list)
}