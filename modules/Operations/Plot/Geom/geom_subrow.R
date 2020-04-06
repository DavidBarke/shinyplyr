geom_subrow_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::div(
      class = "plot-subrow-content",
      shiny::uiOutput(
        outputId = ns("index"),
        class = "grid-center"
      ),
      htmltools::div(
        class = "grid-gap geom-content",
        geom_content_ui(
          id = ns("id_geom_content"),
          toggle_button = m_toggle_button(
            id = ns("id_m_toggle"),
            class = "grid-center"
          )
        )
      )
    ),
    m_toggle_ui(
      id = ns("id_m_toggle"),
      geom_subrows_ui(
        id = ns("id_geom_subrows")
      )
    )
  )
}

geom_subrow <- function(
  input, output, session, .values, data_r, row_index, free_aesthetics_r,
  n_var_r
) {
  
  ns <- session$ns
  
  # Output ------------------------------
  subrow_index <- paste(row_index, 2, sep = ".")
  
  output$index <- shiny::renderUI({
    subrow_index
  })
  
  geom_xxx_r <- shiny::reactive({
    do.call(geom_content_return$geom_fun_r(), geom_subrows_return$geom_args_r())
  })
  
  geom_content_return <- shiny::callModule(
    module = geom_content,
    id = "id_geom_content",
    .values = .values,
    data_r = data_r,
    n_var_r = n_var_r
  )
  
  geom_subrows_return <- shiny::callModule(
    module = geom_subrows,
    id = "id_geom_subrows",
    .values = .values,
    data_r = data_r,
    subrow_index = subrow_index,
    free_aesthetics_r = free_aesthetics_r
  )
  
  shiny::callModule(
    module = m_toggle,
    id = "id_m_toggle",
    .values = .values
  )
  
  return_list <- list(
    geom_xxx_r = geom_xxx_r,
    geom_r = geom_content_return$geom_r
  )
  
  return(return_list)
}