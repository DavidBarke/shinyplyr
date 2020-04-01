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
          id = ns("id_geom_content")
        )
      )
    ),
    geom_subrows_ui(
      id = ns("id_geom_subrows")
    )
  )
}

geom_subrow <- function(
  input, output, session, .values, data_r, row_index, free_aesthetics_r
) {
  
  ns <- session$ns
  
  # Toggle geom subrows ---------------------------------------------------------
  geom_subrows_selector <- paste0("#", ns("id_geom_subrows"), "-subrows")
  
  shiny::observeEvent(geom_content_return$toggle_rv(), {
    if (geom_content_return$toggle_rv() %% 2 == 0) {
      shinyjs::show(
        anim = .values$ANIM,
        selector = geom_subrows_selector
      )
    } else {
      shinyjs::hide(
        anim = .values$ANIM,
        selector = geom_subrows_selector
      )
    }
  })
  
  # Output ------------------------------
  subrow_index <- paste(row_index, 2, sep = ".")
  
  output$index <- shiny::renderUI({
    subrow_index
  })
  
  geom_content_return <- shiny::callModule(
    module = geom_content,
    id = "id_geom_content",
    .values = .values,
    data_r = data_r
  )
  
  geom_subrows_return <- shiny::callModule(
    module = geom_subrows,
    id = "id_geom_subrows",
    .values = .values,
    data_r = data_r,
    subrow_index = subrow_index,
    free_aesthetics_r = free_aesthetics_r
  )
  
  return_list <- list(
    geom_r = geom_content_return$geom_r,
    geom_args_r = geom_subrows_return$geom_args_r,
    geom_fun_r = geom_content_return$geom_fun_r,
    n_var_r = geom_content_return$n_var_r
  )
  
  return(return_list)
}