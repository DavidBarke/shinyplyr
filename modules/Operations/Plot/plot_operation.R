plot_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::div(
      class = "plot-op-container grid-gap"
    )
  )
}

plot_subrows_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    aes_subrow_ui(
      id = ns("id_aes_subrow")
    ),
    geom_subrow_ui(
      id = ns("id_geom_subrow")
    ),
    facet_subrow_ui(
      id = ns("id_facet_subrow")
    ),
    coord_subrow_ui(
      id = ns("id_coord_subrow")
    ),
    theme_subrow_ui(
      id = ns("id_theme_subrow")
    )
  )
}

plot_operation <- function(
  input, output, session, .values, data_r, row_index, row_container_id, sr_toggle_rv
) {
  
  ns <- session$ns
  
  plot_r <- shiny::reactive({
    ggplot(data_r(), aes_subrow_return$aes_r()) +
      do.call(geom_subrow_return$geom_fun_r(), geom_subrow_return$geom_args_r()) +
      facet_subrow_return$facet_r() +
      coord_subrow_return$coord_fun_r()() + 
      theme_subrow_return$theme_fun_r()()
  })
  
  aes_subrow_return <- shiny::callModule(
    module = aes_subrow,
    id = "id_aes_subrow",
    .values = .values,
    data_r = data_r,
    row_index = row_index,
    geom_r = geom_subrow_return$geom_r,
    n_var_r = geom_subrow_return$n_var_r
  )
  
  geom_subrow_return <- shiny::callModule(
    module = geom_subrow,
    id = "id_geom_subrow",
    .values = .values,
    data_r = data_r,
    row_index = row_index,
    free_aesthetics_r = aes_subrow_return$free_aesthetics_r 
  )
  
  facet_subrow_return <- shiny::callModule(
    module = facet_subrow,
    id = "id_facet_subrow",
    .values = .values,
    data_r = data_r,
    row_index = row_index
  )
  
  coord_subrow_return <- shiny::callModule(
    module = coord_subrow,
    id = "id_coord_subrow",
    .values = .values,
    data_r = data_r,
    row_index = row_index
  )
  
  theme_subrow_return <- shiny::callModule(
    module = theme_subrow,
    id = "id_theme_subrow",
    .values = .values,
    data_r = data_r,
    row_index = row_index
  )
  
  return_list <- list(
    data_r = data_r,
    plot_r = plot_r
  )
}