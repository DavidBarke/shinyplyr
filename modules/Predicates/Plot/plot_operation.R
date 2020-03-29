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
    htmltools::div(
      class = "subrows plot-subrows",
      aes_subrow_ui(
        id = ns("id_aes_subrow")
      ),
      geom_subrow_ui(
        id = ns("id_geom_subrow")
      )
    )
  )
}

plot_operation <- function(
  input, output, session, .values, data_r, row_index, row_container_id, sr_toggle_rv
) {
  
  ns <- session$ns
  
  ggplot2_plot_r <- shiny::reactive({
    ggplot(mtcars, aes(x = mpg, y = cyl)) +
      geom_point()
  })
  
  plotly_plot_r <- shiny::reactive({
    plot_ly(mtcars, x = ~mpg, y = ~cyl, type = "scatter", mode = "markers")
  })
  
  plot_r <- ggplot2_plot_r
  
  aes_subrow_return <- shiny::callModule(
    module = aes_subrow,
    id = "id_aes_subrow",
    .values = .values,
    data_r = data_r,
    row_index = row_index,
    geom_r = geom_subrow_return$geom_r
  )
  
  geom_subrow_return <- shiny::callModule(
    module = geom_subrow,
    id = "id_geom_subrow",
    .values = .values,
    data_r = data_r,
    row_index = row_index,
    free_aesthetics_r = aes_subrow_return$free_aesthetics_r 
  )
  
  return_list <- list(
    data_r = data_r,
    plot_r = plot_r
  )
}