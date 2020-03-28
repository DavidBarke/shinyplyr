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
      m_subrows_ui(
        id = ns("id_m_subrows")
      )
    ),
    htmltools::div(
      class = "add-subrow grid-vertical-center",
      m_action_button(
        inputId = ns("add_subrow"),
        label = "Add layer",
        icon = shiny::icon("plus")
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
  
  # Subrows --------------------------------------------------------------------
  subrows_return <- shiny::callModule(
    module = m_subrows,
    id = "id_m_subrows",
    .values = .values,
    content_ui = plot_content_ui,
    content_server = plot_content,
    row_index = row_index,
    row_container_id = row_container_id,
    add_r = shiny::reactive(input$add_subrow),
    toggle_rv = sr_toggle_rv,
    index_offset = 1
  )
  
  shiny::callModule(
    module = aes_subrow,
    id = "id_aes_subrow",
    .values = .values,
    data_r = data_r,
    row_index = row_index
  )
  
  return_list <- list(
    data_r = data_r,
    plot_r = plot_r
  )
}