help_plot_facet_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The facet layer allows to select up two faceting columns (preferably factors).
      The user interface consists of an input for selecting the type of faceting:",
      htmltools::tags$ul(
        htmltools::tags$li(
          "none: No faceting"
        ),
        htmltools::tags$li(
          "grid: Faceting with ggplot2::facet_grid"
        ),
        htmltools::tags$li(
          "wrap: Faceting with ggplot2::facet_wrap"
        )
      )
    )
  )
}

help_plot_facet <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}