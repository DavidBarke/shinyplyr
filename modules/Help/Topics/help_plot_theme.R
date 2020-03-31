help_plot_theme_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "In the theme layer the overall theme of the plot can be selected."
    )
  )
}

help_plot_theme <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}