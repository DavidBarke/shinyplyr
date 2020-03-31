help_plot_geom_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The geometry layer determines how the mapped aesthetics are plotted. The
      user interface consists of two inputs for selecting the number of required
      aesthetics and the specific geometry and a table for mapping non-specified
      optional aesthetics to a static value. All required aesthetics must be mapped
      to a column of the dataset in the",
      shiny::actionLink(
        inputId = ns("help_plot_aes"),
        label = "aesthetics layer."
      ),
      "The choices for the static values are dependent on the optional aesthetic."
    ),
    htmltools::h4(
      "Limitations"
    ),
    htmltools::p(
      "Unlike in ggplot2 it is currently only possible to add exactly one geometry to a plot."
    )
  )
}

help_plot_geom <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$help_plot_aes, {
    .values$help$open("plot_aes")
  })
}