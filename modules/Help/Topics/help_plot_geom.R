help_plot_geom_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The geometry layer determines how the mapped aesthetics are plotted. The
      user interface consists of an input for selecting the specific geometry 
      and a table for mapping non-specified optional aesthetics to a fixed value. 
      All required aesthetics ('x' for one-, 'x' and 'y' for two-variable-geometries) 
      must be mapped to a column of the dataset in the",
      shiny::actionLink(
        inputId = ns("help_plot_aes"),
        label = "aesthetics layer."
      ),
      "The choices for the fixed values are dependent on the optional aesthetic."
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