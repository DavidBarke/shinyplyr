help_plot_aes_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The aesthetic layer maps columns of the dataset to aesthetics of the plot.
      Clicking on the triangle right next to the index toggles the table, which
      is used for executing this mapping. The choices for the aesthetics are
      dependent on the selected geometry in the
      ",
      shiny::actionLink(
        inputId = ns("help_plot_geom"),
        label = "geometry layer."
      ),
      "A distinction is made between required and optional aesthetics. Required
      aesthetics are 'x', 'y' and 'z'. These have to be mapped to a column. 
      Optional aesthetics may be mapped to a column or to 'NULL'. If they are mapped
      to 'NULL', this aesthetic may be set to a static value in the geometry layer."
    )
  )
}

help_plot_aes <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$help_plot_geom, {
    .values$help$open("plot_geom")
  })
}