help_plot_coord_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    "The coordinate layer specifies the plot's coordinate system. The following
    table explains the available choices:",
    htmltools::tags$table(
      htmltools::tags$thead(
        htmltools::tags$th("Coordinate System"),
        htmltools::tags$th("Description")
      ),
      htmltools::tags$tbody(
        htmltools::tags$tr(
          htmltools::tags$td("Cartesian"),
          htmltools::tags$td(
            "Cartesian coordinate system"
          )
        ),
        htmltools::tags$tr(
          htmltools::tags$td("Fixed"),
          htmltools::tags$td(
            "Cartesian coordinate system with fixed aspect ratio"
          )
        ),
        htmltools::tags$tr(
          htmltools::tags$td("Flip"),
          htmltools::tags$td(
            "Cartesian coordinate system with x and y flipped"
          )
        ),
        htmltools::tags$tr(
          htmltools::tags$td("Polar"),
          htmltools::tags$td(
            "Polar coordinate system"
          )
        )
      )
    )
  )
}

help_plot_coord <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}