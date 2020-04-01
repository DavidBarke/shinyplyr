coord_subrow_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "plot-subrow-content",
    shiny::uiOutput(
      outputId = ns("index"),
      class = "grid-center"
    ),
    htmltools::div(
      class = "grid-gap coord-content",
      # Toggle sr
      htmltools::div(),
      htmltools::tags$b(
        class = "grid-vertical-center",
        "Coordinates"
      ),
      htmltools::tags$div(
        class = "grid-center",
        help_button(ns("help_plot_coord"))
      ),
      shiny::selectInput(
        inputId = ns("coord"),
        label = NULL,
        choices = list(
          "Select a coordinate system" = coords
        )
      )
    )
  )
}

coord_subrow <- function(
  input, output, session, .values, data_r, row_index
) {
  
  ns <- session$ns
  
  subrow_index <- paste(row_index, 4, sep = ".")
  
  output$index <- shiny::renderUI({
    subrow_index
  })
  
  coords <- list(
    cartesian = "cartesian",
    "fixed aspect ratio" = "fixed",
    flip = "flip",
    polar = "polar"
  )
  
  coord_fun_r <- shiny::reactive({
    switch(
      shiny::req(input$coord),
      "cartesian" = ggplot2::coord_cartesian,
      "fixed" = ggplot2::coord_fixed,
      "flip" = ggplot2::coord_flip,
      "polar" = ggplot2::coord_polar
    )
  })
  
  shiny::observeEvent(input$help_plot_coord, {
    .values$help$open("plot_coord")
  })
  
  return_list <- list(
    coord_fun_r = coord_fun_r
  )
}