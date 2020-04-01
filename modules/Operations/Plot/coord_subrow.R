coord_subrow_ui <- function(id) {
  ns <- shiny::NS(id)
  
  coords <- list(
    Cartesian = "cartesian",
    "Fixed aspect ratio" = "fixed",
    Flip = "flip",
    Polar = "polar"
  )
  
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
      ),
      shiny::uiOutput(
        outputId = ns("details")
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
  
  output$details <- shiny::renderUI({
    shiny::req(input$coord %in% c("fixed", "polar"))
    
    if (input$coord == "fixed") {
      shiny::numericInput(
        inputId = ns("aspect_ratio"),
        label = NULL,
        value = 1,
        min = 0,
        step = 0.5
      )
    } else {
      shiny::selectInput(
        inputId = ns("angular_variable"),
        label = NULL,
        choices = list("Angular Variable" = list("x", "y"))
      )
    }
  })
  
  safe_aspect_ratio_r <- shiny::reactive({
    value <- safe_numeric_input_value(shiny::req(input$aspect_ratio))
    max(value, 0.001)
  })
  
  debounced_aspect_ratio_r <- shiny::reactive({
    shiny::req(input$aspect_ratio)
  }) %>% shiny::debounce(1000)
  
  shiny::observeEvent(debounced_aspect_ratio_r(), {
    if (shiny::req(input$aspect_ratio) <= 0) {
      shiny::updateNumericInput(
        session = session,
        inputId = "aspect_ratio",
        value = 1
      )
    }
  })
  
  coord_r <- shiny::reactive({
    switch(
      shiny::req(input$coord),
      "cartesian" = ggplot2::coord_cartesian(),
      "fixed" = ggplot2::coord_fixed(ratio = safe_aspect_ratio_r()),
      "flip" = ggplot2::coord_flip(),
      "polar" = ggplot2::coord_polar(theta = shiny::req(input$angular_variable))
    )
  })
  
  shiny::observeEvent(input$help_plot_coord, {
    .values$help$open("plot_coord")
  })
  
  return_list <- list(
    coord_r = coord_r
  )
}