plot_content_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "plot-content grid-gap",
    # Select layer
    htmltools::div(
      shiny::selectInput(
        inputId = ns("select_layer"),
        label = "Layer",
        choices = c(
          None = "none",
          Geometry = "geom",
          Facet = "facet",
          Coordinates = "coord",
          Theme = "theme"
        )
      )
    ),
    # Property
    shiny::uiOutput(
      outputId = ns("layer"),
      class = "plot-layer"
    )
  )
}

plot_content <- function(
  input, output, session, .values, data_r
) {
  
  ns <- session$ns
  
  # output$layer <- shiny::renderUI({
  #   layer_ui_list[[shiny::req(input$layer)]]
  # })
  # 
  # layer_ui_list <- list(
  #   "geom" = geom_layer_ui(
  #     id = ns("id_geom_layer")
  #   ),
  #   "facet" = facet_layer_ui(
  #     id = ns("id_facet_layer")
  #   ),
  #   "coord" = coord_layer_ui(
  #     id = ns("id_coord_layer"),
  #   ),
  #   "theme" = theme_layer_ui(
  #     id = ns("id_theme_layer")
  #   )
  # )
  # 
  # shiny::callModule(
  #   module = geom_layer,
  #   id = "id_geom_layer",
  #   .values = .values,
  #   data_r = data_r
  # )
  # 
  # shiny::callModule(
  #   module = facet_layer,
  #   id = "id_facet_layer",
  #   .values = .values,
  #   data_r = data_r
  # )
  # 
  # shiny::callModule(
  #   module = coord_layer,
  #   id = "id_coord_layer",
  #   .values = .values,
  #   data_r = data_r
  # )
  # 
  # shiny::callModule(
  #   module = theme_layer,
  #   id = "id_theme_layer",
  #   .values = .values,
  #   data_r = data_r
  # )
}