help_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The plot operation allows you to create a plot based on the grammar of
      graphics used by the ggplot2 package. with the grammar of graphics each
      plot consists of several layers. The plot operation is in fact divided in
      five suboperations. Each suboperation represents one layer. For details see
      the help page of the respective layer:"
    ),
    shiny::uiOutput(
      outputId = ns("layer_links"),
      container = htmltools::tags$ul
    ),
    htmltools::h4(
      "Note"
    ),
    htmltools::p(
      "The data layer of ggplot2 is omitted for obvious reasons, because the 
      dataset from the previous",
      shiny::actionLink(
        inputId = ns("help_transformation"),
        label = "transformation step"
      ),
      "is taken. Furthermore the statistics layer is currently not implemented."
    )
  )
}

help_plot <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  output$layer_links <- shiny::renderUI({
    purrr::map2(.values$plot$LAYER$layer, .values$plot$LAYER$name, function(layer, name) {
      htmltools::tags$li(
        shiny::actionLink(
          inputId = ns("link" %_% layer),
          label = paste(name, "layer")
        )
      )
    })
  })
  
  purrr::walk(.values$plot$LAYER$layer, function(layer) {
    shiny::observeEvent(input[["link" %_% layer]], {
      .values$help$open("plot" %_% layer)
    })
  })
  
  shiny::observeEvent(input$help_transformation, {
    .values$help$open("transformation")
  })
}