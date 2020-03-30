plot_output_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("connected_state")
    ),
    shiny::plotOutput(
      outputId = ns("plot")
    )
  )
}

plot_output <- function(
  input, output, session, .values, plot_r, dataset_object
) {
  
  ns <- session$ns
  
  rvs <- shiny::reactiveValues(
    static_plot = NULL
  )
  
  name_r <- shiny::reactive({
    dataset_object$get_name()
  })
  
  id_r <- shiny::reactive({
    dataset_object$get_id()
  })
  
  is_connected_r <- shiny::reactive({
    .values$dataset_id_rv() == id_r()
  })
  
  safe_plot_r <- shiny::reactive({
    if (is_connected_r()) {
      plot_r()
    } else {
      rvs$static_plot
    }
  })
  
  shiny::observeEvent(plot_r(), {
    if (is_connected_r()) {
      rvs$static_plot <- plot_r()
    }
  })
  
  output$plot <- shiny::renderPlot({
    safe_plot_r()
  })
  
  output$connected_state <- shiny::renderUI({
    shiny::req(!is_connected_r())
    htmltools::tags$b(glue::glue(
      "Plot is temporarily disconnected, because the selected dataset is not
      equal to {name_r()}. If you select {name_r()}, the connection will be
      reestablished." 
    ))
  })
}