plot_output_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("message")
    ),
    shiny::uiOutput(
      outputId = ns("connected_state")
    ),
    shiny::plotOutput(
      outputId = ns("plot")
    )
  )
}

plot_output <- function(
  input, output, session, .values, plot_r, dataset_object, tab_value, message_r
) {
  
  force(tab_value)
  
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
  
  observe({
    if (!is_connected_r()) {
      .values$viewer$remove_tab(tab_value)
    }
  })
  
  output$message <- shiny::renderUI({
    htmltools::div(message_r())
  })
  
  output$plot <- shiny::renderPlot({
    shiny::req(purrr::is_null(message_r()))
    shiny::req(is_connected_r())
    shiny::req(plot_r())
    plot_r()
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