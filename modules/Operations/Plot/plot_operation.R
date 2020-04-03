plot_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "plot-op-container grid-gap",
    htmltools::div(
      shiny::uiOutput(
        outputId = ns("n_var")
      )
    )
  )
}

plot_subrows_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    aes_subrow_ui(
      id = ns("id_aes_subrow")
    ),
    geom_subrow_ui(
      id = ns("id_geom_subrow")
    ),
    facet_subrow_ui(
      id = ns("id_facet_subrow")
    ),
    coord_subrow_ui(
      id = ns("id_coord_subrow")
    ),
    theme_subrow_ui(
      id = ns("id_theme_subrow")
    )
  )
}

plot_operation <- function(
  input, output, session, .values, data_r, row_index, row_container_id
) {
  
  ns <- session$ns
  
  names_r <- shiny::reactive({
    names(data_r())
  })
  
  output$n_var <- shiny::renderUI({
    choices <- 1:2
    choices <- choices[choices <= length(names_r())]
    
    shiny::selectInput(
      inputId = ns("n_var"),
      label = NULL,
      choices = list(
        "Number of Variables" = as.list(choices)
      )
    )
  })
  
  plot_r <- shiny::reactive({
    ggplot(data_r(), layer_returns$aes$aes_r()) +
      layer_returns$geom$geom_xxx_r() +
      layer_returns$facet$facet_r() +
      layer_returns$coord$coord_r() + 
      layer_returns$theme$theme_r()
  })
  
  message_r <- shiny::reactive({
    messages <- purrr::map2(layer_returns, names(layer_returns), function(layer, name) {
      if (purrr::is_null(layer$message_r)) return(NULL)
      if (purrr::is_null(layer$message_r())) return(NULL)
      
      full_name <- .values$plot$LAYER$name[.values$plot$LAYER$layer == name]
      
      htmltools::tags$tr(
        htmltools::tags$td(full_name),
        htmltools::tags$td(layer$message_r())
      )
    })
    
    are_null <- purrr::map_lgl(messages, purrr::is_null)
    
    if (all(are_null)) return(NULL)
    
    htmltools::tags$table(
      htmltools::tags$thead(
        htmltools::tags$th("Layer"),
        htmltools::tags$th("Message")
      ),
      htmltools::tags$tbody(
        messages
      )
    )
  })
  
  n_var_r <- shiny::reactive(as.numeric(shiny::req(input$n_var)))
  
  layer_returns <- list(
    aes = shiny::callModule(
      module = aes_subrow,
      id = "id_aes_subrow",
      .values = .values,
      data_r = data_r,
      row_index = row_index,
      geom_r = layer_returns$geom$geom_r,
      n_var_r = n_var_r
    ),
    geom = shiny::callModule(
      module = geom_subrow,
      id = "id_geom_subrow",
      .values = .values,
      data_r = data_r,
      row_index = row_index,
      free_aesthetics_r = layer_returns$aes$free_aesthetics_r,
      n_var_r = n_var_r
    ),
    facet = shiny::callModule(
      module = facet_subrow,
      id = "id_facet_subrow",
      .values = .values,
      data_r = data_r,
      row_index = row_index
    ),
    coord = shiny::callModule(
      module = coord_subrow,
      id = "id_coord_subrow",
      .values = .values,
      data_r = data_r,
      row_index = row_index
    ),
    theme = shiny::callModule(
      module = theme_subrow,
      id = "id_theme_subrow",
      .values = .values,
      data_r = data_r,
      row_index = row_index
    )
  )
  
  return_list <- list(
    data_r = data_r,
    message_r = message_r,
    plot_r = plot_r
  )
}