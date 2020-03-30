geom_content_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("toggle_btn"),
      class = "sr-toggle-btn grid-vertical-center"
    ),
    htmltools::div(
      class = "grid-vertical-center",
      htmltools::tags$b(
        "Geometry"
      )
    ),
    shiny::uiOutput(
      outputId = ns("n_var")
    ),
    shiny::uiOutput(
      outputId = ns("geom")
    )
  )
}

geom_content <- function(
  input, output, session, .values, data_r
) {
  
  ns <- session$ns
  
  # Toggle geom subrows --------------------------------------------------------
  output$toggle_btn <- shiny::renderUI({
    m_action_button(
      inputId = ns("sr_toggle"),
      label = NULL,
      icon = toggled_icon_r()
    )
  })
  
  toggle_rv <- shiny::reactiveVal(0)
  
  toggled_icon_r <- shiny::reactive({
    if (toggle_rv() %% 2 == 0) {
      shiny::icon("caret-down")
    } else {
      shiny::icon("caret-right")
    }
  })
  
  shiny::observeEvent(input$sr_toggle, {
    toggle_rv(toggle_rv() + 1)
  })
  
  # Outputs --------------------------------------------------------------------
  names_r <- shiny::reactive({
    names(data_r())
  })
  
  output$n_var <- shiny::renderUI({
    choices <- 2
    choices <- choices[choices <= length(names_r())]
    
    shiny::selectInput(
      inputId = ns("n_var"),
      label = NULL,
      choices = list(
        "Number of Variables" = list(choices)
      )
    )
  })
  
  geom_choices <- list(
    "1" = list(
      "Continuous" = list(
        "dotplot",
        "histogram"
      ),
      "Discrete" = list(
        "bar"
      )
    ), 
    "2" = list(
      "Graphical Primitives" = list(
        "path"
      ),
      "Continuous X, Continuous Y" = list(
        "point",
        "smooth"
      ),
      "Continous Bivariate Distribution" = list(
        "bin2d",
        "density2d",
        "hex"
      ),
      "Continuous Function" = list(
        "area",
        "line",
        "step"
      )
    ),
    "3" = list(
      "contour",
      "raster",
      "tile"
    )
  )
  
  geom_choices_r <- shiny::reactive({
    geom_choices[[shiny::req(input$n_var)]]
  })
  
  output$geom <- shiny::renderUI({
    shiny::selectInput(
      inputId = ns("geom"),
      label = NULL,
      choices = geom_choices_r()
    )
  })
  
  geom_fun_r <- shiny::reactive({
    switch(
      shiny::req(input$geom),
      "area" = ggplot2::geom_area,
      "bin2d" = ggplot2::geom_bin2d,
      "density2d" = ggplot2::geom_density2d,
      "hex" = ggplot2::geom_hex,
      "histogram" = ggplot2::geom_histogram,
      "line" = ggplot2::geom_line,
      "path" = ggplot2::geom_path,
      "point" = ggplot2::geom_point,
      "smooth" = ggplot2::geom_smooth,
      "step" = ggplot2::geom_step
    )
  })
  
  return_list <- list(
    toggle_rv = toggle_rv,
    geom_r = shiny::reactive(shiny::req(input$geom)),
    geom_fun_r = geom_fun_r,
    n_var_r = shiny::reactive(as.integer(shiny::req(input$n_var)))
  )
  
  return(return_list)
}