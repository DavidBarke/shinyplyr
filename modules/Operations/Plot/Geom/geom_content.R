geom_content_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("toggle_btn"),
      class = "sr-toggle-btn grid-vertical-center"
    ),
    htmltools::tags$b(
      class = "grid-vertical-center",
      "Geometry"
    ),
    htmltools::div(
      class = "grid-center",
      help_button(ns("help_plot_geom"))
    ),
    shiny::uiOutput(
      outputId = ns("geom")
    )
  )
}

geom_content <- function(
  input, output, session, .values, data_r, n_var_r
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
  
  toggle_rv <- shiny::reactiveVal(1)
  
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
    geom_choices[[n_var_r()]]
  })
  
  output$geom <- shiny::renderUI({
    shiny::selectInput(
      inputId = ns("geom"),
      label = NULL,
      choices = geom_choices_r(),
      selected = c("histogram", "point")[n_var_r()]
    )
  })
  
  geom_default_r <- shiny::reactive({
    if (n_var_r() == 1) "histogram" else "point"
  })
  
  geom_r <- shiny::reactive(fallback(input$geom, geom_default_r()))
  
  geom_fun_r <- shiny::reactive({
    switch(
      geom_r(),
      "area" = ggplot2::geom_area,
      "bar" = ggplot2::geom_bar,
      "bin2d" = ggplot2::geom_bin2d,
      "dotplot" = ggplot2::geom_dotplot,
      "density2d" = ggplot2::geom_density_2d,
      "hex" = ggplot2::geom_hex,
      "histogram" = ggplot2::geom_histogram,
      "line" = ggplot2::geom_line,
      "path" = ggplot2::geom_path,
      "point" = ggplot2::geom_point,
      "smooth" = ggplot2::geom_smooth,
      "step" = ggplot2::geom_step
    )
  })
  
  shiny::observeEvent(input$help_plot_geom, {
    .values$help$open("plot_geom")
  })
  
  return_list <- list(
    toggle_rv = toggle_rv,
    geom_r = geom_r,
    geom_fun_r = geom_fun_r
  )
  
  return(return_list)
}