theme_subrow_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "plot-subrow-content",
    shiny::uiOutput(
      outputId = ns("index"),
      class = "grid-center"
    ),
    shiny::uiOutput(
      outputId = ns("content"),
      class = "grid-gap theme-content"
    )
  )
}

theme_subrow <- function(
  input, output, session, .values, data_r, row_index
) {
  
  ns <- session$ns
  
  subrow_index <- paste(row_index, 5, sep = ".")
  
  output$index <- shiny::renderUI({
    subrow_index
  })
  
  themes <- list(
    "bw", "classic", "dark", "gray", "grey", "light", "linedraw", "minimal"
  )
  
  output$content <- shiny::renderUI({
    htmltools::tagList(
      # Toggle sr
      htmltools::div(),
      htmltools::tags$b(
        class = "grid-vertical-center",
        "Theme"
      ),
      shiny::selectInput(
        inputId = ns("theme"),
        label = NULL,
        choices = list(
          Themes = themes
        )
      )
    )
  })
  
  theme_fun_r <- shiny::reactive({
    switch(
      shiny::req(input$theme),
      "bw" = ggplot2::theme_bw,
      "classic" = ggplot2::theme_classic,
      "dark" = ggplot2::theme_dark,
      "gray" = ggplot2::theme_gray,
      "grey" = ggplot2::theme_grey,
      "light" = ggplot2::theme_light,
      "linedraw" = ggplot2::theme_linedraw,
      "minimal" = ggplot2::theme_minimal
    )
  })
  
  return_list <- list(
    theme_fun_r = theme_fun_r
  )
}