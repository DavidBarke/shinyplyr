facet_subrow_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "plot-subrow-content",
    shiny::uiOutput(
      outputId = ns("index"),
      class = "grid-center"
    ),
    shiny::uiOutput(
      outputId = ns("content"),
      class = "grid-gap facet-content"
    )
  )
}

facet_subrow <- function(
  input, output, session, .values, data_r, row_index
) {
  
  ns <- session$ns
  
  subrow_index <- paste(row_index, 3, sep = ".")
  
  output$index <- shiny::renderUI({
    subrow_index
  })
  
  output$content <- shiny::renderUI({
    htmltools::tagList(
      # Toggle sr
      htmltools::div(),
      htmltools::div(
        class = "grid-vertical-center",
        htmltools::tags$b(
          "Facet"
        )
      ),
      shiny::selectInput(
        inputId = ns("facet_type"),
        label = NULL,
        choices = list(
          "Select a facet type" = list(
            "none", "grid", "wrap"
          )
        )
      ),
      shiny::uiOutput(
        outputId = ns("facets")
      )
    )
  })
  
  choices_r <- shiny::reactive({
    names(data_r())
  })
  
  output$facets <- shiny::renderUI({
    shiny::req(input$facet_type != "none")
    shiny::selectInput(
      inputId = ns("facet_vars"),
      label = NULL,
      choices = list(
        "Select up to two facet variables" = as.list(
          choices_r()
        )
      ),
      multiple = TRUE
    )
  })
  
  debounced_facet_vars_r <- shiny::reactive({
    input$facet_vars
  }) %>% debounce(1000)
  
  shiny::observeEvent(debounced_facet_vars_r(), {
    if (length(input$facet_vars) > 2) {
      shiny::updateSelectInput(
        session = session,
        inputId = "facet_vars",
        selected = input$facet_vars[1:2]
      )
    }
  })
  
  safe_facet_vars_r <- shiny::reactive({
    if (length(shiny::req(input$facet_vars)) > 2) {
      return(input$facet_vars[1:2])
    } else {
      return(input$facet_vars)
    }
  })
  
  facet_r <- shiny::reactive({
    type <- shiny::req(input$facet_type)
    if (purrr::is_null(input$facet_vars)) type <- "none"
    
    switch(
      type,
      none = ggplot2::facet_null(),
      grid = {
        if (length(safe_facet_vars_r()) == 1) {
          rows <- safe_facet_vars_r()
          rows <- sym(rows)
          ggplot2::facet_grid(rows = vars(!!rows))
        } else {
          rows <- safe_facet_vars_r()[1]
          cols <- safe_facet_vars_r()[2]
          rows <- sym(rows)
          cols <- sym(cols)
          ggplot2::facet_grid(rows = vars(!!rows), cols = vars(!!cols))
        }
      },
      wrap = ggplot2::facet_wrap(safe_facet_vars_r())
    )
  })
  
  return_list <- list(
    facet_r = facet_r
  )
  
  return(return_list)
}