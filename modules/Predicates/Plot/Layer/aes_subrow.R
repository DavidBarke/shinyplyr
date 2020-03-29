aes_subrow_ui <- function(id, row_index) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "subrow-container aes-subrow-container with-subrows aes-subrows-open",
    id = ns("aes_subrow_container"),
    shiny::uiOutput(
      outputId = ns("index"),
      class = "subrow-index grid-center"
    ),
    shiny::uiOutput(
      outputId = ns("content"),
      class = "subrow-content grid-gap"
    ),
    htmltools::div(
      class = "subrow-remove grid-center"
    ),
    shiny::uiOutput(
      outputId = ns("subrows"),
      class = "subrows"
    )
  )
}

aes_subrow <- function(
  input, output, session, .values, data_r, row_index, geom_r
) {
  
  ns <- session$ns
  
  # Toggle aes subrows ---------------------------------------------------------
  output$subrows_toggle_btn <- shiny::renderUI({
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
  
  aes_subrows_selector <- paste0("#", ns("subrows"))
  aes_subrow_container_selector <- paste0("#", ns("aes_subrow_container"))
  
  shiny::observeEvent(toggle_rv(), {
    if (toggle_rv() %% 2 == 0) {
      shinyjs::show(
        anim = F,
        selector = aes_subrows_selector
      )
      
      shinyjs::addClass(
        class = "aes-subrows-open",
        selector = aes_subrow_container_selector
      )
    } else {
      shinyjs::hide(
        anim = F,
        selector = aes_subrows_selector
      )
      
      shinyjs::removeClass(
        class = "aes-subrows-open",
        selector = aes_subrow_container_selector
      )
    }
  })
  
  # Outputs --------------------------------------------------------------------
  choices_r <- shiny::reactive({
    names(data_r())
  })
  
  subrow_index <- paste(row_index, 1, sep = ".")
  
  output$index <- shiny::renderUI({
    subrow_index
  })
  
  aes_r <- shiny::reactive({
    unlist(properties(geom_r()))
  })
  
  properties_r <- shiny::reactive({
    properties(geom_r())
  })
  
  required_aes_r <- shiny::reactive({
    properties_r()$required
  })
  
  optional_aes_r <- shiny::reactive({
    properties_r()$optional
  })
  
  output$subrows <- shiny::renderUI({
    purrr::map2(aes_r(), seq_along(aes_r()), function(aes, index) {
      choices <- choices_r()
      
      if (aes %in% required_aes_r()) {
        selected <- choices_r()[index]
      } else {
        selected <- NULL
        choices <- c("NULL", choices_r())
      }
      
      htmltools::div(
        class = "subrow-container grid-gap m-index",
        htmltools::div(
          class = "subrow-index grid-center",
          paste(subrow_index, index, sep = ".")
        ),
        htmltools::div(
          class = "subrow-content aes-subrow-content grid-gap",
          htmltools::div(
            class = "grid-vertical-center",
            aes
          ),
          htmltools::div(
            shiny::selectInput(
              inputId = ns(aes %_% "value"),
              label = NULL,
              choices = choices,
              selected = selected
            )
          )
        ),
        htmltools::div(
          class = "subrow-remove"
        )
      )
    })
  })
  
  output$content <- shiny::renderUI({
    ui <- if (toggle_rv() %% 2 == 0) {
      htmltools::tagList(
        htmltools::div(
          class = "grid-vertical-center",
          htmltools::tags$label(
            "Aesthetic"
          )
        ),
        htmltools::div(
          class = "grid-vertical-center",
          htmltools::tags$label(
            "Column"
          )
        )
      )
    } else {
      aes_name_val <- purrr::map_chr(aes_r(), function(aes) {
        paste(aes, shiny::req(input[[aes %_% "value"]]), sep = ": ")
      })
      
      htmltools::tagList(
        htmltools::div(
          class = "grid-vertical-center",
          htmltools::tags$b(
            "Aesthetic"
          )
        ),
        htmltools::div(
          class = "grid-vertical-center",
          paste(aes_name_val, collapse = ", ")
        )
      )
    }
    
    htmltools::tagList(
      htmltools::div(
        shiny::uiOutput(
          outputId = ns("subrows_toggle_btn"),
          class = "sr-toggle-btn"
        )
      ),
      ui
    )
  })
  
  selected_aesthetics_r <- shiny::reactive({
    purrr::map_chr(aes_r(), function(aes) {
      shiny::req(input[[aes %_% "value"]])
    })
  })
  
  free_aesthetics_r <- shiny::reactive({
    setdiff(
      aes_r()[selected_aesthetics_r() == "NULL"],
      "group"
    )
  })
  
  return_list <- list(
    free_aesthetics_r = free_aesthetics_r
  )
  
  return(return_list)
}