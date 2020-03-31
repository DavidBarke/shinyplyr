type_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("op_container"),
    class = "type-op-container grid-gap"
  )
}

type_subrows_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("subrows"),
    class = "subrows type-subrows"
  )
}

type_operation <- function(
  input, output, session, .values, data_r, row_index, sr_toggle_rv
) {
  
  ns <- session$ns
  
  subrows_open_r <- shiny::reactive({
    sr_toggle_rv() %% 2 == 0
  })
  
  shiny::observeEvent(subrows_open_r(), {
    # Op container's class is dependent on visible state of subrows
    selector <- paste0("#", ns("op_container"))
    
    if (subrows_open_r()) {
      shinyjs::removeClass(
        class = "subrows-closed",
        selector = selector
      )
    } else {
      shinyjs::addClass(
        class = "subrows-closed",
        selector = selector
      )
    }
  })
  
  output$op_container <- shiny::renderUI({
    if (subrows_open_r()) {
      htmltools::tagList(
        htmltools::div(
          class = "grid-vertical-center",
          htmltools::tags$b(
            "Column"
          )
        ),
        htmltools::div(
          class = "grid-vertical-center",
          htmltools::tags$b(
            "Old type"
          )
        ),
        htmltools::div(
          class = "grid-vertical-center",
          htmltools::tags$b(
            "New type"
          )
        )
      )
    } else {
      htmltools::div(
        class = "grid-vertical-center",
        shiny::uiOutput(
          outputId = ns("type_overview")
        )
      )
    }
  })
  
  choices_r <- shiny::reactive({
    names(data_r())
  })
  
  old_types_r <- shiny::reactive({
    purrr::map_chr(data_r(), function(col) class(col))
  })
  
  output$type_overview <- shiny::renderUI({
    repl <- purrr::map2_chr(choices_r(), old_types_r(), function(choice, type) {
      paste(choice, type, sep = ": ")
    })
    paste(repl, collapse = ", ")
  })
  
  output$subrows <- shiny::renderUI({
    ui <- purrr::pmap(
      list(choice = choices_r(), old_type = old_types_r(), index = seq_along(choices_r())), 
      function(choice, old_type, index) {
      htmltools::div(
        class = "subrow-container",
        htmltools::div(
          class = "subrow-index grid-center",
          paste(row_index, index, sep = ".")
        ),
        htmltools::div(
          class = "subrow-content grid-gap",
          htmltools::div(
            class = "grid-vertical-center",
            choice
          ),
          htmltools::div(
            class = "grid-vertical-center",
            old_type
          ),
          htmltools::div(
            shiny::textInput(
              inputId = ns("new_type" %_% index),
              label = NULL,
              value = old_type
            )
          )
        )
      )
    })
    
    if (shiny::isolate(!subrows_open_r())) {
      return(shinyjs::hidden(ui))
    }
    
    ui
  })
  
  return_list <- list(
    data_r = data_r
  )
  
  return(return_list)
}