rename_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "rename-op-container grid-gap",
    htmltools::div(
      class = "grid-vertical-center",
      htmltools::tags$label(
        "Old name"
      )
    ),
    htmltools::div(
      class = "grid-vertical-center",
      htmltools::tags$label(
        "New name"
      )
    )
  )
}

rename_subrows_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("subrows"),
    class = "subrows rename-subrows"
  )
}

rename_operation <- function(
  input, output, session, .values, data_r, row_index, sr_toggle_rv, row_html_id
) {
  
  ns <- session$ns
  
  old_names_r <- shiny::reactive({
    names(data_r())
  })
  
  output$subrows <- shiny::renderUI({
    ui <- purrr::map2(old_names_r(), seq_along(old_names_r()), function(old_name, index) {
      htmltools::div(
        class = "subrow-container",
        htmltools::div(
          class = "subrow-content",
          htmltools::div(
            class = "grid-vertical-center",
            old_name
          ),
          htmltools::div(
            shiny::textInput(
              inputId = ns("new_name" %_% index),
              label = NULL,
              value = old_name
            )
          )
        )
      )
    })
    
    htmltools::tagList(
      htmltools::div(
        class = "subrow-start",
        id = ns("subrow_start")
      ),
      ui,
      htmltools::div(
        class = "subrow-end",
        id = ns("subrow-end")
      )
    )
  })
  
  new_names_r <- shiny::reactive({
    new_names <- purrr::map_chr(seq_along(old_names_r()), function(index) {
      shiny::req(!purrr::is_null(input[["new_name" %_% index]]))
      input[["new_name" %_% index]]
    })
    
    setNames(old_names_r(), new_names)
  })
  
  renamed_data_r <- shiny::reactive({
    data_r() %>%
      dplyr::rename(new_names_r())
  })
  
  subrow_selector <- paste0("#", ns("subrow_start"), " ~ .subrow-container")
  
  shiny::observeEvent(sr_toggle_rv(), {
    if (sr_toggle_rv() %% 2 == 0) {
      shinyjs::show(
        anim = TRUE,
        selector = subrow_selector
      )
    } else {
      shinyjs::hide(
        anim = TRUE,
        selector = subrow_selector
      )
    }
  })
  
  return_list <- list(
    data_r = renamed_data_r
  )
  
  return(return_list)
}