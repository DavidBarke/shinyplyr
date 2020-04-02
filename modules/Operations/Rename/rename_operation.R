rename_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("op_container"),
    class = "rename-op-container grid-gap subrows-closed"
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
            "Old name"
          )
        ),
        htmltools::div(
          class = "grid-vertical-center",
          htmltools::tags$b(
            "New name"
          )
        )
      )
    } else {
      htmltools::div(
        class = "grid-vertical-center",
        shiny::uiOutput(
          outputId = ns("rename_overview")
        )
      )
    }
  })
  
  output$rename_overview <- shiny::renderUI({
    repl <- purrr::map2_chr(old_names_r(), new_names_r(), function(old_name, new_name) {
      if (new_name == "") {
        new_name <- "-"
      }
      paste(old_name, "=", new_name)
    })
    paste(repl, collapse = ", ")
  })
  
  old_names_r <- shiny::reactive({
    names(data_r())
  })
  
  output$subrows <- shiny::renderUI({
    ui <- purrr::map2(old_names_r(), seq_along(old_names_r()), function(old_name, index) {
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
    
    ui
  })
  
  new_names_r <- shiny::reactive({
    new_names <- purrr::map2_chr(old_names_r(), seq_along(old_names_r()), function(old_name, index) {
      # Extra line is necessary, because req would stop when detecting ""
      new_name <- fallback(input[["new_name" %_% index]], old_name)
      stringr::str_trim(new_name)
    })
  })
  
  is_empty_name_r <- shiny::reactive({
    new_names_r() == ""
  })
  
  non_empty_names_r <- shiny::reactive({
    new_names_r()[!is_empty_name_r()]
  })
  
  old_matched_names_r <- shiny::reactive({
    old_names_r()[!is_empty_name_r()]
  })
  
  rename_names_r <- shiny::reactive({
    setNames(old_matched_names_r(), non_empty_names_r())
  })
  
  renamed_data_r <- shiny::reactive({
    data_r() %>%
      dplyr::select(old_matched_names_r()) %>%
      dplyr::rename(rename_names_r())
  })
  
  return_list <- list(
    data_r = renamed_data_r
  )
  
  return(return_list)
}