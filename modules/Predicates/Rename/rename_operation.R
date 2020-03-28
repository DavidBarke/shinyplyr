rename_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("op_container"),
    class = "rename-op-container grid-gap"
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
  input, output, session, .values, data_r, row_index, row_container_id, sr_toggle_rv
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
  
  output$old_name <- shiny::renderUI({
    if (subrows_open_r()) {
      htmltools::tags$label(
        "Old name"
      )
    } else {
      paste("Old names:", paste(old_names_r(), collapse = ", "))
    }
  })
  
  output$new_name <- shiny::renderUI({
    if (subrows_open_r()) {
      htmltools::tags$label(
        "New name"
      )
    } else {
      paste("New names:", paste(new_names_r(), collapse = ", "))
    }
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
    
    ui <- htmltools::tagList(
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
    
    if (shiny::isolate(!subrows_open_r())) {
      print("hidden")
      return(shinyjs::hidden(ui))
    }
    
    ui
  })
  
  new_names_r <- shiny::reactive({
    new_names <- purrr::map_chr(seq_along(old_names_r()), function(index) {
      # Extra line is necessary, because req would stop when detecting ""
      shiny::req(!purrr::is_null(input[["new_name" %_% index]]))
      stringr::str_trim(input[["new_name" %_% index]])
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
  
  subrow_selector <- paste(row_container_id, " > .subrows-container")
  
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