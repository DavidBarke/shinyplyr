type_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("op_container"),
    class = "type-op-container"
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
  input, output, session, .values, data_r, row_index, subrows_open_r
) {
  
  ns <- session$ns
  
  output$op_container <- shiny::renderUI({
    if (subrows_open_r()) {
      htmltools::div(
        class = "type-op-sr-open grid-gap",
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
      shiny::uiOutput(
        outputId = ns("type_overview"),
        class = "type-op-sr-closed grid-vertical-center"
      )
    }
  })
  
  col_names_r <- shiny::reactive({
    names(data_r())
  })
  
  old_types_r <- shiny::reactive({
    purrr::map_chr(data_r(), function(col) pillar::type_sum(col))
  })
  
  old_type_names_r <- shiny::reactive({
    purrr::map_chr(old_types_r(), function(type) {
      if (!type %in% .values$TYPE_DATA$type) {
        "unknown"
      } else {
        .values$TYPE_DATA$name[.values$TYPE_DATA$type == type]
      }
    })
  })
  
  output$type_overview <- shiny::renderUI({
    repl <- purrr::map2_chr(col_names_r(), new_type_names_r(), function(col_name, type_name) {
      paste(col_name, type_name, sep = ": ")
    })
    paste(repl, collapse = ", ")
  })
  
  allowed_type_data <- dplyr::filter(.values$TYPE_DATA, allowed == TRUE)
  allowed_types <- allowed_type_data$type
  names(allowed_types) <- allowed_type_data$name
  
  output$subrows <- shiny::renderUI({
    ui <- purrr::pmap(
      list(
        col = col_names_r(), old_type = old_types_r(),
        old_type_name = old_type_names_r(), index = seq_along(col_names_r())
      ), 
      function(col, old_type, old_type_name, index) {
        # Only columns of allowed types can be changed to another allowed type
        if (old_type %in% allowed_types) {
          new_type_ui <- shiny::selectInput(
            inputId = ns("new_type" %_% index),
            label = NULL,
            choices = list(
              "Select a new type" = as.list(allowed_types)
            ),
            selected = old_type
          )
        } else {
          new_type_ui <- old_type
        }
        
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
              col
            ),
            htmltools::div(
              class = "grid-vertical-center",
              old_type_name
            ),
            htmltools::div(
              new_type_ui
            )
          )
        )
      }
    )
    
    ui
  })
  
  # Indices of column, that have allowed type
  allowed_indices_r <- shiny::reactive({
    which(old_types_r() %in% allowed_types)
  })
  
  new_types_r <- shiny::reactive({
    purrr::map_chr(seq_along(col_names_r()), function(index) {
      if (index %in% allowed_indices_r()) {
        shiny::req(input[["new_type" %_% index]])
      } else {
        old_types_r()[index]
      }
    })
  })
  
  new_type_names_r <- shiny::reactive({
    purrr::map_chr(new_types_r(), function(type) {
      if (!type %in% .values$TYPE_DATA$type) {
        "unknown"
      } else {
        .values$TYPE_DATA$name[.values$TYPE_DATA$type == type]
      }
    })
  })
  
  # Walk only over columns, whose type changed
  diff_indices_r <- shiny::reactive({
    which(new_types_r() != old_types_r())
  })
  
  typed_data_r <- shiny::reactive({
    new_types <- new_types_r()
    
    data <- data_r()
  
    for (index in diff_indices_r()) {
      type_index <- which(.values$TYPE_DATA$type == new_types_r()[index])
      
      type_fun <- .values$TYPE_DATA$convert_fun[[type_index]]
      
      data[[index]] <- type_fun(data[[index]])
    }
    
    data
  })
  
  return_list <- list(
    data_r = typed_data_r
  )
  
  return(return_list)
}