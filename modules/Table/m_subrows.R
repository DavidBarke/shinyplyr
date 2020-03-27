m_subrows_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    # subrow-start and subrow-end are pseudo elements, that don't contain
    # anything but provide anchors for inserting and removing subrows
    htmltools::div(
      class = "subrow-start",
      id = ns("subrow_start")
    ),
    htmltools::div(
      class = "subrow-end",
      id = ns("subrow-end")
    )
  )
}

m_subrows <- function(
  input, output, session, .values, content_ui, content_server, row_index, add_r,
  subrow_class = NULL, show_r = function() NULL, hide_r = function() NULL,
  toggle_r = function() NULL
) {
  
  ns <- session$ns
  
  rvs <- shiny::reactiveValues(
    # n_subrow holds number of all subrows, even if removed
    n_subrow = 0,
    # active_subrow holds indices of not removed subrows
    active_subrows = numeric()
  )
  
  subrow_return_env <- new.env()
  
  shiny::observeEvent(add_r(), {
    rvs$n_subrow <- rvs$n_subrow + 1
    
    n <- rvs$n_subrow
    
    rvs$active_subrows <- c(rvs$active_subrows, n)
    
    # Insert new subrow
    shiny::insertUI(
      selector = paste0("#", ns("subrow-end")),
      where = "beforeBegin",
      ui = m_subrow_ui(
        id = ns("id_m_subrow") %_% n,
        class = paste(subrow_class, "active-subrow"),
        index = paste(row_index, n, sep = ".")
      )
    )
    
    # Call new subrow server
    subrow_return_env[["subrow" %_% n]] <- shiny::callModule(
      module = m_subrow,
      id = "id_m_subrow" %_% n,
      .values = .values,
      content_ui = content_ui,
      content_server = content_server
    )
    
    # Listen to remove button inside of subrow
    shiny::observeEvent(subrow_return_env[["subrow" %_% n]]$remove_r(), {
      shiny::removeUI(
        selector = paste0("#", ns("id_m_subrow") %_% n)
      )
      
      rvs$active_subrows <- rvs$active_subrows[rvs$active_subrows != n]
      
      # Show label of new last subrow
      last_index <- dplyr::last(rvs$active_subrows)
      
      if (!is.na(last_index)) {
        shinyjs::addClass(
          class = "active-subrow",
          selector = paste0("#", ns("id_m_subrow") %_% prev_index)
        )
      }
    })
    
    # Hide label of previous subrow
    prev_index <- dplyr::nth(rvs$active_subrows, -2)
    
    if (!is.na(prev_index)) {
      shinyjs::removeClass(
        class = "active-subrow",
        selector = paste0("#", ns("id_m_subrow") %_% prev_index)
      )
    }
  })
  
  subrow_selector <- paste0("#", ns("subrow_start"), " ~ .subrow-container")
  
  shiny::observeEvent(hide_r(), {
    shinyjs::hide(
      anim = TRUE,
      selector = subrow_selector
    )
  })
  
  shiny::observeEvent(show_r(), {
    shinyjs::show(
      anim = TRUE,
      selector = subrow_selector
    )
  })
  
  shiny::observeEvent(toggle_r(), {
    if (toggle_r() %% 2 == 0) {
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
    n_active_subrows_r = shiny::reactive({
      print(rvs$active_subrows)
      length(rvs$active_subrows)
    })
  )
  
  return(return_list)
}