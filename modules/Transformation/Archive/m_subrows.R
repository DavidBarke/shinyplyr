m_subrows_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::div(
      class = "subrow-end",
      id = ns("subrow_end")
    )
  )
}

m_subrows <- function(
  input, output, session, .values, content_ui, content_server, row_index, 
  row_container_id, add_r, subrow_class = NULL, toggle_rv = function() TRUE,
  index_offset = 0, ...
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
      selector = paste0("#", ns("subrow_end")),
      where = "beforeBegin",
      ui = m_subrow_ui(
        id = ns("id_m_subrow") %_% n,
        class = paste(subrow_class, "active-subrow")
      )
    )
    
    # Call new subrow server
    subrow_return_env[["subrow" %_% n]] <- shiny::callModule(
      module = m_subrow,
      id = "id_m_subrow" %_% n,
      .values = .values,
      content_ui = content_ui,
      content_server = content_server,
      index_r = shiny::reactive({
        i <- which(rvs$active_subrows == n)
        paste(row_index, i + index_offset, sep = ".")
      }),
      ...
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
          selector = paste0("#", ns("id_m_subrow") %_% last_index)
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
    
    # Expand subrows if collapsed
    if (toggle_rv() %% 2 == 1) {
      toggle_rv(toggle_rv() + 1)
    }
  })
  
  subrow_selector <- paste(row_container_id, "> .subrows-container")
  
  # Toggle expand or collapse of subrows
  shiny::observeEvent(toggle_rv(), {
    if (toggle_rv() %% 2 == 0) {
      shinyjs::show(
        anim = F,
        selector = subrow_selector
      )
    } else {
      shinyjs::hide(
        anim = F,
        selector = subrow_selector
      )
    }
  })
  
  return_list <- list(
    n_active_subrows_r = shiny::reactive(length(rvs$active_subrows)),
    subrows_open_r = shiny::reactive(toggle_rv() %% 2 == 0)
  )
  
  return(return_list)
}