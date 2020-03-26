m_table_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    # plus button in uiOutput, so that it appears just when the module is ready.
    # Otherwise inconsistencies are possible, if the button is clicked early
    shiny::uiOutput(
      outputId = ns("plus_button")
    ),
    htmltools::tags$hr(),
    htmltools::div(
      id = ns("table_container"),
      class = "table-container",
      htmltools::div(
        class = "row-container",
        htmltools::div(
          class = "area-predicate table-title",
          "Predicate"
        ),
        htmltools::div(
          class = "area-operation table-title",
          htmltools::div(
            "Operation"
          )
        )
      ),
      htmltools::div(
        id = ns("row_0"),
        class = "row-container",
        htmltools::div(
          class = "area-predicate",
          "data"
        ),
        htmltools::div(
          class = "area-operation",
          htmltools::div(
            shiny::uiOutput(
              outputId = ns("select_dataset")
            )
          )
        )
      )
    ),
    htmltools::tags$hr(),
    m_action_button(
      inputId = ns("apply"),
      label = "Apply"
    )
  )
}

m_table <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  rvs <- shiny::reactiveValues(
    # Current row count
    n_row = 0,
    # Maximum row count
    max_row = 0,
    data = NULL
  )
  
  m_row_env <- new.env()
  
  dataset_choices_r <- shiny::reactive({
    .values$dataset_storage$get_ids()
  })
  
  output$select_dataset <- shiny::renderUI({
    shiny::selectInput(
      inputId = ns("selected_dataset"),
      label = NULL,
      choices = dataset_choices_r()
    )
  })
  
  data_r <- shiny::reactive({
    .values$dataset_storage$get_object(shiny::req(input$selected_dataset))$get_dataset()
  })
  
  shiny::observeEvent(data_r(), {
    purrr::walk(seq_len(rvs$n_row), function(row_index) {
      shiny::removeUI(
        selector = paste0("#", ns("row_"), row_index)
      )
    })
    
    rvs$n_row <- 0
    
    rvs$data <- data_r()
  })
  
  output$plus_button <- shiny::renderUI({
    m_action_button(
      inputId = ns("add_row"),
      label = NULL,
      icon = shiny::icon("plus")
    )
  })
  
  # Rows are added with insertUI. If a row with that index is added for the
  # first time, the server function is called exactly once
  shiny::observeEvent(input$add_row, {
    rvs$n_row <- rvs$n_row + 1
    
    ui <- m_row_ui(
      id = ns("id_m_row" %_% rvs$n_row),
      container_id = ns("row" %_% rvs$n_row)
    )
    
    shiny::insertUI(
      selector = paste0("#", ns("table_container")),
      where = "beforeEnd",
      ui = ui
    )
    
    if (rvs$n_row > rvs$max_row) {
      if (rvs$n_row == 1) {
        prev_data_r <- data_r
      } else {
        prev_data_r <- m_row_env[["m_row" %_% (rvs$n_row - 1)]]$data_r
      }
      
      rvs$max_row <- rvs$n_row
      
      m_row_env[["m_row" %_% rvs$n_row]] <- shiny::callModule(
        module = m_row,
        id = "id_m_row" %_% rvs$n_row,
        .values = .values,
        data_r = prev_data_r,
        row_index = rvs$n_row,
        remove_row_fun = remove_row_fun
      ) 
    }
  })
  
  # Debounce rvs$n_row, so that disabling is done with delay. If you click
  # very fast, you may get more than the last row enabled. 
  debounced_n_row_r <- shiny::reactive({
    rvs$n_row
  }) %>% debounce(500)
  
  shiny::observeEvent(debounced_n_row_r(), {
    if (rvs$n_row > 0) {
      # All but the last row get disabled
      purrr::walk(seq_len(rvs$n_row - 1), function(row_index) {
        shinyjs::hide(
          selector = paste0("#", ns("row_"), row_index, " > .area-remove")
        )
        
        shinyjs::disable(
          selector = paste0("#", ns("row_"), row_index)
        )
      })
    }
  })
  
  shiny::observeEvent(rvs$n_row, {
    # Last row gets enabled
    shinyjs::enable(
      selector = paste0("#", ns("row_"), rvs$n_row)
    )
    
    shinyjs::show(
      selector = paste0("#", ns("row_"), rvs$n_row, " > .area-remove")
    )
  })
  
  shiny::observeEvent(input$apply, {
    if (rvs$n_row == 0) {
      rvs$data <- data_r()
    } else {
      rvs$data <- m_row_env[["m_row" %_% rvs$n_row]]$data_r()
    }
  })
  
  remove_row_fun = function() {
    shiny::removeUI(
      selector = paste0("#", ns("row_"), rvs$n_row)
    )
    
    rvs$n_row <- rvs$n_row - 1
  } 
  
  return_list <- list(
    data_r = shiny::reactive(rvs$data)
  )
  
  return(return_list)
}
