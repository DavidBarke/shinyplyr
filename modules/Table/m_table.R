m_table_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    # Header
    htmltools::div(
      id = ns("table_container"),
      class = "table-container",
      htmltools::div(
        class = "row-container",
        htmltools::div(
          class = "row-content",
          # Index
          htmltools::div(),
          # sr-toggle
          htmltools::div(),
          htmltools::div(
            class = "table-title",
            "Predicate"
          ),
          htmltools::div(
            class = "table-title",
            htmltools::div(
              "Operation"
            )
          ),
          htmltools::div(
            class = "table-title",
            htmltools::div(
              "Result"
            )
          )
        )
      ),
      # First row always contains selection of dataset
      htmltools::div(
        id = ns("row_0"),
        class = "row-container",
        htmltools::div(
          class = "row-content",
          htmltools::div(
            class = "grid-center",
            "0"
          ),
          htmltools::div(),
          htmltools::div(
            class = "grid-vertical-center",
            "data"
          ),
          htmltools::div(
            # Class needed for disabling
            class = "operation",
            shiny::uiOutput(
              outputId = ns("select_dataset")
            )
          ),
          htmltools::div(
            class = "grid-center",
            m_action_button(
              inputId = ns("open_data"),
              label = NULL,
              icon = shiny::icon("table")
            )
          )
        )
      )
    ),
    # plus button in uiOutput, so that it appears just when the module is ready.
    # Otherwise inconsistencies are possible, if the button is clicked early
    shiny::uiOutput(
      outputId = ns("plus_button")
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
      inputId = ns("selected_dataset_object"),
      label = NULL,
      choices = dataset_choices_r()
    )
  })
  
  dataset_object_r <- shiny::reactive({
    .values$dataset_storage$get_object(shiny::req(input$selected_dataset_object))
  })
  
  data_r <- shiny::reactive({
    dataset_object_r()$get_dataset()
  })
  
  name_r <- shiny::reactive({
    dataset_object_r()$get_name()
  })
  
  id_r <- shiny::reactive({
    dataset_object_r()$get_id()
  })
  
  shiny::observe({
    .values$dataset_id_rv(id_r())
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
  
  # add_row has to be throttled, otherwise spaming the button would lead to
  # multiple rows added, which is not a problem, but if you are too fast not
  # all of them get disabled properly.
  throttled_add_row_r <- shiny::reactive({
    input$add_row
  }) %>% throttle(500)
  
  # Rows are added with insertUI. If a row with that index is added for the
  # first time, the server function is called exactly once
  shiny::observeEvent(throttled_add_row_r(), {
    rvs$n_row <- rvs$n_row + 1
    
    row_container_id <- ns("row" %_% rvs$n_row)
    
    ui <- m_row_ui(
      id = ns("id_m_row" %_% rvs$n_row),
      row_container_id = row_container_id,
      index = rvs$n_row
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
        dataset_object_r = dataset_object_r,
        row_index = rvs$n_row,
        remove_row_fun = remove_row_fun,
        row_container_id = paste0("#", row_container_id)
      ) 
    }
  })
  
  shiny::observeEvent(rvs$n_row, {
    if (rvs$n_row > 0) {
      # All but the last row get disabled
      purrr::walk(seq_len(rvs$n_row - 1), function(row_index) {
        shinyjs::hide(
          selector = paste0("#", ns("row_"), row_index, " .remove")
        )
        
        shinyjs::disable(
          selector = paste0("#", ns("row_"), row_index, " .predicate")
        )
        
        row_predicate <- m_row_env[["m_row" %_% row_index]]$predicate_r()
        if (row_predicate != "plot") {
          shinyjs::disable(
            selector = paste0("#", ns("row_"), row_index, " .operation")
          )
          
          shinyjs::disable(
            selector = paste0("#", ns("row_"), row_index, " .subrows-container")
          )
        }
      })
    }
  })
  
  shiny::observeEvent(rvs$n_row, {
    # Last row gets enabled
    shinyjs::enable(
      selector = paste0("#", ns("row_"), rvs$n_row)
    )
    
    shinyjs::show(
      selector = paste0("#", ns("row_"), rvs$n_row, " .remove")
    )
  })
  
  remove_row_fun = function() {
    shiny::removeUI(
      selector = paste0("#", ns("row_"), rvs$n_row)
    )
    
    rvs$n_row <- rvs$n_row - 1
  } 
  
  shiny::observeEvent(input$open_data, {
    new <- .values$transformation$viewer$append_tab(
      tab = shiny::tabPanel(
        title = paste(name_r(), "0", sep = ": "),
        value = ns(id_r()),
        DT::dataTableOutput(
          outputId = ns(id_r() %_% "dataset")
        )
      )
    )
    
    if (new) {
      output[[id_r() %_% "dataset"]] <- DT::renderDataTable({
        DT::datatable(
          shiny::isolate(data_r())
        )
      })
    }
  })
  
  return_list <- list(
    data_r = shiny::reactive(rvs$data)
  )
  
  return(return_list)
}
