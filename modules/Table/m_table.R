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
          class = "area-step table-title"
        ),
        htmltools::div(
          class = "area-predicate table-title",
          "Predicate"
        ),
        htmltools::div(
          class = "area-operation table-title",
          htmltools::div(
            "Operation"
          )
        ),
        htmltools::div(
          class = "area-result table-title",
          htmltools::div(
            "Result"
          )
        ),
        htmltools::div(
          class = "area-remove"
        )
      ),
      # First row always contains selection of dataset
      htmltools::div(
        id = ns("row_0"),
        class = "row-container",
        htmltools::div(
          class = "area-step grid-center",
          "0"
        ),
        htmltools::div(
          class = "area-predicate grid-vertical-center",
          "data"
        ),
        htmltools::div(
          class = "area-operation",
          shiny::uiOutput(
            outputId = ns("select_dataset")
          )
        ),
        htmltools::div(
          class = "area-result grid-center",
          m_action_button(
            inputId = ns("open_data"),
            label = NULL,
            icon = shiny::icon("table")
          )
        ),
        htmltools::div(
          class = "area-remove grid-center"
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
    
    row_html_id <- ns("row" %_% rvs$n_row)
    
    ui <- m_row_ui(
      id = ns("id_m_row" %_% rvs$n_row),
      row_html_id = row_html_id,
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
        name_r = name_r,
        id_r = id_r,
        row_index = rvs$n_row,
        remove_row_fun = remove_row_fun
      ) 
    }
  })
  
  shiny::observeEvent(rvs$n_row, {
    if (rvs$n_row > 0) {
      # All but the last row get disabled
      purrr::walk(seq_len(rvs$n_row - 1), function(row_index) {
        shinyjs::hide(
          selector = paste0("#", ns("row_"), row_index, " .area-remove")
        )
        
        shinyjs::disable(
          selector = paste0("#", ns("row_"), row_index, " .area-predicate")
        )
        
        row_predicate <- m_row_env[["m_row" %_% row_index]]$predicate_r()
        if (row_predicate != "plot") {
          shinyjs::disable(
            selector = paste0("#", ns("row_"), row_index, " .area-operation")
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
      selector = paste0("#", ns("row_"), rvs$n_row, " .area-remove")
    )
  })
  
  remove_row_fun = function() {
    shiny::removeUI(
      selector = paste0("#", ns("row_"), rvs$n_row)
    )
    
    rvs$n_row <- rvs$n_row - 1
  } 
  
  shiny::observeEvent(input$open_data, {
    new <- .values$home$viewer$append_tab(
      tab = shiny::tabPanel(
        title = paste(dataset_object_r()$get_name(), "0", sep = ": "),
        value = ns(input$selected_dataset %_% "0"),
        DT::dataTableOutput(
          outputId = ns("dataset")
        )
      )
    )
    
    if (new) {
      output$dataset <- DT::renderDataTable({
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
