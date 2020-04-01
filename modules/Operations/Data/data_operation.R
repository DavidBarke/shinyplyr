data_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
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
      htmltools::div(),
      htmltools::div(
        # Class needed for disabling
        class = "operation",
        shinyExplorer::explorer_selector_ui(
          id = ns("id_explorer_selector")
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
}

data_operation <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  output$select_dataset <- shiny::renderUI({
    shiny::selectInput(
      inputId = ns("selected_dataset_object"),
      label = NULL,
      choices = dataset_choices_r()
    )
  })
  
  dataset_object_r <- shiny::reactive({
    explorer_selector_return$selected_node_r()$get_object()
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
  
  shiny::observeEvent(input$open_data, {
    new <- .values$viewer$append_tab(
      tab = shiny::tabPanel(
        title = paste(name_r(), "0", sep = ": "),
        value = ns(id_r()),
        DT::dataTableOutput(
          outputId = ns(id_r() %_% "dataset")
        ),
        data_export_ui(
          id = ns("id_data_export")
        )
      ),
      select = TRUE
    )
    
    if (new) {
      output[[id_r() %_% "dataset"]] <- DT::renderDataTable({
        DT::datatable(
          shiny::isolate(data_r())
        )
      })
    }
  })
  
  # Export
  shiny::callModule(
    module = data_export,
    id = "id_data_export",
    .values = .values,
    data_r = data_r,
    name_r = name_r,
    row_index = 0
  )
  
  # Explorer selector
  explorer_selector_return <- shiny::callModule(
    module = shinyExplorer::explorer_selector,
    id = "id_explorer_selector",
    .values = .values,
    .root_node_r = shiny::reactive(.values$tree$get_root_node()),
    .explorer_classes = .values$explorer_classes,
    selectable_r = shiny::reactive("dataset"),
    visible_r = shiny::reactive(c("__group__", "dataset")),
    ui = "minimal",
    mode = "modal",
    action_button_fun = m_action_button,
    .label_list = label_explorer_selector(select_element = "Select dataset")
  )
  
  return_list <- list(
    data_r = data_r,
    dataset_object_r = dataset_object_r
  )
  
  return(return_list)
}