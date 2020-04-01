# Right click on explorer_body's datatable displays contextmenu. In the lower 
# part the contextmenu_items for adding new nodes to the explorer are displayed.
dataset_explorer_class_contextmenu_item_ui <- function(id) {
  ns <- shiny::NS(id)
  
  # Won't be displayed, because datasets are only addable via the import modules
  contextmenu_item(
    inputId = ns("add"),
    label = "Dataset",
    icon = shiny::icon("table")
  )
}

# Right click on explorer_body's datatable displays contextmenu. In the upper
# part the contextmenu_items specific to the explorer_class of the current node
# are displayed.
dataset_explorer_class_specific_contextmenu_items_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shinyExplorer::contextmenu_item(
      inputId = ns("open"),
      label = "Open"
    ),
    shiny::uiOutput(
      outputId = ns("rename")
    )
  )
}

dataset_explorer_class_server <- function(
  input, output, session, .values, .explorer_rvs, .state = list()
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$open, {
    object <- .explorer_rvs$contextmenued_node$get_object()
    
    new <- .values$viewer$append_tab(
      tab = shiny::tabPanel(
        title = object$get_name(),
        value = ns(object$get_id()),
        data_output_ui(
          id = ns(object$get_id() %_% "data_output")
        )
      )
    )
    
    if (new) {
      shiny::callModule(
        module = data_output,
        id = object$get_id() %_% "data_output",
        .values = .values,
        data_r = shiny::reactive(object$get_dataset()),
        dataset_object = object,
        row_index = 0,
        check_for_disconnect = FALSE
      )
    }
  })
  
  output$rename <- shiny::renderUI({
    shinyExplorer::rename_contextmenu_item_ui(
      id = ns("rename")
    )
  })
  
  shiny::callModule(
    module = shinyExplorer::rename_contextmenu_item,
    id = "rename",
    .values = .values,
    .explorer_rvs = .explorer_rvs
  )
  
  return_list <- list(
    icon_r = shiny::reactive({
      shiny::icon("table")
    }),
    is_group_r = shiny::reactive({
      FALSE
    })
  )
  
  return(return_list)
}
