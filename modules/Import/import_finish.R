import_finish_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::hr(),
    m_action_button(
      inputId = ns("preview"),
      label = "Preview"
    ),
    htmltools::hr(),
    htmltools::h4("Choose folder"),
    shiny::uiOutput(
      outputId = ns("explorer")
    ),
    htmltools::hr(),
    shiny::uiOutput(
      outputId = ns("folder")
    ),
    m_action_button(
      inputId = ns("save"),
      label = "Save"
    )
  )
}

import_finish <- function(
  input, output, session, .values, data_r, file_name_r, name_r,
  reset_rv, file_input_outdated_rv
) {
  
  ns <- session$ns
  
  output$finish <- shiny::renderUI({
    
  })
  
  shiny::observeEvent(input$preview, {
    new <- .values$viewer$append_tab(
      tab = shiny::tabPanel(
        title = paste("Preview", name_r(), sep = ": "),
        value = "preview" %_% file_name_r() %_% name_r(),
        DT::dataTableOutput(
          outputId = ns("preview" %_% file_name_r() %_% name_r())
        )
      )
    )
    
    if (new) {
      output[["preview" %_% file_name_r() %_% name_r()]] <- DT::renderDataTable({
        DT::datatable(data_r())
      })
    }
  })
  
  output$explorer <- shiny::renderUI({
    reset_rv()
    
    shinyExplorer::explorer_ui(
      id = ns("id_explorer")
    )
  })
  
  path_to_folder_r <- shiny::reactive({
    path <- character()
    
    node <- explorer_return$current_node_r()
    
    while(node$get_id() != .values$import$node$get_id()) {
      path <- c(node$get_object()$get_name(), path)
      node <- node$get_parent_node()
    }
    
    path <- c(.values$import$node$get_object()$get_name(), path)
    
    paste(path, collapse = "/")
  })
  
  output$folder <- shiny::renderUI({
    paste("Selected folder", path_to_folder_r(), sep = ": ")
  })
  
  shiny::observeEvent(input$save, {
    group_node <- explorer_return$current_node_r()
    
    group_node$add_child(
      explorer_class_id = "dataset",
      object = DatasetObject$new(
        name = name_r(),
        dataset = data_r()
      )
    )
    
    shinyjs::alert(paste("Dataset", name_r(), "successfully imported."))
    
    reset_rv(reset_rv() + 1)
    file_input_outdated_rv(TRUE)
  })
  
  explorer_return <- shiny::callModule(
    module = shinyExplorer::explorer,
    id = "id_explorer",
    .values = .values,
    .root_node_r = shiny::reactive(.values$import$node),
    .explorer_classes = .values$explorer_classes,
    addable_r = shiny::reactive("__group__"),
    visible_r = shiny::reactive("__group__")
  )
}