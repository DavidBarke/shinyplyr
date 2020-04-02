tab_data_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      title = htmltools::tagList(
        "Data",
        help_button(ns("help_data"))
      ),
      status = "primary",
      explorer_ui(
        id = ns("id_explorer")
      )
    )
  )
}

tab_data <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$help_data, {
    .values$help$open("data")
  })
  
  shiny::callModule(
    module = explorer,
    id = "id_explorer",
    .values = .values,
    .root_node_r = shiny::reactive(.values$tree$get_root_node()),
    .explorer_classes = .values$explorer_classes,
    addable_r = shiny::reactive("__group__"),
    visible_r = shiny::reactive(c("__group__", "dataset")),
    .label_list = shinyExplorer::label_explorer(
      add_group = "New folder"
    )
  )
}