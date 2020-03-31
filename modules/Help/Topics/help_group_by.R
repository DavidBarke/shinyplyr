help_group_by_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The group-by operation selects columns by which the current dataset is
      grouped. Each group-by operation will override existing groups. Not selecting
      any column removes grouping. Present grouping variables are printed next
      to the dataset in the",
      shiny::actionLink(
        inputId = ns("help_dataset_output"),
        label = "dataset output."
      )
    )
  )
}

help_group_by <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$help_dataset_output, {
    .values$help$open("dataset_output")
  })
}