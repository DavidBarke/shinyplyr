help_rename_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The rename operation allows you to rename columns of the current dataset.
      Clicking the triangle will toggle the rename table, which has one row for
      each column showing the old name and the new name. Omitting the new name
      (that means leaving the input empty) removes this column from the dataset.
      Therefore the rename operation is a convenient alternative to the
      ",
      shiny::actionLink(
        inputId = ns("help_select"),
        label = "select_operation."
      )
    )
  )
}

help_rename <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$help_select, {
    .values$help$open("select")
  })
}