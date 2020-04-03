help_group_by_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The group-by operation selects columns by which the current dataset is
      grouped. Each group-by operation will override existing groups. Not selecting
      any column removes grouping. Present grouping variables are printed next
      to the dataset in the viewer."
    )
  )
}

help_group_by <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}