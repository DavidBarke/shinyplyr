help_summarise_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The summarise operation allows you to reduce multiple values down to a
      single value by selecting a summarise function, which is applied to every
      group created by the",
      shiny::actionLink(
        inputId = ns("help_group_by"),
        label = "group-by operation."
      ),
      "Enter the name of the column that shall contain the summarised values. Select
      one of the predefined summary functions and choose the column to which you
      want to apply this function. The columns that can be used depend on the
      summary function selected (for example, you may not apply the mean function 
      on a character column)."
    )
  )
}

help_summarise <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$help_group_by, {
    .values$help$open("group_by")
  })
}