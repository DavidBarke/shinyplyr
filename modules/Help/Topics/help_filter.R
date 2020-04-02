help_filter_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The filter operation defines a condition by which the dataset is filtered.
      For specifying multiple conditions you therefore need to create multiple
      transformation steps. Each condition is made up of three parts: a column,
      an operator and a value. The condition is composed as <column> <operator>
      <value>.
      "
    ),
    htmltools::h4(
      "Operator"
    ),
    htmltools::p(
      "The choices for the operator are dependent on the type of the selected 
      column. For supported column types see:",
      htmltools::br(),
      shiny::actionLink(
        inputId = ns("help_type"),
        label = "Type operation"
      ),
      htmltools::br(),
      "Columns that posess other data types can't be filtered."
    ),
    htmltools::h4(
      "Limitations"
    ),
    htmltools::tags$ul(
      htmltools::tags$li(
        "Multiple conditions require you to add multiple transformation steps and 
        are therefore always connected by the logical 'and' operator."
      ),
      htmltools::tags$li(
        "The value part of the condition can't be another column."
      )
    )
  )
}

help_filter <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$help_type, {
    .values$help$open("type")
  })
}