help_init <- function(input, output, session, .values) {
  
  ns <- session$ns
  
  .values$help <- list(
    operation_rv = shiny::reactiveVal(0)
  )
  
  shiny::callModule(
    module = help_operation,
    id = "id_help_operation",
    .values = .values
  )
  
  shiny::observeEvent(.values$help$operation_rv(), ignoreInit = TRUE, {
    .values$transformation$viewer$append_tab(
      tab = shiny::tabPanel(
        title = "Help: Operation",
        value = ns("help_operation"),
        help_operation_ui(
          id = ns("id_help_operation")
        )
      )
    )
  })
}