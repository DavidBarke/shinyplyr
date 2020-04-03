tab_home_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      htmltools::div(
        class = "help-page",
        htmltools::h4("What would you like to do?"),
        htmltools::tags$ul(
          htmltools::tags$li(
            shiny::actionLink(
              inputId = ns("help_getting_started"),
              label = "Getting started"
            )
          ),
          htmltools::tags$li(
            shiny::actionLink(
              inputId = ns("nav_data"),
              label = "Explore raw datasets"
            )
          ),
          htmltools::tags$li(
            shiny::actionLink(
              inputId = ns("nav_transformation"),
              label = "Transform and visualize datasets"
            )
          ),
          htmltools::tags$li(
            shiny::actionLink(
              inputId = ns("nav_import_csv"),
              label = "Import custom datasets from csv"
            )
          ),
          htmltools::tags$li(
            shiny::actionLink(
              inputId = ns("nav_import_rds"),
              label = "Import custom datasets from rds"
            )
          )
        )
      )
    )
  )
}

tab_home <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$help_getting_started, {
    .values$help$open("getting_started")
  })
  
  shiny::observeEvent(input$nav_data, {
    .values$navbar$open("data")
  })
  
  shiny::observeEvent(input$nav_transformation, {
    .values$navbar$open("transformation")
  })
  
  shiny::observeEvent(input$nav_import_csv, {
    .values$navbar$open("import_csv")
  })
  
  shiny::observeEvent(input$nav_import_rds, {
    .values$navbar$open("import_rds")
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