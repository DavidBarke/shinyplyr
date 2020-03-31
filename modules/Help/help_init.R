help_init <- function(input, output, session, .values) {
  
  ns <- session$ns
  
  # Interface for opening help pages from other modules
  .values$help$open <- function(topic) {
    stopifnot(topic %in% names(.values$help_rvs))
    .values$help_rvs[[topic]] <- .values$help_rvs[[topic]] + 1
  }
  
  .values$help$DATA <- tibble::tribble(
    ~topic, ~title, ~desc,
    "filter", "Filter", "Operation: Return rows with matching conditions",
    "group_by", "Group By", "Operation: Group by one or more variables",
    "index", "Index", "Index of help pages",
    "mutate", "Mutate", "Operation: Create or transform variables",
    "operation", "Operation", "Operations of the transformation table",
    "plot", "Plot", "Operation: Create a new plot",
    "predicate", "Predicate", "Predicates of the transformation table",
    "rename", "Rename", "Operation: Rename variables by name", 
    "select", "Select", "Operation: Select variables by name",
    "summarise", "Summarise", "Operation: Reduce multiple values down to a single value",
    "transformation", "Transformation table", "Step-wise data transformation"
  )
  
  help_ui <- list(
    filter = help_filter_ui,
    group_by = help_group_by_ui,
    index = help_index_ui,
    mutate = help_mutate_ui,
    operation = help_operation_ui,
    plot = help_plot_ui,
    predicate = help_predicate_ui,
    rename = help_rename_ui,
    select = help_select_ui,
    summarise = help_summarise_ui,
    transformation = help_transformation_ui
  )
  
  help_server <- list(
    filter = help_filter,
    group_by = help_group_by,
    index = help_index,
    mutate = help_mutate,
    operation = help_operation,
    plot = help_plot,
    predicate = help_predicate,
    rename = help_rename,
    select = help_select,
    summarise = help_summarise,
    transformation = help_transformation
  )
  
  purrr::walk2(.values$help$DATA$topic, .values$help$DATA$title, function(topic, title) {
    .values$help_rvs[[topic]] <- 0
    
    shiny::callModule(
      module = help_server[[topic]],
      id = "id_help" %_% topic,
      .values = .values
    )
    
    shiny::observeEvent(.values$help_rvs[[topic]], ignoreInit = TRUE, {
      .values$transformation$viewer$append_tab(
        tab = shiny::tabPanel(
          title = paste("Help", title, sep = ": "),
          value = ns("help" %_% topic),
          help_ui[[topic]](
            ns("id_help" %_% topic)
          )
        )
      )
    })
  })
}