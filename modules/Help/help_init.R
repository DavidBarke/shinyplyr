help_init <- function(input, output, session, .values) {
  
  ns <- session$ns
  
  .values$help$history_rv <- shiny::reactiveVal(character())
  
  # Interface for opening help pages from other modules
  .values$help$open <- function(topic) {
    stopifnot(topic %in% names(.values$help_rvs))
    
    .values$help_rvs[[topic]] <- .values$help_rvs[[topic]] + 1
    
    hist <- .values$help$history_rv()
    if (length(hist) && topic != hist[1]) {
      hist <- c(topic, hist)
      .values$help$history_rv(hist)
    }
    
    if (!length(hist)) {
      .values$help$history_rv(topic)
    }
  }
  
  .values$help$DATA <- tibble::tribble(
    ~topic, ~title, ~desc,
    "dataset_output", "Dataset Output", "View and export transformation results",
    "filter", "Filter", "Operation: Return rows with matching conditions",
    "group_by", "Group By", "Operation: Group by one or more variables",
    "index", "Index", "Index of help pages",
    "mutate", "Mutate", "Operation: Create or transform variables",
    "operation", "Operation", "Operations of the transformation table",
    "plot", "Plot", "Operation: Create a new plot",
    "plot_aes", "Aesthetic", "Aesthetic layer",
    "plot_geom", "Geometry", "Geometry layer",
    "plot_facet", "Facets", "Facet layer",
    "plot_coord", "Coordinates", "Coordinates layer",
    "plot_theme", "Theme", "Theme layer",
    "predicate", "Predicate", "Predicates of the transformation table",
    "rename", "Rename", "Operation: Rename variables by name", 
    "select", "Select", "Operation: Select variables by name",
    "summarise", "Summarise", "Operation: Reduce multiple values down to a single value",
    "transformation", "Transformation table", "Step-wise data transformation"
  )
  
  help_ui <- list(
    dataset_output = help_dataset_output_ui,
    filter = help_filter_ui,
    group_by = help_group_by_ui,
    index = help_index_ui,
    mutate = help_mutate_ui,
    operation = help_operation_ui,
    plot = help_plot_ui,
    plot_aes = help_plot_aes_ui,
    plot_geom = help_plot_geom_ui,
    plot_facet = help_plot_facet_ui,
    plot_coord = help_plot_coord_ui,
    plot_theme = help_plot_theme_ui,
    predicate = help_predicate_ui,
    rename = help_rename_ui,
    select = help_select_ui,
    summarise = help_summarise_ui,
    transformation = help_transformation_ui
  )
  
  help_server <- list(
    dataset_output = help_dataset_output,
    filter = help_filter,
    group_by = help_group_by,
    index = help_index,
    mutate = help_mutate,
    operation = help_operation,
    plot = help_plot,
    plot_aes = help_plot_aes,
    plot_geom = help_plot_geom,
    plot_facet = help_plot_facet,
    plot_coord = help_plot_coord,
    plot_theme = help_plot_theme,
    predicate = help_predicate,
    rename = help_rename,
    select = help_select,
    summarise = help_summarise,
    transformation = help_transformation
  )
  
  purrr::pmap(.values$help$DATA, function(...) {
    row <- list(...)
    topic <- row$topic; title <- row$title; desc <- row$desc
    
    .values$help_rvs[[topic]] <- 0
    
    shiny::callModule(
      module = help_server[[topic]],
      id = "id_help" %_% topic,
      .values = .values
    )
    
    shiny::callModule(
      module = help_page,
      id = "id_help_page" %_% topic,
      .values = .values
    )
    
    shiny::observeEvent(.values$help_rvs[[topic]], ignoreInit = TRUE, {
      .values$transformation$viewer$append_tab(
        tab = shiny::tabPanel(
          title = paste("Help", title, sep = ": "),
          value = ns("help" %_% topic),
          help_page_ui(
            id = ns("id_help_page" %_% topic),
            content = help_ui[[topic]](
              id = ns("id_help" %_% topic)
            ),
            desc = desc
          )
        )
      )
    })
  })
}