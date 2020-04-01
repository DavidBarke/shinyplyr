init_explorer_classes <- function() {
  group_explorer_class <- shinyExplorer::group_explorer_class()
  
  dataset_explorer_class <- shinyExplorer::ExplorerClass$new(
    id = "dataset",
    ui = list(
      contextmenu_item_ui = dataset_explorer_class_contextmenu_item_ui,
      specific_contextmenu_items_ui = dataset_explorer_class_specific_contextmenu_items_ui
    ),
    server = dataset_explorer_class_server
  )
  
  explorer_classes <- list(
    group_explorer_class,
    dataset_explorer_class
  )
  
  names(explorer_classes) <- purrr::map_chr(explorer_classes, function(class) {
    class$id
  })
  
  explorer_classes
}