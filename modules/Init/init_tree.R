init_tree <- function(tree, .values) {
  # Add pkgs node
  pkgs_node <- tree$get_root_node()$add_child(
    explorer_class_id = "__group__",
    object = Object$new("Package datasets"),
    removable = FALSE,
    return = "child"
  )
  
  # Extract all data.frames out of datasets and fill pkgs_node
  add_pkg_datasets("datasets", pkgs_node)
  add_pkg_datasets("ggplot2", pkgs_node)
  add_pkg_datasets("dplyr", pkgs_node)
  
  # Add import node
  .values$import$node <- tree$get_root_node()$add_child(
    explorer_class_id = "__group__",
    object = Object$new("Imported datasets"),
    removable = FALSE,
    return = "child"
  )
}

add_pkg_datasets <- function(pkg, node) {
  obj_names <- ls(paste("package", pkg, sep = ":"))
  
  pkg_node <- node$add_child(
    explorer_class_id = "__group__",
    object = Object$new(pkg),
    removable = FALSE,
    return = "child"
  )
  
  purrr::walk(obj_names, function(obj_name) {
    obj <- get(obj_name, paste("package", pkg, sep = ":"))
    
    if ("data.frame" %in% class(obj)) {
      pkg_node$add_child(
        explorer_class_id = "dataset",
        object = DatasetObject$new(
          name = obj_name,
          dataset = obj
        ),
        removable = FALSE
      )
    }
  })
}