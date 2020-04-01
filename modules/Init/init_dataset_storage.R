init_dataset_storage <- function(dataset_storage) {
  # Extract all data.frames out of datasets and fill dataset_storage
  add_pkg_datasets("datasets", dataset_storage)
  add_pkg_datasets("ggplot2", dataset_storage)
  add_pkg_datasets("dplyr", dataset_storage)
}

add_pkg_datasets <- function(pkg, dataset_storage) {
  obj_names <- ls(paste("package", pkg, sep = ":"))
  
  purrr::walk(obj_names, function(obj_name) {
    obj <- get(obj_name, paste("package", pkg, sep = ":"))
    
    if ("data.frame" %in% class(obj)) {
      dataset_storage$add_object(
        DatasetObject$new(
          name = obj_name,
          dataset = tibble::as_tibble(obj)
        )
      )
    }
  })
}