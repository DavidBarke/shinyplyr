fill_dataset_storage <- function(dataset_storage) {
  # Extract all data.frames out of datasets and fill dataset_storage
  obj_names <- ls("package:datasets")
  
  purrr::walk(obj_names, function(obj_name) {
    obj <- get(obj_name, "package:datasets")
    
    if ("data.frame" %in% class(obj)) {
      dataset_storage$add_object(
        DatasetObject$new(
          name = obj_name,
          dataset = obj
        )
      )
    }
  })
}