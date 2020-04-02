helper_summarise_class <- function(type, .values) {
  helper_filter_class(type, .values)
}

helper_summarise_fun <- function(name, .values) {
  if (!name %in% .values$SUMMARISE_FUN$name) stop(paste(name, "not in .values$SUMMARISE_FUN"))
  .values$SUMMARISE_FUN$fun[.values$SUMMARISE_FUN$name == name][[1]]
}

helper_summarise_allowed_classes <- function(name, .values) {
  if (!name %in% .values$SUMMARISE_FUN$name) stop(paste(name, "not in .values$SUMMARISE_FUN")) 
  .values$SUMMARISE_FUN$allowed[.values$SUMMARISE_FUN$name == name][[1]]
}