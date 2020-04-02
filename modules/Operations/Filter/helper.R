helper_filter_class <- function(type, .values) {
  if (!type %in% .values$TYPE_DATA$type) return("missing")
  .values$TYPE_DATA$filter[.values$TYPE_DATA$type == type]
}

helper_column_type_name <- function(type, .values) {
  if (!type %in% .values$TYPE_DATA) return(type)
  .values$TYPE_DATA$name[.values$TYPE_DATA$type == type]
}