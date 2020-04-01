safe_numeric_input_value <- function(value) {
  as.numeric(stringr::str_replace_all(value, ",", "."))
}