helper_mutate_operator_choices <- function(.values) {
  groups <- c("math", "cum", "lgl", "rank")
  names(groups) <- groups
  
  purrr::map(groups, function(group) {
    indices <- .values$MUTATE_OPERATORS$group == group
    ops <- .values$MUTATE_OPERATORS$name[indices]
    setNames(ops, .values$MUTATE_OPERATORS$op[indices])
  })
}

helper_mutate_operator_type <- function(operator, .values) {
  # no check, because operator is one of helper_mutate_operator_choices
  .values$MUTATE_OPERATORS$type[.values$MUTATE_OPERATORS$name == operator]
}

helper_mutate_allowed_column_types <- function(operator, .values) {
  .values$MUTATE_OPERATORS$allowed[.values$MUTATE_OPERATORS$name == operator][[1]]
}

helper_mutate_fun <- function(operator, .values) {
  .values$MUTATE_OPERATORS$fun[.values$MUTATE_OPERATORS$name == operator][[1]]
}
