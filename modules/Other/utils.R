`%_%` <- function(x, y) {
  paste(x, y, sep = "_")
}

fallback <- function(x, y) {
  if (is.null(x)) {
    return(y)
  } else {
    return(x)
  }
}

is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}