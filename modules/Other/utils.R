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