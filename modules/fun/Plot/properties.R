properties <- function(geom) {
  required <- switch(
    geom,
    "point" = c("x", "y"),
    "line" = c("x", "y")
  )
  
  optional <- switch(
    geom,
    "point" = c("alpha", "colour", "fill", "group", "linetype", "shape", "size"),
    "line" = c("alpha", "colour", "group", "linetype", "size")
  )
  
  return(list(required = required, optional = optional))
}

aes_classes <- c(
  alpha = "numeric",
  colour = "colour",
  fill = "colour",
  linetype = "linetype",
  shape = "shape",
  size = "integer"
)

aes_class <- function(aes) {
  aes_classes[aes]  
}