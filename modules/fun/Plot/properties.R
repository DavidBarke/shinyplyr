GEOM_CLASSES <- list(
  area = "filled",
  bin2d = "bin2d",
  density2d = "filled",
  hex = "filled",
  line = "path",
  path = "path",
  point = "point",
  smooth = "filled",
  step = "path"
)

REQUIRED_PROPERTIES <- list(
  "x",
  c("x", "y"),
  c("x", "y", "z")
)

OPTIONAL_PROPERTIES <- list(
  bin2d = c("alpha", "colour", "group", "linetype", "size"),
  filled = c("alpha", "colour", "fill", "group", "linetype", "size"),
  path = c("alpha", "colour", "group", "linetype", "size"),
  point = c("alpha", "colour", "fill", "group", "shape", "size")
)

all_additional <- c("na.rm", "show.legend")

additional_properties <- list(
  step = "direction"
)

geom_class <- function(geom) {
  GEOM_CLASSES[[geom]]
}

properties <- function(geom, n) {
  list(
    required = REQUIRED_PROPERTIES[[n]], 
    optional = OPTIONAL_PROPERTIES[[geom_class(geom)]],
    additional = c(all_additional, additional_properties[[geom]])
  )
}

AES_CLASSES <- c(
  alpha = "percentage",
  colour = "colour",
  fill = "colour",
  linetype = "linetype",
  shape = "shape",
  size = "positive"
)

aes_class <- function(aes) {
  AES_CLASSES[aes]  
}