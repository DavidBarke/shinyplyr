geom_classes <- list(
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

required_properties <- list(
  "x",
  c("x", "y"),
  c("x", "y", "z")
)

optional_properties <- list(
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
  geom_classes[[geom]]
}

properties <- function(geom, n) {
  list(
    required = required_properties[[n]], 
    optional = optional_properties[[geom_class(geom)]],
    additional = c(all_additional, additional_properties[[geom]])
  )
}

aes_classes <- c(
  alpha = "percentage",
  colour = "colour",
  fill = "colour",
  linetype = "linetype",
  shape = "shape",
  size = "positive"
)

aes_class <- function(aes) {
  aes_classes[aes]  
}