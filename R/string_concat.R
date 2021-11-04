#' String concatenation
#' @param x string x
#' @param y string y
"%+%" <- function(x, y) {
  if (is.character(x) & is.character(y)) {
    return(paste0(x, y))
  } else {
    .Primitive("+")(x, y)
  }
}
