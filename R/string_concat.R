#' String concatenation
#' @param x string x
#' @param y string y
#' @export
"%+%" <- function(x, y) {
  if (is.character(x) & is.character(y)) {
    return(paste(x, y, sep = ""))
  } else {
    .Primitive("+")(x, y)
  }
}
