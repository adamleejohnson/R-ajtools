#' @export
"%:%" <- function(a, b) {
  x <- strsplit(paste(deparse(substitute(a)), col = "\n"), "%?%", fixed = T)[[1]]
  if (length(x) != 2) stop("Invalid syntax for ternary operator.")
  if (eval(parse(text = x[1]))) eval(parse(text = x[2])) else b
}
