#' Is the number an integer?
#' @param n Numeric to test
#' @export
is_integer <- function(n) {
  n == round(n)
}
