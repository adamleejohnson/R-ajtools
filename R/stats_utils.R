#' Z score
#' @param x Numeric vector
#' @export
z_score <- function(x) {
  (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
}
