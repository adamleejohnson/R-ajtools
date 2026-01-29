#' Z score
#' @param x Numeric vector
#' @export
z_score <- function(x) {
  (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
}

#' @noRd
sym_setdiff <- function(a,b) c(setdiff(a,b), setdiff(b,a))

#' @noRd
setdiff.lazy <- function(x,y) {
  for (i in seq_along(y)) {
    if (y[i] %in% x) x <- x[ -match(y[i], x) ]
  }
  return(x)
}


#' @title Is the number an integer?
#' @description Different from [is.integer()] and [rlang::is_integer()], which only check if the class of the object is `"integer"`.
#' @param n Numeric to test
#' @export
is_integer_num <- function(n) {
  !is.na(n) & n == round(n)
}
