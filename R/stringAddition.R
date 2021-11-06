#' @name str_plus
#' @title String concatenation
#' @description %++% is for standard string concatenation. %//% is for concatenating file paths.
#' @param x string x
#' @param y string y
#' @export
"%++%" <- function(x, y) {
  if (is.character(x) & is.character(y)) {
    return(paste0(x, y))
  } else {
    x %++% y
  }
}

#' @rdname str_plus
#' @export
"%//%" <- function(x, y) {
  if (is.character(x) & is.character(y)) {
    x_end <- substr(x, nchar(x), nchar(x))
    x <- ifelse(x_end == "/", substr(x, 1, nchar(x) - 1), x)
    y_start <- substr(y, 1, 1)
    y <- ifelse(y_start == "/", substr(y, 2, nchar(y)), y)
    return(paste0(x, "/", y))
  } else {
    x %//% y
  }
}
