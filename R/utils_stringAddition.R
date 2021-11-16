#' @export
"%paste%" <- function(a, b) {
  UseMethod("%paste%")
}

#' @export
"%paste%.default" <- function(a, b) {
  stop("`%paste%` does not have a defined default method")
}

#' @name str_plus
#' @title String concatenation
#' @description %paste% is for standard string concatenation. %slash% is for concatenating file paths.
#' @param x string x
#' @param y string y
#' @export
"%paste%.character" <- function(x, y) {
  if (is.character(x) & is.character(y)) {
    return(paste0(x, y))
  } else {
    NextMethod("%paste%")
  }
}


#' @export
"%slash%" <- function(a, b) {
  UseMethod("%slash%")
}

#' @export
"%slash%.default" <- function(a, b) {
  stop("`%slash%` does not have a defined default method")
}

#' @rdname str_plus
#' @export
"%slash%.character" <- function(x, y) {
  if (is.character(x) & is.character(y)) {
    x_end <- substr(x, nchar(x), nchar(x))
    x <- ifelse(x_end == "/", substr(x, 1, nchar(x) - 1), x)
    y_start <- substr(y, 1, 1)
    y <- ifelse(y_start == "/", substr(y, 2, nchar(y)), y)
    return(paste0(x, "/", y))
  } else {
    NextMethod("%slash%")
  }
}
