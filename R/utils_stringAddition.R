#' @name str_plus
#' @title String concatenation
#' @description %paste% is for standard string concatenation. %slash% is for concatenating file paths.
#' @param a string a
#' @param b string b
#' @export
"%paste%" <- function(a, b) {
  UseMethod("%paste%")
}

#' @rdname str_plus
#' @export
"%paste%.default" <- function(a, b) {
  stop("`%paste%` does not have a defined default method")
}

#' @rdname str_plus
#' @export
"%paste%.character" <- function(a, b) {
  if (is.character(a) & is.character(b)) {
    return(paste0(a, b))
  } else {
    NextMethod("%paste%")
  }
}

#' @rdname str_plus
#' @export
"%slash%" <- function(a, b) {
  UseMethod("%slash%")
}

#' @rdname str_plus
#' @export
"%slash%.default" <- function(a, b) {
  stop("`%slash%` does not have a defined default method")
}

#' @rdname str_plus
#' @export
"%slash%.character" <- function(a, b) {
  if (is.character(a) & is.character(b)) {
    a_end <- substr(a, nchar(a), nchar(a))
    a <- ifelse(a_end == "/", substr(a, 1, nchar(a) - 1), a)
    b_start <- substr(b, 1, 1)
    b <- ifelse(b_start == "/", substr(b, 2, nchar(b)), b)
    return(paste0(a, "/", b))
  } else {
    NextMethod("%slash%")
  }
}
