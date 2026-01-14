#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

waiver <- ggplot2::waiver
is_waive <- ggplot2:::is_waiver

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

## usethis namespace: start
#' @useDynLib ajtools, .registration = TRUE
## usethis namespace: end
NULL


## unexported functions from other packages
grDevices__cairoVersion <- utils::getFromNamespace("cairoVersion", "grDevices")
httr__parse_text <- utils::getFromNamespace("parse_text", "httr")
patchwork__register_s3_method <- utils::getFromNamespace("register_s3_method", "patchwork")
