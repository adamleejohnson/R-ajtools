#' Transpose a table preserving data.frame
#' @param data Tidy table
#' @export
transpose <- function(data) {
  as.data.frame(t(as.data.frame(data)))
}
