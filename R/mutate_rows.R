#' Apply dplyr's mutate to a subset of rows
#' @inheritParams dplyr::mutate
#' @inheritDotParams dplyr::mutate
#' @param row_selector vector of row indices
#' @export
mutate_rows <- function(.data, row_selector, ...) {
  .data <- as.data.frame(.data)
  .data[row_selector, ] <- dplyr::mutate(.data[row_selector, ], ...)
  .data
}
