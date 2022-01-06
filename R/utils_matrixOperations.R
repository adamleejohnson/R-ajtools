#' Transpose a table preserving data.frame
#' @param data Tidy table
#' @export
transpose <- function(data) {
  as.data.frame(t(as.data.frame(data)))
}

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

#' @title Combine two matrices along block diagonals
#' @description `(A, B) -> [A 0; 0 B]`
#' @param A Matrix x
#' @param B Matrix y
#' @param fill Fill value
#' @export
combine_block_diagonal <- function(A, B, fill = NA) {
  nrow_A <- nrow(A) %||% 0
  ncol_A <- ncol(A) %||% 0
  nrow_B <- nrow(B) %||% 0
  ncol_B <- ncol(B) %||% 0
  if (nrow_A == 0 && ncol_A == 0) return(B)
  if (nrow_B == 0 && ncol_B == 0) return(A)
  r <- rbind(
    cbind(A, matrix(fill, nrow = nrow_A, ncol = ncol_B)),
    cbind(matrix(fill, nrow = nrow_B, ncol = ncol_A), B)
  )
  dimnames(r) <- list(
    c(dimnames(A)[[1]], dimnames(B)[[1]]),
    c(dimnames(A)[[2]], dimnames(B)[[2]])
  )
  return(r)
}
