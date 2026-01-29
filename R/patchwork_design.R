#' @title Create a patchwork design specification
#' @description Create a non-overlapping patchwork design layout by specifying a list of block dimensions.
#'
#' **Motivation:** When using the `"design"` argument of [patchwork::plot_layout()], it can be cumbersome to use the syntax required by the patchwork framework. This syntax requires creating a vector of area elements, each of which is created by a call to [patchwork::area()]. This `area` function requires specifying the left/right/top/bottom coordinates of each block. This can become impractical when designing and making multiple edits to a complex layout in which we want our patches to be tiled/adjacent to one another. It can be much easier to instead specify a list of block dimensions, and have a function dynamically calculate all the necessary top/left/bottom/right coordinates. That is what this function does.
#' @param ... Character vectors specifying block dimensions. Each vector corresponds to a row in the output layout.
#'
#' Each element of each vector is a character of the form `"WxH"`, (e.g. "8x10"). The *height* of each row will be set to the height of the  tallest block in the row. Blocks with extra vertical space will be vertically centered. The *width* of each row will be set to the width of the widest row, and any extra horizontal space will be evenly distributed in between blocks in the row.
#' @param margin.row An optional integer specifying the number of units of space to place in-between rows.
#' @return A vector of patchwork areas.
#' @export
#' @examples
#' # Create a layout with 2 blocks in the first row,
#' # 2 blocks in the second row, 1 block in the
#' # third row, and 2 blocks in the fourth row.
#'
#' patchwork_design(
#'   c("8x10", "6.5x18"),
#'   c("9x12", "10x17"),
#'   c("4x28"),
#'   c("11x9", "11x13"),
#'   margin.row = 2
#' )
#' @seealso [patchwork::area()], [patchwork::plot_layout()]
patchwork_design <- function(..., margin.row = 0) {
  area_list <- list(...)
  hw <- function(s) matrix(2 * as.numeric(stringr::str_split_fixed(s, "x", n = 2)), nrow = length(s))
  max_width <- max(sapply(area_list, function(x) {
    hw_x <- hw(x)
    colSums(hw_x)[2]
  }))
  t <- 1
  p_areas <- c()
  for (row in area_list) {
    row_dims <- hw(row)
    row_width <- colSums(row_dims)[2]
    row_height_max <- max(row_dims[,1])
    nboxes <- nrow(row_dims)
    inner_pad <- max(0, floor((max_width - row_width)/(nboxes + 1)))
    l <- 1 + inner_pad
    for (n in seq_len(nboxes)) {
      box_h <- row_dims[n,1]
      box_w <- row_dims[n,2]
      top_pad <- max(0, floor((row_height_max - box_h)/2))
      p_areas <- append(p_areas, patchwork::area(t + top_pad, l, t + top_pad + box_h - 1, l + box_w - 1))
      l <- l + box_w + inner_pad
    }
    t <- t + row_height_max + margin.row
  }
  p_areas
}
