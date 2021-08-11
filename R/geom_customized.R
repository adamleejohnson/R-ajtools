#' Customized geom_histogram
#'
#' @param color outline color
#' @param fill fill color
#' @param size outline size
#' @param linetype outline type
#' @param alpha alpha
#' @inheritParams ggplot2::geom_histogram
#'
#' @export
geom_histogram_aj <-
  function(color = "white",
           fill = "grey25",
           size = 0.4,
           bins = 25,

           # defaults
           linetype = 1,
           alpha = NA,
           mapping = NULL,
           data = NULL,
           stat = "bin",
           position = "stack",
           binwidth = NULL,
           na.rm = FALSE,
           orientation = NA,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::geom_histogram(
      color = color,
      fill = fill,
      size = size,
      alpha = alpha,
      data = data,
      mapping = mapping,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      binwidth = binwidth,
      bins = bins,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  }
