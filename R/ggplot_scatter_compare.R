#' Scatter plot for method comparisons
#'
#' Define a scatter plot generator for making comparisons between two different methods
#'
#' @param data Data frame
#' @param data.x String label for x-axis data
#' @param data.y String label for y-axis data
#' @param label.x Plot label for x-axis
#' @param label.y Plot label for y-axis
#' @param title Plot title
#' @param units Units to append to axes (default = NULL)
#' @param lim Axes limits in form of 2-vector to apply to both x and y axes (the plot should be square)
#' @param line.color Color of the fit line (default = "blue")
#' @param line.size Fit line size (default = 0.7)
#' @param point.size Scatter point size (default = 0.7)
#' @param title.size Title font size (default = 12)
#' @param ticks.breaks Vector of break points to use on the axes
#' @param ticks.minor_breaks Vector of minor break points to use on the axes
#' @param ... Additional theme parameters
#'
#' @return
#' @export
ggplot_scatter_compare <-
  function(data,
           data.x,
           data.y,
           label.x,
           label.y,
           title = "Title",
           units = NULL,
           lim = NULL,
           line.color = "blue",
           line.size = 0.7,
           point.size = 0.7,
           title.size = 12,
           ticks.breaks = NULL,
           ticks.minor_breaks = NULL,
           ...) {
    ggplot2::ggplot(data) +
      ggplot2::aes_string(x = data.x, y = data.y) +
      ggplot2::geom_smooth(
        method = "lm",
        se = TRUE,
        color = line.color,
        size = line.size
      ) +
      ggplot2::geom_point(size = point.size) +
      ggplot2::labs(
        title = paste0(title, ": ", label.y, " vs ", label.x),
        x = if (is.character(units))
          paste0(label.x, " (", units, ")")
        else
          label.x,
        y = if (is.character(units))
          paste0(label.y, " (", units, ")")
        else
          label.y,
      ) +
      ggplot2::scale_x_continuous(
        breaks = if (is.null(ticks.breaks)) ggplot2::waiver() else ticks.breaks,
        minor_breaks = if (is.null(ticks.minor_breaks)) ggplot2::waiver() else ticks.minor_breaks
      ) +
      ggplot2::scale_y_continuous(
        breaks = if (is.null(ticks.breaks)) ggplot2::waiver() else ticks.breaks,
        minor_breaks = if (is.null(ticks.minor_breaks)) ggplot2::waiver() else ticks.minor_breaks
      ) +
      ggplot2::coord_fixed(ratio = 1,
                           xlim = lim,
                           ylim = lim) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = title.size),
                     ...)
  }
