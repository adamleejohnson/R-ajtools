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
#' @param ticks.breaks Vector of break points to use on the axes
#' @param ticks.minor_breaks Vector of minor break points to use on the axes
#' @param ... Additional theme parameters
#'
#' @export
gg_scatter_compare <-
  function(data,
           data.x,
           data.y,
           label.x,
           label.y,
           title,
           units,
           lim,
           line.color = "blue",
           line.size = 0.7,
           point.size = 0.7,
           ticks.breaks = NULL,
           ticks.minor_breaks = NULL,
           ...) {
    label.x <- bquote_remask(label.x)
    label.y <- bquote_remask(label.y)
    title <- bquote_remask(title)

    if (!missing(units)) {
      units <- bquote_remask(units)
      label.x <- bquote_remask(label.x ~ (units))
      label.y <- bquote_remask(label.y ~ (units))
    }

    ggplot2::ggplot(data) +
      ggplot2::aes_string(x = data.x, y = data.y) +
      ggplot2::geom_smooth(
        formula = y ~ x,
        method = "lm",
        se = TRUE,
        color = line.color,
        size = line.size
      ) +
      ggplot2::geom_point(size = point.size) +
      ggplot2::labs(
        title = title,
        x = label.x,
        y = label.y
      ) +
      ggplot2::scale_x_continuous(
        breaks = if (is.null(ticks.breaks)) ggplot2::waiver() else ticks.breaks,
        minor_breaks = if (is.null(ticks.minor_breaks)) ggplot2::waiver() else ticks.minor_breaks
      ) +
      ggplot2::scale_y_continuous(
        breaks = if (is.null(ticks.breaks)) ggplot2::waiver() else ticks.breaks,
        minor_breaks = if (is.null(ticks.minor_breaks)) ggplot2::waiver() else ticks.minor_breaks
      ) +
      ggplot2::coord_fixed(
        ratio = 1,
        xlim = lim,
        ylim = lim
      )
  }