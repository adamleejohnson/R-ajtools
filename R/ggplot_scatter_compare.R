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
#' @param lim Axes limits to apply to both x and y axes (the plot should be square)
#' @param line.color Color of the fit line (default = "blue")
#' @param line.size Fit line size (default = 0.7)
#' @param point.size Scatter point size (default = 0.7)
#' @param title.size Title font size (default = 12)
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
           lim = c(0, 100),
           line.color = "blue",
           line.size = 0.7,
           point.size = 0.7,
           title.size = 12,
           ...) {

    ggplot2::ggplot(data) +
      ggplot2::aes_string(x = data.x, y = data.y) +
      ggplot2::geom_smooth(method = "lm", se = TRUE, color = line.color, size = line.size) +
      ggplot2::geom_point(size = point.size) +
      ggplot2::labs(
        title = paste0(title, ": ", label.y, " vs ", label.x),
        x = if(is.character(units)) paste0(label.x, " (", units, ")") else label.x,
        y = if(is.character(units)) paste0(label.y, " (", units, ")") else label.y,
      ) +
      ggplot2::scale_x_continuous() +
      ggplot2::scale_y_continuous() +
      ggplot2::coord_fixed(ratio = 1,
                  xlim = lim,
                  ylim = lim) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = title.size),
        ...)
  }
