#' Generate a QQ-plot with confidence bands
#'
#' UNDER DEVELOPMENT
#'
#' @param data ...
#' @param rescale.expected ...
#' @param stat ...
#' @param ci ...
#' @param label_x ...
#' @param label_y ...
#'
#' @importFrom stats median
#'
ggplot_qqplot_aj <-
  function(data,
           rescale.expected = FALSE,
           stat = stats::qnorm,
           ci = 0.95,
           label_x = "Expected",
           label_y = "Observed") {

    # Generate data
    data <- as.numeric(unlist(data))
    ppoints <- stat(ppoints(data))

    if (rescale.expected) {
      # scale the expected values so that it has the same range and median as the observed
      ppoints <- median(data) + ppoints*(max(data) - min(data))/(max(ppoints) - min(ppoints))
    }

    plotdata = data.frame(
      expected = ppoints,
      observed = sort(data)
    )

    # Create confidence bands

    # Plot

    # Plot
    ggplot(plotdata) +
    aes(x = expected, y = observed) +
    # geom_ribbon(aes(ymax = cupper, ymin = clower),
    #             fill = "grey30",
    #             alpha = 0.5) +
    geom_point(
      size = 1.1) +
    # geom_segment(
    #   data = . %>% filter(expected == max(expected)),
    #   aes(
    #     x = 0,
    #     xend = expected,
    #     y = 0,
    #     yend = expected
    #   ),
    #   size = 1.25,
    #   alpha = 0.5,
    #   color = "grey30",
    #   lineend = "round") +
    # xlim(min(data), max(data)) +
    labs(x = label_x,
         y = label_y)

  }
