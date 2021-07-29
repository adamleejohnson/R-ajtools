#' Generate a Bland-Altman plot
#'
#' @importFrom blandr blandr.draw
#' @export
ggplot_bland.altman <- function(method1, method2, fill.colors = c("steelblue3", "gray85"),
                                ...) {
  args <- list(
    method1 = method1,
    method2 = method2,
    ...,
    ciShading = TRUE,
    sig.level = 0.95
  )
  args <- args[!duplicated(names(args))]
  p <- do.call("blandr.draw", args)
  q <- ggplot2::ggplot_build(p)
  q$data[[1]]$size <- 1.4 # increase size of points (default 1.5)
  q$data[[12]]$fill <- fill.colors[1] # edit fill color for shaded CI geoms
  q$data[[13]]$fill <- fill.colors[2]
  q$data[[14]]$fill <- fill.colors[2]
  plot(ggplot2::ggplot_gtable(q))
}
