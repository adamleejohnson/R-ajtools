#' @title Convert a ggsurvplot to a patchwork
#' @description A thin wrapper around [survminer::ggsurvplot()] that creates a survival plot and risk table, converts plots to a patchwork (see [`patchwork::patchwork`]), and applies some custom formatting options.
#' @inheritParams survminer::ggsurvplot
#' @inheritDotParams survminer::ggsurvplot
#' @param scibeautify Whether to apply [scibeautify()] to the numbers in the risk table
#' @param axis.title.y.nudge Custom right-margin for the y-axis title of the plot, in units of "lines"
#' @return A patchwork
#' @export
ggsurvplot_patchwork <- function(fit,
                                 scibeautify = T,
                                 axis.title.y.nudge = 0,
                                 ...) {

  # pass custom options to ggsurvplot
  custom_opts <- list(
    conf.int = T,
    palette = rev(ggsci::pal_npg()(2)),
    legend.title = "",
    legend = c(0.2, 0.7),
    censor = F
  )
  opts <- utils::modifyList(list(...), list(fit = fit, risk.table = T))
  opts <- utils::modifyList(custom_opts, opts, keep.null = T)
  ggsurv <- do.call(survminer::ggsurvplot, opts)

  ggsurv$plot$theme <- theme(
    plot.margin = unit(c(0,0,0,0), "pt"),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.title.y = element_text(margin = margin(r = -unit(90 + axis.title.y.nudge, "lines"))),
    axis.text = element_text(size = rel(1)),
    legend.position = legend,
    legend.background = element_rect(fill = NA),
    legend.text = element_text(size = rel(1))
  )

  # adjust the theme of the risk table
  ggsurv$table$theme <- theme(
    plot.margin = unit(c(0,0,0,0), "pt"),
    plot.title = element_text(hjust = 0, face = "bold", size = rel(1), margin = margin(b = 2)),
    plot.title.position = "plot",
    axis.title = element_blank(),
    axis.text = element_text(size = rel(1), color = ggplot2::calc_element("text", theme_get())$colour),
    axis.text.y = element_text(hjust = 0, size = rel(1)),
    axis.text.x = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )
  if (scibeautify %||% F) ggsurv$table$layers[[1]]$data$llabels <- scibeautify(ggsurv$table$layers[[1]]$data$llabels)
  ggsurv$table <- ggsurv$table + gg_themelock()

  # # convert risk table to grob and move the y labels one row to the left
  # riskGrob <- ggplot2::ggplotGrob(ggsurv$table)
  # ylab_grob_ind <- which(riskGrob$layout$name == "axis-l")
  # ylab_row <- riskGrob$layout[ylab_grob_ind, "l"]
  # swap_idx <- c(ylab_row - 1, ylab_row)
  # riskGrob$widths <- replace(riskGrob$widths, ylab_row - 1, riskGrob$widths[ylab_row])
  # riskGrob$widths <- replace(riskGrob$widths, ylab_row, unit(0, "pt"))
  # riskGrob$layout[ylab_grob_ind, "l"] <- ylab_row - 1
  # riskGrob$layout[ylab_grob_ind, "r"] <- ylab_row - 1
  # riskGrob$grobs[[ylab_grob_ind]]$vp$x <- unit(0, "npc")
  # riskGrob$grobs[[ylab_grob_ind]]$vp$justification <- c(0, 0.5)

  # compose the patchwork
  ggsurv$plot /
  as.ggGeomTextModify(ggsurv$table) +
  patchwork::plot_layout(heights = grid::unit.c(unit(1, "null"), unit(length(fit$strata), "lines") + unit(6, "pt")))
}

y <- function() {
  x <- x
  message("hello")
  plot(x)
}
