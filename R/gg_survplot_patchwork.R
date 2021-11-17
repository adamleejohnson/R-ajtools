#' @title Convert a ggsurvplot to a patchwork
#' @description A thin wrapper around [survminer::ggsurvplot()] that creates a survival plot and risk table, converts plots to a patchwork (see [`patchwork::patchwork`]), and applies some custom formatting options.
#' @inheritParams survminer::ggsurvplot
#' @inheritDotParams survminer::ggsurvplot
#' @param scibeautify Whether to apply [scibeautify()] to the numbers in the risk table
#' @param axis.title.y.margin.r Custom right-margin for the y-axis title of the plot, in units of "lines"
#' @return A patchwork
#' @export
ggsurvplot_patchwork <- function(fit,
                                 scibeautify = T,
                                 axis.title.y.margin.r = 3,
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

  # specify axis.text color from the current theme (fix for some bug...)
  axis.text <- ggplot2::calc_element("text", theme_get())

  # convert components to grobs
  plotGrob <- ggplot2::ggplotGrob({
    ggsurv$plot$theme <- theme(
      plot.margin = unit(c(0,0,0,0), "pt"),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = axis.title.y.margin.r, unit = "lines")),
      axis.text = element_text(size = rel(1), color = axis.text$colour),
      legend.position = c(0.25, 0.6),
      legend.background = element_rect(fill = NA),
      legend.text = element_text(size = rel(1))
    )
    ggsurv$plot
  })

  # move the plot y-axis title grob one position to the right
  y_title_grob_ind <- which(plotGrob$layout$name == "ylab-l")
  y_title_row <- plotGrob$layout[y_title_grob_ind, "l"]
  plotGrob$widths <- replace(plotGrob$widths, y_title_row, unit(0, "pt"))
  plotGrob$layout[y_title_grob_ind, "l"] <- y_title_row + 1
  plotGrob$layout[y_title_grob_ind, "r"] <- y_title_row + 1
  plotGrob$grobs[[y_title_grob_ind]]$vp$parent$x <- unit(1, "npc") - unit(2, "lines")

  riskGrob <- ggplotGrob({
    ggsurv$table$theme <- theme(
      plot.margin = unit(c(5,0,0,0), "pt"),
      plot.title = element_text(hjust = 0, face = "bold", size = rel(1), margin = margin(b = 4)),
      plot.title.position = "plot",
      axis.title = element_blank(),
      axis.text = element_text(size = rel(1), color = axis.text$colour),
      axis.text.y = element_text(hjust = 0, size = rel(1)),
      axis.text.x = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )
    if (scibeautify %||% F) ggsurv$table$layers[[1]]$data$llabels <- scibeautify(ggsurv$table$layers[[1]]$data$llabels)
    ggsurv$table + gg_themelock()
  })

  # compose the patchwork
  patchwork::wrap_ggplot_grob(plotGrob) /
  patchwork::wrap_ggplot_grob(riskGrob) +
  patchwork::plot_layout(heights = c(.85, .15))
}
