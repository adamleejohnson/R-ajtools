#' Apply the stored theme in a wrapped patchwork object to individual textGrob elements
#' -- when displaying a wrapped_patch, retrieve the ggtheme, extract info
#'    about text color, size, etc, and apply it to all text grobs
#' -- this allows you to do:
#'    wrap_element(some_tableGrob) & theme(text = element_text(color = "red"))
#' @noRd
plot_table.wrapped_patch <- function(x, guides) {
  if ("grobs" %in% names(attributes(x))) {
    theme_text <- ggplot2::calc_element("text", utils::modifyList(ggplot2::theme_get(), remove_null(x$theme)))
    grobs_attr <- attr(x, "grobs")
    for (n in (names(grobs_attr))) {
      grobs <- grobs_attr[[n]]$grobs
      for (i in seq_along(grobs)) {
        g <- grobs[[i]]
        if (inherits(g, "text") && inherits(g, "grob")) {
          g$gp$col        <- theme_text$colour      %||%  g$gp$col
          g$gp$fontfamily <- theme_text$family      %||%  g$gp$fontfamily
          g$gp$lineheight <- theme_text$lineheight  %||%  g$gp$lineheight
          g$gp$fontsize   <- theme_text$size        %||%  g$gp$fontsize
          g$gp$fontface   <- switch(theme_text$face %||% T, "plain" = 1, "bold" = 2, "italic" = 3, "bold.italic" = 4, T = g$gp$fontface)
          # g$hjust <- theme_text$hjust
          # g$vjust <- theme_text$vjust
          # g$rot <- theme_text$angle

          grobs[[i]] <- g
        }
      }
      grobs_attr[[n]]$grobs <- grobs
    }
    attr(x, "grobs") <- grobs_attr
  }

  NextMethod("plot_table")
}
