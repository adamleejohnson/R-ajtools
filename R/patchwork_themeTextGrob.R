#' Apply the stored theme in a wrapped patchwork object to individual textGrob elements
#' -- when displaying a wrapped_patch, retrieve the ggtheme, extract info
#'    about text color, size, etc, and apply it to all text grobs
#' -- this allows you to do:
#'    wrap_element(some_tableGrob) & theme(text = element_text(color = "red"))
#' @noRd
plot_table.wrapped_patch <- function(x, guides) {
  if ("grobs" %in% names(attributes(x))) {
    theme_text <- ggplot2::calc_element("text", ggplot2::theme_get() + x$theme)
    grobs_attr <- attr(x, "grobs")
    for (n in names(grobs_attr)) {
      if (is.null(grobs_attr[[n]])) next
      grobs_attr[[n]]$grobs <- recurse_apply_theme(grobs_attr[[n]]$grobs, theme_text)
    }
    attr(x, "grobs") <- grobs_attr
  }

  NextMethod("plot_table")
}

#' @noRd
plot_table.table_patch <- function(x, guides) {
  if ("table" %in% names(attributes(x))) {
    theme_text <- ggplot2::calc_element("text", ggplot2::theme_get() + x$theme)
    gTable <- attr(x, "table")
    gTable$grobs <- recurse_apply_theme(gTable$grobs, theme_text)
    attr(x, "table") <- gTable
  }

  NextMethod("plot_table")
}

recurse_apply_theme <- function(gList, theme_text) {

  if (!is.list(gList)) return(gList)

  for (i in seq_along(gList)) {
    e <- gList[[i]]
    if (inherits(e, "grob") && inherits(e, "text")) {

      # apply the edits
      e$gp$col        <- theme_text$colour      %||%  e$gp$col
      e$gp$fontfamily <- theme_text$family      %||%  e$gp$fontfamily
      e$gp$lineheight <- theme_text$lineheight  %||%  e$gp$lineheight
      e$gp$fontsize   <- theme_text$size        %||%  e$gp$fontsize
      e$gp$fontface   <- switch(theme_text$face %||% T, "plain" = 1, "bold" = 2, "italic" = 3, "bold.italic" = 4, T = e$gp$fontface)

    } else if (inherits(e, "gList") || inherits(e, "gTree")) {
      e <- recurse_apply_theme(e, theme_text)
    }

    if (!is.null(e)) gList[[i]] <- e
  }

  return(gList)
}
