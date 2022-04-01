#' Apply the stored theme in a wrapped patchwork object (patchwork::wrap_element()) to individual textGrob elements
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
      if (inherits(grobs_attr[[n]], "grob") && inherits(grobs_attr[[n]], "text")) {
        grobs_attr[[n]] <- recurse_apply_theme(list(grobs_attr[[n]]), theme_text)[[1]]
      }
      if ("grobs" %in% names(grobs_attr[[n]])) {
        grobs_attr[[n]]$grobs <- recurse_apply_theme(grobs_attr[[n]]$grobs, theme_text)
      }
    }
    attr(x, "grobs") <- grobs_attr
  }
  NextMethod("plot_table")
}

#' Special method for when using patchwork:::wrap_ggplot_grob(), instead of patchwork::wrap_element()
#' @noRd
plot_table.table_patch <- function(x, guides) {
  if ("table" %in% names(attributes(x))) {
    theme_text <- ggplot2::calc_element("text", ggplot2::theme_get() + x$theme)
    gTable <- attr(x, "table")
    if ("grobs" %in% names(gTable)) {
      gTable$grobs <- recurse_apply_theme(gTable$grobs, theme_text)
      attr(x, "table") <- gTable
    }
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

    } else if (inherits(e, "gList") || inherits(e, "gTree") || identical(names(gList)[i], "grobs")) {
      e <- recurse_apply_theme(e, theme_text)
    }

    if (!is.null(e)) gList[[i]] <- e
  }
  return(gList)
}


## ------------------------------------------------------------------------
#' Add ability to modify GeomText elements in a ggplot before being plotted as part of a patchwork
#' @noRd
plot_table.ggGeomTextModify <- function(x, guides) {
  if (ggplot2::is.ggplot(x)) {

    theme_text <- ggplot2::calc_element("text", ggplot2::theme_get() + x$theme)

    for (i in seq_along(x$layers)) {
      if (any(sapply(c("GeomText", "GeomTextRepel"), function(c) inherits(x$layers[[i]]$geom, c)))) {

        x$layers[[i]]$aes_params$colour     <- theme_text$colour
        x$layers[[i]]$aes_params$size       <- theme_text$size * 0.35
        x$layers[[i]]$aes_params$fontface   <- theme_text$face
        x$layers[[i]]$aes_params$family     <- theme_text$family
        x$layers[[i]]$aes_params$lineheight <- theme_text$lineheight
      }
    }
  }

  NextMethod("plot_table")
}

#' @noRd
as.ggGeomTextModify <- function(obj) {
  if (!inherits(obj, "ggGeomTextModify")) class(obj) <- c("ggGeomTextModify", class(obj))
  obj
}
