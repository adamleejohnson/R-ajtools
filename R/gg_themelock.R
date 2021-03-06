#' @title Lock the theme elements of a ggplot
#' @description **ggThemeLock** introduces the ability to “lock” the state of a ggplot theme during a ggplot's assembly. When `gg_themelock()` is used in the composition of a ggplot (e.g., using the the '`+`' operator), the theme elements are stored and the object is placed in a “locked” state. The subsequent addition of theme elements that have been previously locked is ignored, and the saved theme elements are preserved. New theme elements that haven’t been locked can still be added.
#' @details A plot object can be locked by either including `gg_themelock()` in its assembly using the '`+`' operator, or by passing the plot object as an argument to `gg_themelock()`. A theme object can only be locked by passing it as the argument to `gg_themelock()`. See examples below.
#'
#' Take note, addition of ggplot objects is not necessarily associative or commutative. With this in mind, ggThemeLock was built to be as intuitive as possible. Calls to `gg_themelock()` will "lock" the state of the theme on the left-hand (LHS) side of an addition expression. However, if the right-hand (RHS) side of an addition expression is in a "locked" state (e.g., adding a locked theme to a theme), the addition behavior depends on whether the LHS side is also locked. If both the LHS and RHS are locked, the locked elements are merged and re-locked (with priority going to the LHS); if the LHS is unlocked and the RHS is locked, the locked elements of the RHS remain locked.
#' @param object A ggplot or theme object to apply a lock to; optional. See description and examples.
#' @examples
#' library(ggplot2)
#'
#' # ========================================
#' # set a theme element and lock the state
#' ggplot(mtcars) +
#'   aes(x = mpg, y = hp) +
#'   geom_point() +
#'   theme(axis.text = element_text(color = "red")) +
#'   gg_themelock() +
#'   theme(
#'     axis.text = element_text(color = "blue"),  # this element is locked and not applied
#'     axis.title = element_text(color = "green") # this element is applied
#'   )
#'
#' # ========================================
#' # build a theme and lock it, then use it in a plot
#' my_theme <- theme(
#'     axis.text = element_text(color = "blue"),
#'     axis.title = element_blank()
#'   )
#' my_theme <- gg_themelock(my_theme)
#' ggplot(mtcars) +
#'   aes(x = mpg, y = hp) +
#'   geom_point() +
#'   my_theme +
#'   theme_classic() # elements in this theme that have been defined by my_theme are ignored
#'
#' # ========================================
#' # lock and relock a theme multiple times during construction
#' # (try commenting out the gg_themelock() lines one at a time)
#' ggplot(mtcars) +
#'   aes(x = mpg, y = hp) +
#'   geom_point() +
#'   theme(axis.text = element_text(color = "blue")) +
#'   gg_themelock() +
#'   theme(
#'     axis.text = element_text(color = "red"),
#'     axis.title = element_blank()
#'   ) +
#'   gg_themelock() +
#'   theme_bw()
#' @export
gg_themelock <- function(object) {
  if (missing(object)) {
    return( structure(list(), class = "ggthemelock") )
  }
  if (ggplot2::is.ggplot(object) || ggplot2::is.theme(object)) {
    t <- extract_theme(remove_themelock(object))
    return( apply_themelock(object, t) )
  }
  stop("Cannot apply gg_themelock() to object `", deparse(substitute(object)), "`")
}

#' @export
ggplot_add.ggthemelock <- function(object, plot, object_name) {

  # RHS is a ggplot: don't handle this case
  if (ggplot2::is.ggplot(object)) return( NextMethod("ggplot_add") )

  # RHS is a theme:
  if (ggplot2::is.theme(object)) {

    saved_theme <-
      if (is.themelocked(plot))
        extract_theme(object) + extract_theme(plot)
      else
        extract_theme(object)

    r <- remove_themelock(plot) + remove_themelock(object)
    r <- apply_themelock(r, saved_theme)
    return(r)

  }

  # Empty call to gg_themelock() on RHS:
  saved_theme <- extract_theme(remove_themelock(plot)) + extract_theme(plot)
  apply_themelock(plot, saved_theme)
}

#' @export
ggplot_build.ggthemelock <- function(plot) {
  plot <- plot + extract_theme(plot)
  NextMethod("ggplot_build", plot)
}

#' @rdname gg_themelock
#' @export
is.themelocked <- function(object) inherits(object, "ggthemelock")

as.themelocked <- function(e) {
  if (!is.themelocked(e)) class(e) <- c("ggthemelock", class(e))
  e
}

extract_theme <- function(e) {
  t <-
    if (is.themelocked(e)) attr(e, "themelock")
    else if (ggplot2::is.ggplot(e)) do.call(ggplot2::theme, e$theme)
    else if (ggplot2::is.theme(e)) e
  attr(t, "complete") <- FALSE
  return(t)
}

apply_themelock <- function(e, saved_theme) {
  e <- e + saved_theme
  attr(e, "themelock") <- saved_theme
  attr(e, "pend_themelock") <- NULL
  as.themelocked(e)
}

remove_themelock <- function(e) {
  attr(e, "themelock") <- NULL
  attr(e, "pend_themelock") <- NULL
  class(e) <- setdiff(class(e), "ggthemelock")
  e
}
