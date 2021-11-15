#' @title Remove NULL recursively
#' @description Recursively remove NULL objects from nested lists (e.g. ggplot themes)
#' @param x List-like object to recurse
#' @export
remove_null <- function(x) {
  if (is.list(x)) {
    for (i in names(x)){
      x[[i]] <- if (is.null(x[[i]])) NULL else remove_null(x[[i]])
    }
    x
  } else x
}
