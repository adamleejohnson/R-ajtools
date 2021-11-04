#' @title Resave
#' @description Add objects to existing Rdata file
#' @inheritParams base::save
#' @seealso [base::save()]
#' @export
resave <- function(...,
                   list = character(),
                   file = stop("'file' must be specified"),
                   compress = "xz",
                   ascii = FALSE,
                   version = NULL,
                   envir = parent.frame(),
                   compression_level,
                   eval.promises = TRUE,
                   precheck = TRUE) {
  #  add objects to existing Rdata file. Original code written by "flodel"
  # on StackOverflow (http://www.linkedin.com/in/florentdelmotte)  .
  local({
    previous <- load(file)
    var.names <- c(list, as.character(substitute(list(...)))[-1L])
    for (var in var.names) assign(var, get(var, envir = parent.frame()))
    base::save(
      list = unique(c(previous, var.names)),
      file = file,
      compress = comress,
      ascii = ascii,
      version = version,
      envir = envir,
      compression_level = compression_level,
      eval.promises = eval.promises,
      precheck = precheck)
  })
}
