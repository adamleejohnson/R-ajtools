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
                   eval.promises = TRUE,
                   precheck = TRUE) {

  previous <- if (file.exists(file)) load(file) else NULL
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) {
    assign(var, get(var, envir = envir))
    if (is.null(eval(as.symbol(var)))) {
      previous <- previous[which(previous != var)]
      var.names <- var.names[which(var.names != var)]
    }
  }
  base::save(
    list = unique(c(previous, var.names)),
    file = file,
    compress = compress,
    ascii = ascii,
    version = version,
    eval.promises = eval.promises,
    precheck = precheck)
}
