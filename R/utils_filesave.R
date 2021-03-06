#' @title Append a datestamp to a filename
#' @description Will also append a counter to prevent overwriting existing files
#'
#' @param input_filename Input filename(s) to check, as characters
#' @param path Optional path to prepend to filename. Defaults to current working directory. If a relative path is provided, path is assumed relative to the current working directory.
#'
#' @return A modified filepath
#' @export
file_datestamp <- function(input_filename = stop("Input filename(s) required"),
                           path = getwd()) {
  use_path_default <- !("path" %in% names(match.call()[-1]))

  stopifnot(is.character(path))
  stopifnot(length(path) == 1)

  input_filename <- path.expand(input_filename)
  if (xfun::is_rel_path(path)) path <- getwd() %//% path
  path <- path.expand(path)

  date_stamp <- format(Sys.Date(), "%Y_%m_%d")
  suffixes <- as.vector(sapply(c("", 1:9), function(x) paste0(letters, x)))

  sapply(input_filename, function(filename) {
    ext_extract <- "^(.*?)\\.([^.]+(?:\\.(?:gz|bz2|xz|zip|zst))?)$"
    stem <- sub(ext_extract, "\\1", filename)
    ext <- sub(ext_extract, "\\2", filename)
    if (ext != "") ext <- "." %++% ext
    stem_date <- stem %++% "." %++% date_stamp
    index <- 1
    test_name <- stem_date %++% ext
    while (file.exists(path %//% test_name)) {
      test_name <- stem_date %++% suffixes[index] %++% ext
      index <- index + 1
    }
    message("Filename ", test_name)

    if (use_path_default) {
      return(test_name)
    } else {
      return(path %//% test_name)
    }
  }, USE.NAMES = F)
}

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
    precheck = precheck
  )
}

#' @title Write a zstd-compressed parquet file
#' @description A basic wrapper around [arrow::write_parquet()].
#' @inheritParams arrow::write_parquet
#' @inheritDotParams arrow::write_parquet
#' @export
write_parquet_zstd <- function(x,
                               sink,
                               compression_level = 6,
                               write_statistics = FALSE, ...) {

  if (!endsWith(sink, ".zst.parquet")) {
    if (endsWith(sink, ".parquet")) sink <- xfun::sans_ext(sink) %++% ".zst.parquet"
    else sink <- sink %++% ".zst.parquet"
  }
  args <- list(
    x = x,
    sink = sink,
    compression = "zstd",
    compression_level = compression_level,
    write_statistics = write_statistics,
    version = "2.0"
  )
  args <- modifyList(list(...), args)
  do.call(arrow::write_parquet, args)
}
