#' @title Source the R code from an RMarkdown file
#' @description Retrieve the R code from an RMarkdown file and source it into a new environment variable. Uses [knitr::purl()], which comments-out any chunks that are not meant to be evaluated (i.e., `eval = F`).
#'
#' @param path Path to RMarkdown file
#' @param suppress Supress all output from executed code?
#' @param wd Optional working directory to use for script execution. Defaults to the directory containing the RMarkdown file.
#'
#' @inheritDotParams base::source
#' @return A new environment containing the results of the sourced R code
#' @export
sourceRmd <- function(path, wd = dirname(path), suppress = T, ...) {

  input.path <- normalizePath(path)
  target_wd <- normalizePath(wd)
  current_wd <- getwd()
  tmp_file <- tempfile()
  on.exit(file.remove(tmp_file), add = TRUE)

  # perform purl in a separate R session (knitr won't let this work when called from an Rmd chunk)
  xfun::Rscript_call(knitr::purl, list(input = input.path, output = tmp_file, quiet = TRUE, documentation = 0))

  # source into a new environment
  env <- new.env()
  setwd(target_wd)
  opts <- modifyList(list(...), list(file = tmp_file, local = env))
  if (suppress) purrr::quietly(do.call)(source, opts)
  else do.call(source, opts)
  setwd(current_wd)
  return(env)
}


#' @title Read chunks from an Rmarkdown file
#' @description Extends the functionality of [knitr::read_chunk()] to read from .Rmd files.
#' @inheritParams knitr::read_chunk
#' @param prevent_overwrite Character vector of chunk labels in the current document to prevent overwriting of; default is just the "setup" chunk.
#' @export
#' @seealso [knitr::read_chunk()], [knitr::purl()]
read_chunk_rmd <- function(path,
                           labels = NULL,
                           prevent_overwrite = c("setup")) {

  input.path <- normalizePath(path)

  tmp_file <- tempfile()
  on.exit(file.remove(tmp_file), add = TRUE)

  # special treatment of the setup chunk; don't overwrite it
  for (label in prevent_overwrite) {
    chunk_content <- knitr::knit_code$get(label)
    on.exit(knitr::knit_code$set(label = chunk_content), add = TRUE)
  }

  # perform purl in a separate R session (knitr won't let this work when called from an Rmd chunk)
  xfun::Rscript_call(knitr::purl, list(input = input.path, output = tmp_file, quiet = TRUE))

  # read chunks
  knitr::read_chunk(tmp_file)
}
