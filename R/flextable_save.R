#' @title Save a flextable to disk in several formats
#' @description Save a flextable to disk as an html, rds (compressed), and tsv.gz (containing the dataframe)
#' @param data Input flextable
#' @param name Output file name
#' @param path Output file path
#' @param overwrite Whether to overwrite an existing file. Default = FALSE, which appends a numeric index to the filename.
#' @return The original flextable, unmodified
#' @export
flextable_save <- function(data,
                           name = waiver(),
                           path = waiver(),
                           overwrite = FALSE) {

  if (!inherits(data, "flextable")) {
    stop("Object must be a flextable")
  }

  if (is_waive(name)) name <- deparse(substitute(data))
  if (is_waive(path)) path <- getwd()

  valid_ext <- c("rds", "html", "tsv.gz", "docx")

  # get name of flextable object in parent environment & validate
  if (xfun::file_ext(name) %in% valid_ext) name <- xfun::sans_ext(name)
  ft_object_name <- name
  while (!grepl("^[A-Za-z0-9_\\-\\. ]+$", ft_object_name)) {
    fixed_name <- gsub(" ", "_", trimws(gsub("[^A-Za-z0-9_\\-\\.]", " ", ft_object_name)))
    cat("Invalid output name \"", ft_object_name, "\"", sep = "")
    new_name <- readline(prompt = paste0("Enter new name (default = ", fixed_name, "): "))
    ft_object_name <- if (new_name == "") fixed_name else new_name
  }

  # create output directory
  if (xfun::is_rel_path(path)) path <- getwd() %//% path
  path <- path %//% ft_object_name
  dir.create(file.path(path), showWarnings = FALSE, recursive = TRUE)

  # don't overwrite old files; append an index instead
  if (!overwrite) {
    append_ind <- 1
    ft_object_name_apppend <- ft_object_name
    overwrite <- interactive()
    while (any(file.exists(path %//% ft_object_name_apppend %++% "." %++% valid_ext))) {
      if (overwrite) {
        overwrite <- utils::askYesNo(paste0("File '", ft_object_name_apppend, "' exists. Overwrite?"), default = F)
        if (overwrite) break
      }
      ft_object_name_apppend <- paste0(ft_object_name, "_", append_ind)
      append_ind <- append_ind + 1
    }
    ft_object_name <- ft_object_name_apppend

    if (file.exists(path %//% ft_object_name_apppend %++% ".html"))
      file.remove(path %//% ft_object_name %++% ".html")
  }

  readr::write_rds(data, path %//% ft_object_name %++% ".rds", compress = "xz", compression = 9)
  readr::write_tsv(data$body$dataset, path %//% ft_object_name %++% ".txt.gz", quote = "all")
  openxlsx::write.xlsx(data$body$dataset, path %//% ft_object_name %++% ".xlsx", overwrite = T)
  flextable::save_as_html(data, path = path %//% ft_object_name %++% ".html", title = ft_object_name)
  flextable::save_as_docx(data, path = path %//% ft_object_name %++% ".docx", pr_section = officer::prop_section(page_margins = officer::page_mar(gutter = 0)))

  invisible(data)
}
