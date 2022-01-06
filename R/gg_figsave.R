#' @title Output and save a ggplot object
#' @description Outputs a ggplot object as a pdf with standard scientific journal settings, and saves and rds object
#' @param ggdata ggplot2 object
#' @param name Filename (optional, defaults to name of the input ggplot object)
#' @param format Output file format, any of "rds", "pdf", "eps", "ps", "tex", "jpeg", "tiff", "png", "bmp", "svg".
#' @param aspect_ratio Ratio of width to height. Default = 1.
#' @param open Open the image file after creation. Default = TRUE.
#' @param overwrite Whether to overwrite an existing file. Default = FALSE, which appends a numeric index to the filename.
#' @inheritParams ggplot2::ggsave
#' @return Original ggplot object, invisibly.
#' @export
ggfigsave <- function(ggdata,
                      name = waiver(),
                      path = waiver(),
                      dpi = 600,
                      width = 6.5,
                      height = waiver(),
                      aspect_ratio = 1,
                      units = c("in", "cm", "mm", "px"),
                      format = c("pdf", "png", "rds"),
                      open = FALSE,
                      overwrite = FALSE) {
  if (!ggplot2::is.ggplot(ggdata)) {
    stop("Object must be a ggplot.")
  }

  valid_ext <- c("pdf", "eps", "ps", "tex", "jpeg", "tiff", "png", "bmp", "svg", "rds")
  format <- match.arg(format, valid_ext, several.ok = TRUE)
  units <- match.arg(units)
  if (is_waive(name)) name <- deparse(substitute(ggdata))
  if (is_waive(path)) path <- getwd()
  if (is_waive(height)) height <- NA

  # get name of ggplot object in parent environment & validate
  if (xfun::file_ext(name) %in% valid_ext) name <- xfun::sans_ext(name)
  plot_object_name <- name
  while (!grepl("^[A-Za-z0-9_\\-\\. ]+$", plot_object_name)) {
    fixed_name <- gsub(" ", "_", trimws(gsub("[^A-Za-z0-9_\\-\\.]", " ", plot_object_name)))
    cat("Invalid output name \"", plot_object_name, "\"", sep = "")
    new_name <- readline(prompt = paste0("Enter new name (default = ", fixed_name, "): "))
    plot_object_name <- if (new_name == "") fixed_name else new_name
  }

  # create output directory
  if (xfun::is_rel_path(path)) path <- getwd() %//% path
  path <- path %//% plot_object_name
  dir.create(file.path(path), showWarnings = FALSE, recursive = TRUE)

  # don't overwrite old files; append an index instead
  if (!overwrite) {
    append_ind <- 1
    plot_object_name_apppend <- plot_object_name
    overwrite <- interactive()
    while (any(file.exists(path %//% plot_object_name_apppend %++% "." %++% c("rds", format)))) {
      if (overwrite) {
        overwrite <- utils::askYesNo(paste0("File '", plot_object_name_apppend, "' exists. Overwrite?"), default = F)
        if (overwrite) break
      }
      plot_object_name_apppend <- paste0(plot_object_name, "_", append_ind)
      append_ind <- append_ind + 1
    }
    plot_object_name <- plot_object_name_apppend
  }

  for (fmt in unique(format)) {
    if (fmt == "rds") {
      # save as rds
      obj_sz <- utils::object.size(rlang::duplicate(ggdata))
      save_rds <- T
      if (obj_sz > 100e6 && interactive()) {
        save_rds <- utils::askYesNo("Object size is " %++% format(obj_sz, units = "MB", standard = "SI") %++% ". Are you sure you want to save an .rds?", default = T)
      }
      if (save_rds) {
        message("Saving '", plot_object_name, ".rds'")
        readr::write_rds(
          ggdata,
          path %//% plot_object_name %++% ".rds",
          compress = "xz",
          compression = 9
        )
      }
    } else {
      # save as rendered images
      output_img_name <- plot_object_name %++% "." %++% fmt
      if (is.na(width)) width <- height * aspect_ratio
      if (is.na(height)) height <- width / aspect_ratio
      message("Saving '", output_img_name, "' as a ", format(width), units, " x ", format(height), units, " image at ", dpi, " dpi")
      ggplot2::ggsave(
        filename = output_img_name,
        path = path,
        plot = ggdata,
        device = if (fmt == "pdf" && grDevices__cairoVersion() != "") grDevices::cairo_pdf else fmt,
        dpi = dpi,
        width = width,
        height = height,
        units = units
      )

      # if on macos, adjust dpi with sips, and open
      if (Sys.info()["sysname"] == "Darwin") {
        img_path <- "'" %++% path %//% output_img_name %++% "'"

        if (fmt %in% c("png", "tiff", "bmp")) {
          system(paste("sips -s dpiHeight", dpi, "-s dpiWidth", dpi, img_path), ignore.stdout = T)
        }

        # workaround for bug in sips that does not change dpi on jpegs
        if (fmt == "jpeg") {
          system(
            paste("sips -s format png -s dpiHeight", dpi, "-s dpiWidth", dpi, img_path),
            ignore.stdout = T,
            ignore.stderr = T
          )
          system(
            paste("sips -s format jpeg -s dpiHeight", dpi, "-s dpiWidth", dpi, img_path),
            ignore.stdout = T
          )
        }

        if (open) system(paste("open ", img_path))
      }
    }
  }

  invisible(ggdata)
}
