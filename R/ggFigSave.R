#' @title Output and save a ggplot object
#' @description Outputs a ggplot object as a pdf with standard scientific journal settings, and saves and rds object
#' @param ggdata ggplot2 object
#' @param name Filename (optional, defaults to name of the input ggplot object)
#' @param format Output file format, one of "pdf", "eps", "ps", "tex", "jpeg", "tiff", "png", "bmp", "svg".
#' @param aspect.ratio Ratio of width to height. Default = 1.
#' @param open Open the image file after creation. Default = TRUE.
#' @param overwrite Whether to overwrite an existing file. Default = FALSE, which appends a numeric index to the filename.
#' @param save_rds Whether to save an RDS file of the ggplot object. For large plots, can be very time-consuming.
#' @inheritParams ggplot2::ggsave
#' @return Original ggplot object, invisibly.
#' @export
ggfigsave <- function(
  ggdata,
  name = deparse(substitute(ggdata)),
  path = paste0(getwd(), "/figures"),
  dpi = 600,
  width = 6.5,
  height = NA,
  aspect.ratio = 1,
  units = c("in", "cm", "mm", "px"),
  format = c("pdf", "eps", "ps", "tex", "jpeg", "tiff", "png", "bmp", "svg"),
  open = TRUE,
  overwrite = FALSE,
  save_rds = TRUE) {

  units <- match.arg(units)
  format <- match.arg(format)

  # create output directory
  if (xfun::is_rel_path(path)) path <- getwd() %//% path
  dir.create(file.path(path), showWarnings = FALSE)

  # get name of ggplot object in parent environment & validate
  plot_object_name <- name
  while (!grepl("^[A-Za-z0-9_\\-\\. ]+$", plot_object_name)) {
    fixed_name <- gsub(" ", "_", trimws(gsub("[^A-Za-z0-9_\\-\\.]", " ", plot_object_name)))

    cat("Invalid output name \"",plot_object_name,"\"", sep = "")
    new_name <- readline(prompt = paste0("Enter new name (default = ", fixed_name, "): "))

    plot_object_name <- if (new_name == "") fixed_name else new_name
  }

  # don't overwrite old files; append an index instead
  if (!overwrite) {
    append_ind <- 1
    plot_object_name_apppend <- plot_object_name
    while (any(file.exists(path %//% plot_object_name_apppend %++% "." %++% c("rds", format)))) {
      if (interactive()) {
        if (utils::askYesNo(paste0("File '",plot_object_name_apppend,"' exists. Overwrite?"), default = F)) break
      }
      plot_object_name_apppend <- paste0(plot_object_name, "_", append_ind)
      append_ind <- append_ind + 1
    }
    plot_object_name <- plot_object_name_apppend
  }

  # save as rds
  if (save_rds) {
    readr::write_rds(ggdata, path %//% plot_object_name %++% ".rds", compress = "xz", compression = 9)
  }

  # save as rendered image
  output_img_name <- paste0(plot_object_name, ".", format)
  if (is.na(width)) width <- height * aspect.ratio
  if (is.na(height)) height <- width / aspect.ratio
  message("Saving '", output_img_name, "' as a ", width, units, " x ", height, units, " image at ", dpi, " dpi")
  ggplot2::ggsave(
    filename = output_img_name,
    path = path,
    plot = ggdata,
    device = format,
    dpi = dpi,
    width = width,
    height = height,
    units = units
  )

  # if on macos, adjust dpi with sips, and open
  if (Sys.info()["sysname"] == "Darwin") {
    img_path <- "'" %++% path %//% output_img_name %++% "'"

    if (format %in% c("png", "tiff", "bmp"))
      system(paste("sips -s dpiHeight", dpi, "-s dpiWidth", dpi, img_path), ignore.stdout = T)

    if (format == "jpeg") {
      # workaround for bug in sips that does not change dpi on jpegs
      system(paste("sips -s format png -s dpiHeight", dpi, "-s dpiWidth", dpi, img_path), ignore.stdout = T, ignore.stderr = T)
      system(paste("sips -s format jpeg -s dpiHeight", dpi, "-s dpiWidth", dpi, img_path), ignore.stdout = T)
    }

    if (open)
      system(paste("open ", img_path))
  }

  invisible(ggdata)
}
