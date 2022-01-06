.onLoad <- function(...) {
  patchwork__register_s3_method("patchwork", "plot_table", "wrapped_patch", plot_table.wrapped_patch)
  # registerS3method("plot_table", "wrapped_patch", plot_table.wrapped_patch, envir = asNamespace("patchwork"))
}

.onUnload <- function (libpath) {
  library.dynam.unload("ajtools", libpath)
}
