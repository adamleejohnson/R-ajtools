.onLoad <- function(...) {
  patchwork__register_s3_method("patchwork", "plot_table", "wrapped_patch", plot_table.wrapped_patch)
  patchwork__register_s3_method("patchwork", "plot_table", "table_patch", plot_table.table_patch)
  patchwork__register_s3_method("patchwork", "plot_table", "ggGeomTextModify", plot_table.ggGeomTextModify)
}

.onUnload <- function (libpath) {
  library.dynam.unload("ajtools", libpath)
}
