.onLoad <- function(...) {
  utils::assignInNamespace("+.gg", add_ggthemelock, "ggplot2")

  patchwork:::register_s3_method("patchwork", "plot_table", "wrapped_patch", plot_table.wrapped_patch)
  # registerS3method("plot_table", "wrapped_patch", plot_table.wrapped_patch, envir = asNamespace("patchwork"))
}