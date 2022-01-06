#' @title Apply scibeautify to a flextable
#' @description This is a wrapper around both [flextable::flextable()] and [scibeautify()] that applies [flextable::as_sup()] where needed to scientific notation superscripts.
#' @param data Input data frame
#' @param .across Tidy-eval selector to apply scibeautify to
#' @inheritDotParams scibeautify -output_format -sci_format -.data
#' @return A flextable
#' @export
flextable_scibeautify <- function(data,
                                  .across = c(),
                                  ...) {

  fn_scibeaut <- function(x) {
    args <- modifyList(
      list(...),
      list(
        .data = x,
        output_format = "ascii",
        sci_format = "e"
      )
    )
    x <- do.call(scibeautify, args)
    x <- gsub("-", "\U2212", x)
    return(x)
  }

  if (inherits(data, "flextable")) {
    ft <- data
    data <- ft$body$dataset

  } else if (is.data.frame(data)) {
    ft <- flextable::flextable(data)

  } else {
    stop("Unrecognized input type")
  }

  data <- as_tibble(data)
  tidycols <- tidyselect::eval_select(substitute(.across), data)
  data <- data %>% mutate(across(tidycols, fn_scibeaut))

  for (col in tidycols) {
    val <- data %>% pull(col)
    has_exp <- grepl("e(.*)$", val, perl = T)
    exp <- ifelse(has_exp, sub(".*e(.*)$", "\\1", val, perl = T), NA)
    val <- ifelse(has_exp, sub("e(.*)$", "", val, perl = T), val)

    ft <- flextable::compose(
      ft,
      j = col,
      value = flextable::as_paragraph(
        val, ifelse(has_exp, "\U2009\U00D7\U2009" %++% "10", ""), flextable::as_sup(exp)
      )
    )
  }

  ft %>%
    set_table_properties(layout = "autofit")
}
