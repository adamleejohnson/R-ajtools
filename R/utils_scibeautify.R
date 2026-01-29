#' @title Beautify a number in scientific format
#' @description Takes a number (or vector of numbers), and formats it in scientific notation as a string (or vector of strings) for use in printing. This function will format the number according to the specified number of significant digits, and using the specified format for scientific notation (e.g. x 10^n, eN, EN, etc). It will also correctly produce latex and html compatible strings.
#' @details `scibeautify.knit` will choose the correct output format (latex or html) based on the knit options at runtime. By default, the function will check whether the knit output is html or graphic (LaTeX), and use the appropriate string codes.
#' @inheritParams signif_beautify
#' @param .data Numeric vector or dataframe. Non-numeric vectors will be coerced to numeric only if numeric values are preserved.
#' @param sci_mode (Optional) Can be one of "auto" (automatically choose whether scientific notation is used based on string length optimization), "on", or "off" (scientific notation always on or off).
#' @param sci_format (Optional) Formatting of scientific notation. Can be one of "x" (times character), "." (center dot), "e" (lowercase e), "E" (uppercase E). Default = "x".
#' @param output_format (Optional) Output target. Can be one of "plain", "ascii" (same as plain), "unicode", "html", "latex", "plotmath", and "markdown". Default = "unicode".
#' @param justify_mode Mode to use for justifying a numbers. Can be "l" (left), "c" (center), "r" (right), or "d" (decimal). An "x" flag can be appended which will independently align the exponent portion of numbers in scientific notation, in addition to aligning the coefficient. Allowed values = `c("l", "c", "r", "d", "lx", "cx", "rx", "dx")`.
#' @param auto_ignore_int (Optional) If the data (or column) is all integers, will not truncate non-significant digits and will not use scientific notation
#' @param .cols Tidy selector for columns when `.data` is a dataframe. Default = `tidyr::everything()`.
#' @param .rows Vector of row numbers indicating where scibeautify should be applied.
#' @export
scibeautify <- function(.data,
                        sig_digits = 3,
                        output_format = c("unicode", "plain", "ascii", "html", "latex", "plotmath", "markdown"),
                        sci_mode = c("auto", "on", "off"),
                        sci_format = c("x", ".", "e", "E"),
                        justify_mode = c("none", "l", "c", "r", "d", "lx", "cx", "rx", "dx"),
                        pad_zeros = F,
                        decimal_mark = ".",
                        big_mark = ",",
                        big_interval = 3,
                        auto_ignore_int = T,
                        nsmall = NULL,
                        .cols = tidyr::everything(),
                        .rows) {

  # if calling scibeautify on a data.frame, pass .cols as a tidyselector and .rows as a row selector
  if (is.data.frame(.data)) {
    .cols <- enquo(.cols)
    this_call <- match.call()
    this_call[[1]] <- as.name("scibeautify.knit")
    this_call[[".data"]] <- as.name("x")
    fn_call <- function(x) eval(this_call)
    pos <- tidyselect::eval_select(.cols, .data)
    return({
      if (missing(.rows)) dplyr::mutate(.data, across(.cols = pos, .fns = fn_call))
      else ajtools::mutate_rows(.data, .rows, across(.cols = pos, .fns = fn_call))
    })
  }

  # try to coerce to numeric
  n <- tryCatch(
    { as.numeric(.data) },
    warning = function(w) return(.data),
    error = function(e) return(.data)
  )
  if (!is.numeric(n)) return(.data)

  # validate options
  justify_mode <- match.arg(justify_mode)
  output_format <- match.arg(output_format)
  sci_format <- match.arg(sci_format)
  sci_mode <- match.arg(sci_mode)

  # adjust options
  if (justify_mode == "none") justify_mode <- NA
  if (output_format == "plain") output_format <- "ascii"
  align_exp <- F
  if (!is.na(justify_mode)) {
    align_exp <- grepl("x", justify_mode)
    justify_mode <- gsub("x", "", justify_mode)
  }

  # auto_ignore integers
  if (auto_ignore_int && all(is_integer_num(n))) {
    sci_mode <- "off"
    sig_digits <- NULL
  }

  no_sci <- as.character(
    signif_beautify(n,
      sig_digits = sig_digits,
      decimal_mark = decimal_mark,
      big_mark = big_mark,
      big_interval = big_interval,
      pad_zeros = pad_zeros,
      nsmall = nsmall
    )
  )

  base <- 10
  n <- as.numeric(n)
  exp <- floor(log(abs(n))/log(base))
  coeff <- ifelse(n == 0, 0, n * base^(-exp))

  coeff_sci <- as.character(
    signif_beautify(coeff,
      sig_digits = sig_digits,
      decimal_mark = decimal_mark,
      big_mark = big_mark,
      big_interval = big_interval,
      pad_zeros = TRUE,
      nsmall = NULL
    )
  )

  # boolean vector for choosing sci or standard notation
  use_sci <-
    n != 0 &
    is.finite(n) &
    switch(sci_mode,
      "on" = TRUE,
      "off" = FALSE,
      { #auto select
        space_save <- nchar(no_sci) - nchar(coeff_sci)
        (space_save > 2) & abs(exp) > 1
      }
    )

  coeff_print <- ifelse(use_sci, coeff_sci, no_sci)
  exp_print <- ifelse(use_sci, paste0("E", exp), "")

  # cleanup coeff_print
  coeff_print <- gsub("Inf", "\U221E", coeff_print)

  blank <- rep("", length(use_sci))
  l_pad <- blank
  r_pad <- blank
  x_pad <- blank

  # create paddings for justification
  if (!is.na(justify_mode)) {
    coeff_pads <- justify.proportional(
      coeff_print,
      justify_mode = justify_mode,
      decimal = decimal_mark,
      tabular_numbers = T,
      tabular_char = "0",
      unicode_spaces = (output_format == "unicode")
    )
    l_pad <- coeff_pads[["l_pad"]]
    r_pad <- coeff_pads[["r_pad"]]
    x_pad <- justify.proportional(
      exp_print,
      justify_mode = "l",
      decimal = decimal_mark,
      tabular_numbers = T,
      tabular_char = "0"
    )[["r_pad"]]
  }

  # Convert sci notation to appropriate formats
  convert_sciformat <- function(x) {
    # format the exponent
    x <- switch(sci_format,
      "x" = ,
      "." = switch(output_format,
        "ascii" = ,
        "unicode" =  x,
        "html" =     gsub("([-\\d]+)$", "<sup>\\1</sup>", x, perl = T),
        "latex" =    gsub("([-\\d]+)$", "^{\\1}", x, perl = T),
        "plotmath" = gsub("([-\\d]+)$", "^{\\1}", x, perl = T),
        "markdown" = gsub("([-\\d]+)$", "^\\1^", x, perl = T),
      ),
      "e" = x,
      "E" = x
    )

    # replace the multiplication sign
    x <- gsub("^E(.+)", switch(output_format,
      "ascii" = switch(sci_format,
        "x" = paste0(" x ",base,"^","\\1"),
        "." = paste0(" . ",base,"^","\\1"),
        "e" = paste0("e","\\1"),
        "E" = paste0("E","\\1")
      ),
      "unicode" = switch(sci_format,
        "x" = paste0("\U2009\U00D7\U2009",base,"^","\\1"),
        "." = paste0("\U2009\U00B7\U2009",base,"^","\\1"),
        "e" = paste0("\U2009\U1D452\U2009","\\1"),
        "E" = paste0("\U2009\U1D5A4\U2009","\\1")
      ),
      "html" = switch(sci_format,
        "x" = paste0("&thinsp;&times;&thinsp;",base,"\\1"),
        "." = paste0("&thinsp;&middot;&thinsp;",base,"\\1"),
        "e" = paste0("&thinsp;<i>e</i>&thinsp;","\\1"),
        "E" = paste0("&thinsp;&#7431;&thinsp;","\\1")
      ),
      "latex" = switch(sci_format,
        "x" = paste0("\\\\times",base,"\\1"),
        "." = paste0("\\\\cdot", base, "\\1"),
        "e" = paste0("e","\\1"),
        "E" = paste0("\\\\scriptsize{\\\\mathrm{E}}\\\\normalsize{_}", "\\1")
      ),
      "plotmath" = switch(sci_format,
        "x" = paste0(" %*% ",base,"\\1"),
        "." = paste0(" %.% ", base, "\\1"),
        "e" = paste0(" * e * ","\\1"),
        "E" = paste0(" * E * ", "\\1")
      ),
      "markdown" = switch(sci_format,
        "x" = paste0("\U2009\U00D7\U2009",base,"\\1"),
        "." = paste0("\U2009\U00B7\U2009",base,"\\1"),
        "e" = paste0("\U2009\U1D452\U2009","\\1"),
        "E" = paste0("\U2009\U1D5A4\U2009","\\1")
      ),
    ), x, perl = T)

    return(x)
  }
  exp_print <- convert_sciformat(exp_print)
  x_pad <- convert_sciformat(x_pad)

  # convert paddings
  if (!is.na(justify_mode)) {
    convert_paddings <- function(x) {
      switch(output_format,
        "markdown" = ,
        "unicode" = ,
        "ascii" = gsub(".", " ", x),
        "html" = ifelse(x == "", "", paste0("<span style=\"visibility:hidden\">",x,"</span>")),
        "latex" = ifelse(x == "", "", paste0("\\phantom{{}",x,"{}}")),
        "plotmath" = ifelse(x == "", "", paste0("phantom( ",x," )"))
      )
    }
    convert_paddings_exp <- function(x) {
      switch(output_format,
        "markdown" = gsub("\\d", " ", x),
        "unicode" = ,
        "ascii" = x,
        "html" = ,
        "latex" = ,
        "plotmath" = convert_paddings(x)
      )
    }
    l_pad <- convert_paddings(l_pad)
    r_pad <- convert_paddings(r_pad)
    x_pad <- convert_paddings_exp(x_pad)
  }

  # compile the result
  result <- {
    if (align_exp) paste0(l_pad, coeff_print, r_pad, exp_print, x_pad)
    else {
      if (is.na(justify_mode)) paste0(l_pad, coeff_print, r_pad, exp_print, x_pad)
      else switch(justify_mode,
        "l" = paste0(l_pad, coeff_print, exp_print, x_pad, r_pad),
        "r" = paste0(l_pad, r_pad, x_pad, coeff_print, exp_print),
        "c" = {
          all_pad <- paste0(l_pad, x_pad, r_pad)
          paste0(all_pad, coeff_print, all_pad)
        },
        "d" = paste0(l_pad, coeff_print, exp_print, x_pad, r_pad)
      )
    }
  }

  # some final cleanup
  switch(output_format,
    "markdown" = ,
    "unicode" = {
      result <- gsub("-", "\U2212", result)
      result <- gsub("Inf", "\U221E", result)
    },
    "html" =  {
      result <- gsub("-", "&minus;", result)
      result <- gsub("\U221E", "&infin;", result)
    },
    "latex" = {
      result <- gsub("\U221E", "\\\\infty", result)
      # result <- paste0("$", result, "$")
    },
    "plotmath" = {
      result <- gsub("\U221E", " infinity ", result)
      result <- gsub("(phantom\\(.*\\))(\\S)", "\\1 * \\2", result)
      result <- gsub("(\\S)(phantom\\(.*?\\))", "\\1 * \\2", result)
      result <- gsub("phantom\\(\\s*([^\\d\\s])", "phantom(\"\" \\1", result, perl = T)
      result <- gsub("([\\d\\.]+)", "\"\\1\"", result, perl = T)
      result <- gsub("(phantom\\(\\s*\"\")\\s*(\")", "\\1 * \\2", result, perl = T)
    }
  )

  return(result)
}

#' @rdname scibeautify
#' @inheritDotParams scibeautify
#' @return A character vector with numbers formatted for knitting output
#' @export
scibeautify.knit <- function(.data, output_format = NULL, ...) {
    output_format <- {
      if (is.null(output_format)) {
        if (knitr::is_latex_output()) "latex"
        else "html"
      }
      else output_format
    }
    thiscall <- match.call()
    thiscall[[1]] <- as.name("scibeautify")
    thiscall[["output_format"]] <- output_format
    eval.parent(thiscall)
  }
