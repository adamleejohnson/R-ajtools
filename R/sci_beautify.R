#' Convert a number to a string in significant-figure form
#'
#' @param n Number or numeric vector to convert
#' @param sig_digits (Optional) Number of significant digits (default = 3)
#' @param padzeros (Optional) Whether to add trailing zeros (default = TRUE)
#' @param justify (Optional) Whether to left-justify string output across a character vector (default = FALSE)
#'
#' @export
format_signif <- function(n, sig_digits = 3, padzeros = T, justify = F) {

  stopifnot(exprs = {
    is.numeric(n)
    sig_digits == round(sig_digits)
  })

  signs <- sign(n)
  sig_stem <- format(signif(abs(n), sig_digits), scientific = F, drop0trailing = T)
  if (!justify) sig_stem <- trimws(sig_stem)

  result <- {
    if (padzeros) {
    has_decimal <- sig_stem != gsub("\\.", "", sig_stem)
    char_len <- nchar(gsub("^0*", "", gsub("\\.", "", trimws(sig_stem))))
    padding <- sapply(sig_digits - char_len, function(s) paste(rep("0", max(0,s)), collapse = ""))
    padding <- paste0(ifelse(has_decimal | sig_digits < char_len | !endsWith(sig_stem, "0"), "", "."), padding)
    paste0(sig_stem, padding)
    }
    else sig_stem
  }

  ifelse(signs == -1,
    paste0("-", result),
  ifelse(signs == 0,
    "0",
  result))
}



#' Beautify a number in scientific format
#'
#' @inheritParams format_signif
#' @param sci.format (Optional) Formatting of scientific notation. Can be one of "x" (multiplication character), "e" (lowercase e), "E" (uppercase E), "dot" (center dot). Default = "x".
#' @param sci.mode (Optional) Can be one of "auto" (automatically choose scientific notation or not), "on", or "off" (scientific notation always on or off).
#' @param output.format (Optional) Output target. Can be one of "plain", "html", or "latex". Default = "plain".
#'
#' @return
#' @export
sci_beautify <- function(n, sig_digits = 3, sci.format = "x", sci.mode = "auto", output.format = "plain", justify = F, padzeros = T) {

  # validate options
  stopifnot(exprs = {
    sci.format %in% c("x", "dot", "e", "E")
    output.format %in% c("plain", "html", "latex")
    sci.mode %in% c("on", "off", "auto")
  })

  # boolean vector for choosing sci notation or not
  use_sci <- {
    if (sci.mode == "on") n == n
    else {
      no_sci <- format_signif(n, sig_digits, padzeros, justify)
      if (sci.mode == "off") n != n
      else {
        #auto select
        nchar(gsub("^-", "",no_sci)) > 6
      }
    }
  }

  result <- ifelse(!use_sci, no_sci, {

    base <- 10
    exp <- floor(log(abs(n))/log(base))
    coeff <- ifelse(n == 0, 0, n * base^(-exp))

    coeff_print <- format_signif(coeff, sig_digits, padzeros, justify)

    exp_print <- {
      ifelse(n == 0, "", {
        if (output.format == "latex") {
          if (sci.format == "x") paste0("\\times",base,"^{",exp,"}")
          else if (sci.format == "dot") paste0("\\cdot",base,"^{",exp,"}")
          else if (sci.format == "e") paste0("\\mathrm{e}{",exp,"}")
          else if (sci.format == "E") paste0("\\mathrm{E}{",exp,"}")
        }
        else if (output.format == "html") {
          if (sci.format == "x") paste0("&times;",base,"<sup>",exp,"</sup>")
          else if (sci.format == "dot") paste0("&middot;",base,"<sup>",exp,"</sup>")
          else if (sci.format == "e") paste0("e",exp)
          else if (sci.format == "E") paste0("E",exp)
        }
        else if (output.format == "plain") {
          if (sci.format == "x") paste0(" x ",base,"^",exp)
          else if (sci.format == "dot") paste0(" . ",base,"^",exp)
          else if (sci.format == "e") paste0("e",exp)
          else if (sci.format == "E") paste0("E",exp)
        }
      })
    }

    paste0(coeff_print, exp_print)
  })

  if (output.format == "html") result <- gsub("-", "&minus;", result)
  if (output.format == "latex") result <- paste0("$", result, "$")

  if (justify) result <- format(result, scientific = F, justify = "left")

  return(result)
}



#' Scientific notation format for knitr tables
#'
#' Takes as input a number (or vector of numbers), and outputs a character (or vector of characters) for use in a knitr table. This function will format the number according to the number of significant digits, and using the appropriate characters for scientific notation. By default, the function will check whether the knit output is html or graphic (LaTeX), and use the apporpriate string codes.
#'
#' @inheritParams sci_beautify
#' @return
#' @export
sci_notation_knit <- function(n, sig_digits = 3, output.format = NULL, sci.format = "x", sci.mode = "auto", justify = F, padzeros = T) {
  target <- output.format
  if (is.null(target)) {
    if(knitr::is_latex_output()) target <- "latex"
    else target <- "html"
  }
  sci_beautify(n, sig_digits, sci.format, sci.mode, target, justify, padzeros)
}
