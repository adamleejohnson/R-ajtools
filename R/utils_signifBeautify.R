#' Convert a number to a string in significant-figure form
#'
#' Standard significant figure functions (such as `signif`, `format`) either do not output a string format (thereby excluding necessary trailing decimals & zeros), or do not correctly pad the number with the correct number of trailing zeros. This function aims to address these issues.
#'
#' @inheritParams justify
#' @param n Number or numeric vector to convert
#' @param sig_digits Number of significant digits. If NA or NULL, the input number is not changed.
#' @param pad_zeros Whether to add extra trailing zeros and/or a decimal to force number of significant digits (even if the original number did not have that many digits). Default = FALSE.
#' @param decimal_mark Character to use as the decimal marker.
#' @param big_mark If not blank, character to use in grouping every `big_interval` digits before the decimal marker.
#' @param big_interval Number of digits between each `big_mark`, if used.
#' @param nsmall Forces output to have nsmall digits after the decimal. If there are fewer significant digits than total digits after including nsmall, the number of significant digits is increased.
#'
#' @export
signif_beautify <-
  function(n,
           sig_digits,
           justify_mode,
           decimal_mark = ".",
           big_mark = ",",
           big_interval = 3,
           pad_zeros = F,
           justify_char = " ",
           nsmall = NULL) {

  if (missing(sig_digits) || is.null(sig_digits) || is.na(sig_digits)) {
    return(trimws(format(n,
      scientific = F,
      drop0trailing = F,
      big.mark = big_mark,
      big.interval = big_interval,
      decimal.mark = decimal_mark
    )))
  }

  if (all(is.na(n))) return(n)

  stopifnot(exprs = {
    is.numeric(n)
    is.numeric(sig_digits)
    sig_digits == round(sig_digits) # must be an integer
    big_interval == round(big_interval) # must be an integer
    length(sig_digits) == 1
    length(decimal_mark) == 1
    length(big_mark) == 1
    length(big_interval) == 1
    nchar(decimal_mark) == 1
    nchar(big_mark) == 1
  })

  signs <- sign(n)
  if (!is.null(nsmall)) {
    stopifnot(exprs = {
      is.numeric(nsmall)
      length(nsmall) == 1
      nsmall >= 0
    })
    nbig <- ifelse(floor(abs(n)) == 0, 0, nchar(floor(abs(n))))
    sig_digits <- nbig + nsmall
  }

  sig_stem <- if (length(n) == length(sig_digits)) {
    sapply(seq_along(n), function(i) {
      trimws(format(
        signif(abs(n[i]), sig_digits[i]),
        scientific = F,
        drop0trailing = T,
        big.mark = big_mark,
        big.interval = big_interval,
        decimal.mark = decimal_mark
      ))
    })
  } else {
    trimws(format(
      signif(abs(n), sig_digits),
      scientific = F,
      drop0trailing = T,
      big.mark = big_mark,
      big.interval = big_interval,
      decimal.mark = decimal_mark
    ))
  }

  add_padding <- pad_zeros | (sig_stem != abs(n))
  result <- ifelse(add_padding, {
    decimal_regex <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", decimal_mark)
    raw_stem <- trimws(sig_stem)
    raw_stem <- gsub("[^\\d]", "", raw_stem, perl = T)
    raw_stem <- gsub("^0*", "", raw_stem, perl = T)
    padding <- {
      char_len <- nchar(raw_stem)
      zeros <- sapply(sig_digits - char_len, function(s) paste(rep("0", max(0,s, na.rm = T)), collapse = ""))
      has_decimal <- grepl(decimal_regex, sig_stem)
      add_decimal <- !has_decimal & (char_len < sig_digits | (char_len == sig_digits & endsWith(sig_stem, "0")))
      paste0(ifelse(add_decimal, decimal_mark, ""), zeros)
    }
    ifelse(is.finite(n), paste0(sig_stem,padding), sig_stem)
  }, sig_stem)

  # add back negative signs, or set to zero
  result <- ifelse(signs == -1, paste0("-", result),
              ifelse(signs == 0, "0",
                result))

  # justify
  if (!missing(justify_mode)) result <- justify(result, justify_mode, decimal_mark, justify_char)

  return(result)
}
