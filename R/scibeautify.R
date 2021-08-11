#' Convert a number to a string in significant-figure form
#'
#' Standard significant figure functions (such as `signif`, `format`) either do not output a string format (thereby excluding necessary trailing decimals & zeros), or do not correctly pad the number with the correct number of trailing zeros. This function aims to address these issues.
#'
#' @param n Number or numeric vector to convert
#' @param sig_digits Number of significant digits. If NA or NULL, the input number is not changed.
#' @param pad_zeros Whether to add the trailing zeros and decimal, if needed.
#' @param decimal_mark Character to use as the decimal marker.
#' @param big_mark If not blank, character to use in grouping every 3 digits before the decimal marker.
#' @inheritParams justify
#'
#' @export
signif_beautify <-
  function(n,
           sig_digits = 3,
           justify_mode = NA,
           decimal_mark = ".",
           big_mark = ",",
           big_interval = 3,
           pad_zeros = T,
           justify_char = " ") {


  if (!is.numeric(sig_digits)) return(n)

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
  sig_stem <- trimws(format(
    signif(abs(n), sig_digits),
    scientific = F,
    drop0trailing = T,
    big.mark = big_mark,
    big.interval = big_interval,
    decimal.mark = decimal_mark
  ))

  result <- {
    if (!pad_zeros) sig_stem
    else {
      # escape the decimal character for regex
      decimal_regex <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", decimal_mark)
      padding <- {
        char_len <- nchar(gsub("^0*", "", gsub(decimal_regex, "", trimws(sig_stem))))
        zeros <- sapply(sig_digits - char_len, function(s) paste(rep("0", max(0,s)), collapse = ""))
        has_decimal <- sig_stem != gsub(decimal_regex, "", sig_stem)
        add_decimal <- !has_decimal & (char_len < sig_digits | (char_len == sig_digits & endsWith(sig_stem, "0")))
        paste0(ifelse(add_decimal, decimal_mark, ""), zeros)
      }
      ifelse(is.finite(n), paste0(sig_stem,padding), sig_stem)
    }
  }

  # add back negative signs, or set to zero
  result <- ifelse(signs == -1, paste0("-", result),
              ifelse(signs == 0, "0",
                result))

  # justify
  if (!is.na(justify_mode)) result <- justify(result, justify_mode, decimal_mark, padding_char)

  return(result)
}

#' Justify a character vector of numbers
#'
#' @param input_vec Input vector. Coerced to character.
#' @param justify_mode Mode to use for justifying a vector of values. Can be "l" (left), "c" (center), "r" (right), or "d" (decimal).
#' @param decimal Character to justify around when justify_mode = "decimal"
#' @param justify_char Character (or string) used to pad strings in order to visually justify.
#'
#' @export
#'
justify <- function(input_vec, justify_mode = "d", decimal = ".", justify_char = " ") {

  stopifnot(exprs = {
    dim(as.data.frame(input_vec))[2] == 1
    length(justify_mode) == 1
    length(decimal) == 1
    length(justify_char) == 1
    nchar(decimal) == 1
    nchar(justify_char) > 0
    justify_mode %in% c("l", "c", "r", "d")
  })

  string <- as.character(input_vec)

  # does the original string have a decimal?
  has_decimal <- grepl(decimal, string, fixed = T)
  # split string by decimal
  splt <- strsplit(string, decimal, fixed = T)
  # coerce to 2 x n matrix
  splt <- sapply(splt, function(s) c(s[1], paste(s[-1], collapse = decimal)))
  # insert the decimal as a middle now, if it was originally present
  splt <- rbind(splt[1,], ifelse(has_decimal, decimal, ""), splt[2,])
  # convert NA to blanks
  splt[is.na(splt)] <- ""

  # padding function
  npad <- function(x) sapply(x, function(y) paste(rep(justify_char, y), collapse = ""))
  total_pad_n <- max(nchar(string)) - nchar(string)

  switch(justify_mode,
    "l" = {
      splt[3,] <- paste0(splt[3,], npad(total_pad_n))
    },
    "r" = {
      splt[1,] <- paste0(npad(total_pad_n), splt[1,])
    },
    "c" = {
      left_pad_center <- npad(floor(total_pad_n / 2))
      right_pad_center <- npad(ceiling(total_pad_n / 2))
      splt[1,] <- paste0(left_pad_center, splt[1,])
      splt[3,] <- paste0(splt[3,], right_pad_center)
    },
    "d" = {
      max_l <- max(nchar(splt[1,]))
      max_d <- max(nchar(splt[2,]))
      max_r <- max(nchar(splt[3,]))
      l_pad <- npad(max_l - nchar(splt[1,]))
      d_pad <- npad(max_d - nchar(splt[2,]))
      r_pad <- npad(max_r - nchar(splt[3,]))
      splt[1,] <- paste0(l_pad, splt[1,])
      splt[2,] <- paste0(splt[2,], d_pad)
      splt[3,] <- paste0(splt[3,], r_pad)
    }
  )

  # re-collapse string
  apply(splt, 2, function(s) paste(s, collapse = ""))
}

unicode_to_html_characters <- c(
  "&Aring;" = "\\\\AA",
  "&asymp;" = "\\\\approx",
  "&ne;" = "\\\\neq",
  "&plusmn;" = "\\\\pm",
  "&times;" = "\\\\times",
  "&middot;" = "\\\\cdot",
  "&divide;" = "\\\\div",
  "&le;" = "\\\\leq",
  "&lt;" = "\\<",
  "&gt;" = "\\>",
  "&plus;" = "\\+",
  "&minus;" = "\\-",
  "&ge;" = "\\\\geq",
  "&sup2;" = "\\^2",
  "&sup3;" = "\\^3",
  "&deg;" = "^\\\\circ",
  "&micro;" = "\\\\mu",
  "~" = "\\\\sim",
  "&Gamma;" = "\\\\Gamma",
  "&Delta;" = "\\\\Delta",
  "&Theta;" = "\\\\Theta",
  "&Lambda;" = "\\\\Lambda",
  "&Xi;" = "\\\\Xi",
  "&Pi;" = "\\\\Pi",
  "&Sigma;" = "\\\\Sigma",
  "&Upsilon;" = "\\\\Upsilon",
  "&Phi;" = "\\\\Phi",
  "&Psi;" = "\\\\Psi",
  "&Omega;" = "\\\\Omega",
  "&alpha;" = "\\\\alpha",
  "&beta;" = "\\\\beta",
  "&gamma;" = "\\\\gamma",
  "&delta;" = "\\\\delta",
  "&epsilon;" = "\\\\epsilon",
  "&zeta;" = "\\\\zeta",
  "&eta;" = "\\\\eta",
  "&theta;" = "\\\\theta",
  "&iota;" = "\\\\iota",
  "&kappa;" = "\\\\kappa",
  "&lambda;" = "\\\\lambda",
  "&mu;" = "\\\\mu",
  "&nu;" = "\\\\nu",
  "&xi;" = "\\\\xi",
  "&pi;" = "\\\\pi",
  "&rho;" = "\\\\rho",
  "&sigmaf;" = "\\\\varsigma",
  "&sigma;" = "\\\\sigma",
  "&tau;" = "\\\\tau",
  "&upsilon;" = "\\\\upsilon",
  "&phi;" = "\\\\phi",
  "&chi;" = "\\\\chi",
  "&psi;" = "\\\\psi",
  "&omega;" = "\\\\omega",
  "&infin;" = "\\\\infty",
  "%" = "\\\\%"
)

#' Beautify a number in scientific format
#'
#' Takes a number (or vector of numbers), and formats it in scientific notation as a string (or vector of strings) for use in printing. This function will format the number according to the specified number of significant digits, and using the specified format for scientific notation (e.g. x 10^n, eN, EN, etc). It will also correctly produce latex and html compatible strings.
#'
#' `scibeautify_knit` will choose the correct output format (latex or html) based on the knit options at runtime. By default, the function will check whether the knit output is html or graphic (LaTeX), and use the appropriate string codes.
#'
#' `justify_mode` defaults to "decimal" (see justify function), but is only appled to ascii, unicode, and html outputs. For html mode, the paddings are wrapping in a `<span>` element with hidden visibility. Set to NA to disable.
#'
#' #@inheritParams signif_beautify
#' @param sci_mode (Optional) Can be one of "auto" (automatically choose whether scientific notation is used based on string length optimization), "on", or "off" (scientific notation always on or off).
#' @param sci_format (Optional) Formatting of scientific notation. Can be one of "x" (times character), "." (center dot), "e" (lowercase e), "E" (uppercase E). Default = "x".
#' @param output_format (Optional) Output target. Can be one of "plain", "ascii" (same as plain), "unicode", "html", "latex". Default = "unicode".
#' @param justify_mode Mode to use for justifying a numbers. Can be "l" (left), "c" (center), "r" (right), or "d" (decimal). An "x" flag can be appended which will independently align the exponent portion of numbers in scientific notation, in addition to aligning the coefficient. Allowed values = `c("l", "c", "r", "d", "lx", "cx", "rx", "dx")`.
#'
#' @return
#' @export
scibeautify <-
  function(n,
           sig_digits = 3,
           sci_mode = "auto",
           sci_format = "x",
           output_format = "unicode",
           justify_mode = "dx",
           pad_zeros = T,
           decimal_mark = ".",
           big_mark = ",",
           big_interval = 3) {

  stopifnot(exprs = {
    sci_format %in% c("x", ".", "e", "E")
    output_format %in% c("plain", "ascii", "unicode", "html", "latex")
    sci_mode %in% c("on", "off", "auto")
    justify_mode %in% c("l", "c", "r", "d", "lx", "cx", "rx", "dx")
  })

  no_sci <- as.character(
    signif_beautify(n,
      sig_digits = sig_digits,
      justify_mode = NA,
      decimal_mark = decimal_mark,
      big_mark = big_mark,
      big_interval = big_interval
    )
  )

  base <- 10
  exp <- floor(log(abs(n))/log(base))
  coeff <- ifelse(n == 0, 0, n * base^(-exp))

  coeff_sci <- as.character(
    signif_beautify(coeff,
      sig_digits = sig_digits,
      justify_mode = NA,
      decimal_mark = decimal_mark,
      big_mark = big_mark,
      big_interval = big_interval
    )
  )

  # boolean vector for choosing sci notation or not
  use_sci <- {
    if (sci_mode == "on") n == n
    else {
      if (sci_mode == "off") n != n
      else {
        #auto select
        space_save <-  nchar(no_sci) - nchar(coeff_sci)
        (space_save > 2) & abs(exp) > 1
      }
    }
  }
  use_sci <- use_sci & n != 0 & is.finite(n)

  coeff_print <- ifelse(use_sci, coeff_sci, no_sci)
  exp_print <-
    ifelse(!use_sci, "",
      switch(output_format,
        "latex" = {
          switch(sci_format,
                 "x"     = paste0("\\times", base, "^{", exp, "}"),
                 "."   = paste0("\\cdot", base, "^{", exp, "}"),
                 "e"     = paste0("e", exp),
                 "E"     = paste0("\\scriptsize{\\mathrm{E}}\\normalsize{_}", exp)
          )
        },
        "plain" = ,
        "ascii" = {
          switch(sci_format,
                 "x"     = paste0(" x ",base,"^",exp),
                 "."   = paste0(" . ",base,"^",exp),
                 "e"     = paste0("e",exp),
                 "E"     = paste0("E",exp)
          )
        },
        "html" = {
          switch(sci_format,
                 "x"     = paste0("&thinsp;&times;&thinsp;",base,"<sup>",exp,"</sup>"),
                 "."   = paste0("&thinsp;&middot;&thinsp;",base,"<sup>",exp,"</sup>"),
                 "e"     = paste0("&thinsp;<i>e</i>&thinsp;",exp),
                 "E"     = paste0("&thinsp;&#7431;&thinsp;",exp)
          )
        },
        "unicode" = {
          exp_uni <- paste0("^",exp)
          # This is a nice idea in theory, but the font is too hard to read on R terminals
          # uni_sup <- c("-" = "\U207B", "0" = "\U2070", "1" = "\U00B9", "2" = "\U00B2",
          #              "3" = "\U00B3", "4" = "\U2074", "5" = "\U2075", "6" = "\U2076",
          #              "7" = "\U2077", "8" = "\U2078", "9" = "\U2079")
          # exp_uni <- sapply(strsplit(as.character(exp),""),
          #                  function(s) paste(uni_sup[s], collapse = ""))
          switch(sci_format,
                 "x" = paste0("\U2007\U00D7\U2007",base,exp_uni),
                 "." = paste0("\U2007\U00B7\U2007",base,exp_uni),
                 "e" = paste0("\U2007\U1D452\U2007",exp),
                 "E" = paste0("\U2007\U1D5A4\U2007",exp)
          )
        }
      )
    )

  # Justify
  if (!is.na(justify_mode)) {
    switch(output_format,
      "ascii" = ,
      "plain" = {
        coeff_print <- justify(coeff_print, justify_mode, decimal = decimal_mark, justify_char = " ")
        exp_print <- justify(exp_print, "left", decimal = decimal_mark, justify_char = " ")
      },
      "unicode" = {
        coeff_print <- justify(coeff_print, justify_mode, decimal = decimal_mark, justify_char = "\U2007")
        # gets trickier for the exponent
        fig_sp <- "\U2007"
        max_exp_nchar <- max(nchar(exp[use_sci]))
        npad_exp <- sapply(max_exp_nchar - ifelse(use_sci, nchar(exp), 0),
                           function(x) paste(rep(fig_sp, x), collapse = ""))
        npad_base <- paste(rep(fig_sp, nchar(base)), collapse = "")
        exp_padding <- ifelse(use_sci,
                              npad_exp,
                              switch(sci_format,
                                "x" = , "." = paste0(fig_sp, fig_sp, fig_sp, npad_base, fig_sp, npad_exp),
                                "e" = , "E" = paste0(fig_sp, fig_sp, fig_sp, npad_exp)
                              )
        )
        exp_print <- paste0(exp_print, exp_padding)
      },
      "html" = {
        fig_sp <- "&numsp;"
        # replace Inf text with I character for correct justification
        coeff_print <- gsub("Inf", "I", coeff_print)
        coeff_print <- justify(coeff_print, justify_mode, decimal_mark, fig_sp)
        # replace/create padding characters for html
        coeff_print <- gsub(paste0("((",fig_sp,")+)"), "<span style=\"visibility:hidden\">\\1</span>", coeff_print)
        coeff_print <- gsub("I", "Inf", coeff_print)
        # gets trickier for the exponent
        max_exp_nchar <- max(nchar(exp[use_sci]))
        npad_exp <- sapply(max_exp_nchar - ifelse(use_sci, nchar(exp), 0),
                           function(x) paste(rep(fig_sp, x), collapse = ""))
        npad_base <- paste(rep(fig_sp, nchar(base)), collapse = "")
        exp_padding <- {
          if (any(use_sci)) {
            ifelse(use_sci,
                   paste0("<sup>",npad_exp,"</sup>"),
                   switch(sci_format,
                          "x" = , "." = paste0("&thinsp;&numsp;&thinsp;",npad_base,"<sup>",npad_exp,"</sup>"),
                          "e" = , "E" = paste0("&thinsp;&nbsp;&thinsp;",npad_exp)))
          }
          else ""
        }
        exp_print <- paste0(exp_print, "<span style=\"visibility:hidden\">", exp_padding, "</span>")
      }
    )
  }

  # paste coefficient to exponent
  result <- paste0(coeff_print, exp_print)

  # some final cleanup
  switch(output_format,
    "unicode" = {
      result <- gsub("-", "\U2212", result)
      result <- gsub("Inf", "\U221E", result)
    },
    "html" =  {
      result <- gsub("-", "&minus;", result)
      result <- gsub("Inf", "&infin;", result)
    },
    "latex" = {
      result <- gsub("Inf", "\\infty", result)
      result <- paste0("$", result, "$")
    }
  )

  return(result)
}

#' @rdname scibeautify
#' @inheritParams scibeautify
#' @return
#' @export
scibeautify_knit <-
  function(n,
           sig_digits = 3,
           output_format = NULL,
           sci_mode = "auto",
           sci_format = "x",
           justify_mode = "dx",
           pad_zeros = T,
           decimal_mark = ".",
           big_mark = ",",
           big_interval = 3) {

  target <- {
    if (is.null(output_format)) {
      if (knitr::is_latex_output()) "latex"
      else "html"
    }
    else output_format
  }
  r <- scibeautify(n, sig_digits,
                   output_format = target, # set target
                   sci_mode = sci_mode,
                   sci_format = sci_format,
                   justify_mode = justify_mode,
                   pad_zeros = pad_zeros,
                   decimal_mark = decimal_mark,
                   big_mark = big_mark,
                   big_interval = big_interval)
  # if (!knitr::is_latex_output()) r <- tex_to_html(r) # superfluous
  return(r)
}
