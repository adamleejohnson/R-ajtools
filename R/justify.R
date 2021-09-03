#' Justify a character vector of numbers using a character to pad strings on the left and right sides
#'
#' @param input_vec Input vector. Coerced to character.
#' @param justify_mode Mode to use for justifying a vector of values. Can be "l" (left), "c" (center), "r" (right), or "d" (decimal).
#' @param decimal Character to justify around when justify_mode = "decimal"
#' @param justify_char Character (or string) used to pad strings.
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
      lpad_center <- npad(floor(total_pad_n / 2))
      rpad_center <- npad(ceiling(total_pad_n / 2))
      splt[1,] <- paste0(lpad_center, splt[1,])
      splt[3,] <- paste0(splt[3,], rpad_center)
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

setdiff.lazy <- function(x,y) {
  for (i in seq_along(y)) {
    if (y[i] %in% x) x <- x[ -match(y[i], x) ]
  }
  return(x)
}

#' justify.proportional is a variation on the justify function that, instead of using a single character (e.g. " ") to pad strings to achieve justification, will use characters from other strings in the vector. This is useful when the output font is not monospaced, and may not even use tabular numbers (i.e. the number widths may be proportional). Usually, the decimal character is not the same width as the numbers, and this can create problems when using extra characters to justify a column of numbers. The function returns a list of two vectors: a left-padding vector of string and right-padding vector of strings. Concatenate these with the original vector of strings to achieve the desired justification.
#'
#' @inheritParams justify
#' @param tabular_numbers When `TRUE`, we assume that all digits have the same width in the target font. We can therefore use a single character (specified by `tabular_char`) to pad all of our strings. Any non-digit characters (i.e. the decimal character) are still treated as if they have a unique widths that need to be accounted for.
#'
#' By specifying `tabular_numbers = FALSE`, we assume that each digit in the target font can have varying (proportional) widths. Therefore, in order to guarantee correct justification, the paddings must consist of a common set of characters. The `tabular_numbers = FALSE` mode finds a common set of characters across all strings in the vector, and then creates paddings from the `setdiff` of each string with the common set. Each string in the final compiled result (l_pad + string + r_pad) will therefore contain the same set of common characters, thereby ensuring justification.
#' @param tabular_char See `tabular_numbers`
#' @param monospace Override to assume all characters are manospaced.
#'
#' @export
justify.proportional <- function(input_vec, justify_mode = "d", decimal = ".", tabular_numbers = TRUE, tabular_char = "0", monospace = FALSE) {

  stopifnot(exprs = {
    dim(as.data.frame(input_vec))[2] == 1
    length(justify_mode) == 1
    length(decimal) == 1
    nchar(decimal) == 1
    length(tabular_char) == 1
    nchar(tabular_char) == 1
    justify_mode %in% c("l", "c", "r", "d")
  })

  input_vec <- as.character(input_vec)

  if (justify_mode != "d") {

    # split each string into a list of characters
    splt <- strsplit(input_vec, "", fixed = T)
    # if tabular_numbers or monospace, use the replacement character
    splt <- {
      if (monospace) lapply(splt, function(x) gsub(".", tabular_char, x))
      else if (tabular_numbers) lapply(splt, function(x) gsub("\\d", tabular_char, x))
      else splt
    }
    # find the common setdiff
    common <- Reduce(function(x, y) c(x, setdiff.lazy(y, x)), splt)
    # creating paddings consisting of setdiff with the common setdiff
    paddings <- sapply(splt, function(x) paste(setdiff.lazy(common, x), collapse = ""))
    # convert to l_pad and r_pads
    switch(justify_mode,
      "l" = {
        l_pad <- rep("", length(input_vec))
        r_pad <- paddings
      },
      "r" = {
        l_pad <- paddings
        r_pad <- rep("", length(input_vec))
      },
      "c" = {
        pad_sz <- nchar(paddings)
        l_pad <- substring(paddings, 1, ceiling(pad_sz / 2))
        r_pad <- substring(paddings, ceiling(pad_sz / 2) + 1, pad_sz)
      })
  }
  else {
    # special case; treat the pre-decimal and post-decimal sub-strings separately
    has_decimal <- grepl(decimal, input_vec, fixed = T)
    # split string by decimal
    splt <- strsplit(input_vec, decimal, fixed = T)
    pre_dec <- sapply(splt, function(x) x[1])
    post_dec <- sapply(splt, function(x) ifelse(is.na(x[2]), "", x[2]))
    dec_pad <- ifelse(has_decimal, "", decimal)

    pre_dec_pad <- justify.proportional(
      pre_dec,
      justify_mode = "r",
      decimal = decimal,
      tabular_numbers = tabular_numbers,
      tabular_char = tabular_char
    )
    post_dec_pad <- justify.proportional(
      post_dec,
      justify_mode = "l",
      decimal = decimal,
      tabular_numbers = tabular_numbers,
      tabular_char = tabular_char
    )

    l_pad <- pre_dec_pad[["l_pad"]]
    r_pad <- post_dec_pad[["r_pad"]]
    if (any(has_decimal)) r_pad <- paste0(dec_pad, r_pad)
  }

  return(list("l_pad" = l_pad, "r_pad" = r_pad))
}
