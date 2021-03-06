% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_scibeautify.R
\name{scibeautify}
\alias{scibeautify}
\alias{scibeautify.knit}
\title{Beautify a number in scientific format}
\usage{
scibeautify(
  .data,
  sig_digits = 3,
  output_format = c("unicode", "plain", "ascii", "html", "latex", "plotmath",
    "markdown"),
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
  .rows
)

scibeautify.knit(.data, output_format = NULL, ...)
}
\arguments{
\item{.data}{Numeric vector or dataframe. Non-numeric vectors will be coerced to numeric only if numeric values are preserved.}

\item{sig_digits}{Number of significant digits. If NA or NULL, the input number is not changed.}

\item{output_format}{(Optional) Output target. Can be one of "plain", "ascii" (same as plain), "unicode", "html", "latex", "plotmath", and "markdown". Default = "unicode".}

\item{sci_mode}{(Optional) Can be one of "auto" (automatically choose whether scientific notation is used based on string length optimization), "on", or "off" (scientific notation always on or off).}

\item{sci_format}{(Optional) Formatting of scientific notation. Can be one of "x" (times character), "." (center dot), "e" (lowercase e), "E" (uppercase E). Default = "x".}

\item{justify_mode}{Mode to use for justifying a numbers. Can be "l" (left), "c" (center), "r" (right), or "d" (decimal). An "x" flag can be appended which will independently align the exponent portion of numbers in scientific notation, in addition to aligning the coefficient. Allowed values = \code{c("l", "c", "r", "d", "lx", "cx", "rx", "dx")}.}

\item{pad_zeros}{Whether to add extra trailing zeros and/or a decimal to force number of significant digits (even if the original number did not have that many digits). Default = FALSE.}

\item{decimal_mark}{Character to use as the decimal marker.}

\item{big_mark}{If not blank, character to use in grouping every \code{big_interval} digits before the decimal marker.}

\item{big_interval}{Number of digits between each \code{big_mark}, if used.}

\item{auto_ignore_int}{(Optional) If the data (or column) is all integers, will not truncate non-significant digits and will not use scientific notation}

\item{nsmall}{Forces output to have nsmall digits after the decimal. If there are fewer significant digits than total digits after including nsmall, the number of significant digits is increased.}

\item{.cols}{Tidy selector for columns when \code{.data} is a dataframe. Default = \code{tidyr::everything()}.}

\item{.rows}{Vector of row numbers indicating where scibeautify should be applied.}

\item{...}{
  Arguments passed on to \code{\link[=scibeautify]{scibeautify}}
  \describe{
    \item{\code{}}{}
  }}
}
\value{

}
\description{
Takes a number (or vector of numbers), and formats it in scientific notation as a string (or vector of strings) for use in printing. This function will format the number according to the specified number of significant digits, and using the specified format for scientific notation (e.g. x 10^n, eN, EN, etc). It will also correctly produce latex and html compatible strings.
}
\details{
\code{scibeautify.knit} will choose the correct output format (latex or html) based on the knit options at runtime. By default, the function will check whether the knit output is html or graphic (LaTeX), and use the appropriate string codes.
}
