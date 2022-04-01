#' @title Invert a hex color
#' @param hexColor Hex color
#' @return Hex color string, suitable for use in R graphics
#' @export
invertHexColor <- function(hexColor) {
  hexColor %>%
    as.character(.) %>%
    sub("^#?(.*)", "\\1", .) %>%
    strtoi(., base = 16) %>%
    {
      ifelse(
        is.na(.),
        NA,
        {
          bitwXor(., 0xFFFFFF) %>%
            as.hexmode(.) %>%
            format(., upper.case = T) %>%
            paste0("#", .)
        }
      )
    }
}

#' @title Color ramp shortcuts
#' @description Quick shortcuts for RColorBrewer color ramp palettes that map `[0,1]` to colors
#' @param palette An RColorBrewer palette.
#' @param alpha Transparency. Default 0.4.
#' @param x Argument to the returned object, which is a function that maps (0,1) -> hex color code.
#' @param convert_alpha_to_white If true, will use the alpha value to internally generate RBG values (instead of RBGA) assuming a white background
#' @export
col_ramp_brewer <- function(palette = rownames(RColorBrewer::brewer.pal.info),
                            alpha = 0.4,
                            convert_alpha_to_white = TRUE) {
  palette <- match.arg(palette)
  pal <- RColorBrewer::brewer.pal(3, palette)
  ramp <- grDevices::colorRamp(pal, space = "Lab")
  function(x) {
    x <- pmin(1, pmax(0, x))
    sapply(unlist(x), function(x) {
      if (is.na(x)) return(NA)
      val <- ramp(x) / 255
      if (convert_alpha_to_white) {
        val <- (1 - alpha) + alpha * val
        rgb(val[1], val[2], val[3])
      }
      else rgb(val[1], val[2], val[3], alpha)
    })
  }
}

#' @rdname col_ramp_brewer
#' @export
col_rmp_red <- col_ramp_brewer("Reds", 0.4)

#' @rdname col_ramp_brewer
#' @export
col_rmp_blue <- col_ramp_brewer("Blues", 0.4)


#' Flatten alpha transparency onto a background color
#' @param color Color value. String or Hex.
#' @param alpha Alpha. Value from 0 to 1.
#' @param bg Background color to flatten transparency onto. Default = "white".
#' @export
col_flatten_alpha <- function(color, alpha = 1, bg = "white") {
  rgb_col <- col2rgb(color, alpha = FALSE)
  rgb_bg <- col2rgb(bg, alpha = FALSE)
  new_col <- (1 - alpha) * rgb_bg + alpha * rgb_col
  rgb(new_col[1], new_col[2], new_col[3], maxColorValue = 255)
}
