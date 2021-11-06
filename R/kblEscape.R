latex_to_html_characters <- c(
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
  "&#43;" = "\\+",
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

#' Convert latex string to html
#' @param input input string
#' @export
tex_to_html <- function(input) {
  sapply(input, simplify = T, USE.NAMES = F, function(v) {
    # convert symbols
    v <- Reduce(
      function(x,y) {
        from <- paste0("(\\$[^\\$]*)(",y,")([^\\$]*\\$)")
        to <- paste0("\\1",names(latex_to_html_characters)[which(latex_to_html_characters == y)][1],"\\3")
        gsub(from, to, x, perl = T)
      }, latex_to_html_characters, v)

    # repeat until string is unchanged:
    repeat {
      s <- v

      # remove whitespace around \n and replace with <br>
      s <- gsub("[ ]*\\n[ ]*", "<br>", s, perl = T)

      # \sqrt{...} -> &radic(...);
      s <- gsub("(\\$[^\\$]*)\\\\sqrt\\{([^\\$\\}]+)\\}([^\\$]*\\$)", "\\1&radic;<span style=\"text-decoration: overline\">\\2</span>\\3", s, perl = T)

      # ^{...} -> <sup>...</sup>
      s <- gsub("(\\$[^\\$]*)\\^\\{([^\\$\\}]+)\\}([^\\$]*\\$)", "\\1<sup>\\2</sup>\\3", s, perl = T)

      # _{...} -> <sub>...</sub>
      s <- gsub("(\\$[^\\$]*)_\\{([^\\$\\}]+)\\}([^\\$]*\\$)", "\\1<sub>\\2</sub>\\3", s, perl = T)

      # \frac{...}{...} -> <sup>...</sup>&frasl;<sub>...</sub>
      s <- gsub("(\\$[^\\$]*)\\\\frac\\{([^\\$\\}]*)\\}\\{([^\\$\\}]*)\\}([^\\$]*\\$)", "\\1<sup>\\2</sup>&frasl;<sub>\\3</sub>\\4", s, perl = T)

      # break condition
      if (s == v) break
      else v <- s
    }

    # do again for general functions
    repeat {
      s <- v

      # remove general latex functions and leave only arguments
      s <- gsub("(\\$[^\\$]*)\\\\[a-zA-Z]+\\{([^\\$\\}]+)\\}([^\\$]*\\$)", "\\1\\2\\3", s, perl = T)

      # break condition
      if (s == v) break
      else v <- s
    }

    # remove $ around latex expressions
    repeat {
      s <- v
      s <- gsub("\\$([^\\$]*)\\$", "\\1", s, perl = T)

      # break condition
      if (s == v) break
      else v <- s
    }

    return(v)
  })
}

#' Automatically escape latex code or convert to equivalent html depending on knit output
#'
#' @param df data frame
#' @param ... Parameters to pass to kableExtra::kbl
#' @inheritParams kableExtra::linebreak
#' @inheritParams kableExtra::kbl
#' @inheritDotParams kableExtra::kbl
#'
#' @importFrom dplyr mutate across "%>%"
#' @importFrom tidyselect everything
#'
#' @export
kbl.escape <- function(df, row.names = NA,
                       col.names = NA, align = "c", caption = NULL, label = NULL, format.args = list(),
                       escape = TRUE, table.attr = "", booktabs = FALSE, longtable = FALSE,
                       valign = "t", position = "", centering = TRUE, linebreaker = "\n", ...) {
  df %>%
    as.data.frame() %>% {
    if (knitr::is_latex_output()) {
      mutate(., across(everything(), ~kableExtra::linebreak(.x, align = align, linebreaker = linebreaker))) %>%
      `colnames<-`(kableExtra::linebreak(colnames(.), align = align, linebreaker = linebreaker)) %>%
      `rownames<-`(kableExtra::linebreak(rownames(.), align = align, linebreaker = linebreaker)) %>%
      kableExtra::kbl("latex",
          booktabs = T,
          escape = F,
          col.names = col.names,
          align = align,
          caption = caption,
          label = label,
          format.args = format.args,
          table.attr = table.attr,
          longtable = longtable,
          valign = valign,
          position = position,
          centering = centering,
          ...
        )
    }
    else {
      mutate(., across(everything(), tex_to_html)) %>%
      `colnames<-`(tex_to_html(colnames(.))) %>%
      `rownames<-`(tex_to_html(rownames(.))) %>%
      kableExtra::kbl("html",
          booktabs = T,
          escape = F,
          col.names = col.names,
          align = align,
          caption = caption,
          label = label,
          format.args = format.args,
          table.attr = table.attr,
          longtable = longtable,
          valign = valign,
          position = position,
          centering = centering,
          ...
        )
    }
  }
}
