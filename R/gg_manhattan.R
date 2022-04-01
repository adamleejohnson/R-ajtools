#' @title Manhattan plot with ggplot2
#' @description ...
#'
#' @param data Input data frame
#' @param chr Name of column with chromosome number.
#' @param pos Name of column with chromosome position (typically in base pair)
#' @param pval Name of column with p-value
#' @param label Optional, name of column with rsid symbols, gene symbols, or other labels
#' @param hline.signif Horizontal significance line. Can be a number, or "bonferroni" to automatically calculate Bonferroni correction. Use NA to omit.
#' @param hline.suggest Horizontal suggestive line. Can be a number, or "bonferroni" to automatically calculate Bonferroni correction. Use NA to omit.
#' @param title Plot title
#' @param label.mode How to apply labels to selected points. One of `c("none", "fdr_0.05", "hline.signif", "hline.suggest")`. `"fdr_0.05"` identifies points that have FDR < 0.05. `"hline.signif"` identifies points above the significance line.
#' @param chr.colors Vector of color names, applied to each chromosome.
#' @param point.size Size of point. Default 0.8.
#' @param overplot.limit If there are greater than 1 million points, only a fraction (specified by `overplot.frac`) of the points with p-values above this limit will be randomly removed.
#' @param overplot.frac Fraction of points above the overplot.limit to plot. Other points (randomly sampled) will be discarded.
#' @param chr.labels Optional. Chromosome labels.
#' @param limits_y Optional. y-axis limits, in -log10 units.
#' @param breaks_y Optional. y-axis breaks, in -log10 units.
#' @param progress Whether to show a progress bar during ggplot construction
#' @param highlight_loci.thresh Threshold p-value to use to identify lead snps in loci that will be highlighted with highlight_loci.color. Use NA to omit.
#' @param highlight_loci.width Width, in kilobases, of loci identified by highlight_loci.thresh to include in the highlight regions
#' @param highlight_loci.color Highlight color
#' @param label.size Font size of labels relative to the ggplot theme text element
#'
#' @import dplyr
#' @importFrom magrittr "%<>%"
#' @export
gg_manhattan <- function(data,
                         chr,
                         pos,
                         pval,
                         label,
                         title = NULL,
                         label.mode = c("none", "fdr05", "signif", "suggest"),
                         label.size = 0.7,
                         point.size = 0.8,
                         hline.signif = NA,
                         hline.suggest = NA,
                         highlight_loci.thresh = hline.signif,
                         highlight_loci.width = 250,
                         highlight_loci.color = "red",
                         overplot.limit = 0.05,
                         overplot.frac = 0.05,
                         chr.colors = manhattan_colors_GnBu(),
                         chr.labels = waiver(),
                         limits_y = waiver(),
                         breaks_y = waiver(),
                         progress = TRUE
                         ) {

  label.mode <- match.arg(label.mode)

  # constants
  BIN_NUM <- 100000 # number of bins for X position
  GAP_PROPORTION <- 8/dev.size("px")[1] # gap between chromosomes is this fraction of overall width
  CHR_MIN_PROPORTION <- 18/dev.size("px")[1] # width of chromosome is at minimum this fraction of overall width

  # verbose progress output
  progBar <- (function() {
    max <- 1
    pb <- if (progress %||% F) utils::txtProgressBar(style = 3, width = 50, max = max)
    nchar_prev <- 0
    function(label, i) {
      if (progress %||% F) {
        if (is.na(progress) || is.na(i)) {
          close(pb)
          cat("\n\n")
        }
        else {
          x <- capture.output(utils::setTxtProgressBar(pb, min(max, i)))
          if (!identical(x, character(0))) {
            spacer <- paste0(rep_len(" ", nchar_prev), collapse = "")
            x <- gsub("(^.+)(\r)", paste0("\\1",spacer,"\\2"), x)
            label <- paste0("   ", label)
            nchar_prev <<- nchar(label)
            x <- paste0(x, label)
            cat(x)
            # Sys.sleep(0.1)
          }
        }
      }
      return(NULL)
    }
  })()

  # extract columns
  progBar("Loading columns", 0)
  data %<>% select(pval = {{ pval }}, chr = {{ chr }}, pos = {{ pos }}, label = {{ label }})

  # TO DO:
  # add checks for column presence
  progBar("Validating data", 0.1)

  # convert to ordered factors & sort
  progBar("Converting chromosomes to factors", 0.2)
  chr_lvls <- paste0(
    if(startsWith(as.character(data$chr[1]), "chr")) "chr" else "",
    c(1:23, "X", "Y", "XY")
  )
  data %<>%
    mutate(
      chr = ordered(chr, levels = chr_lvls, labels = c(1:23, "X", "Y", "XY"))
    ) %>%
    filter( !is.na(chr) & !is.na(pos) & !is.na(pval) )

  # determine significance lines
  if (identical(hline.signif, "bonferroni")) hline.signif <- 0.05 / nrow(data)
  else if (!is.na(hline.signif)) {
    stopifnot(
      length(hline.signif) == 1,
      is.numeric(hline.signif),
      hline.signif > 0 && hline.signif < 1
    )
  }
  if (!is.na(hline.suggest)) {
    stopifnot(
      length(hline.suggest) == 1,
      is.numeric(hline.suggest),
      hline.suggest > 0 && hline.suggest < 1
    )
  }

  # get endpoints & gaps
  progBar("Calculating chromosome endpoints", 0.3)
  unique_chr <- unique(data$chr)
  nchr <- length(unique_chr)
  endpoints <-
    data %>%
    group_by(chr) %>%
    summarize(min = min(pos), max = max(pos)) %>%
    mutate(width = max - min)
  total_width <- sum(endpoints$width)
  min_width <- round(CHR_MIN_PROPORTION * total_width)
  min_gap <- round(GAP_PROPORTION * total_width)
  gap_pre <- sapply(seq_along(endpoints$chr), function(i) {
    if (endpoints$width[i] < min_width) round(0.5*(min_width - endpoints$width[i] + min_gap))
    else if (i == 1) 0
    else round(min_gap/2)
  })
  gap_post <- sapply(seq_along(endpoints$chr), function(i) {
    if (endpoints$width[i] < min_width) round(0.5*(min_width - endpoints$width[i] + min_gap))
    else if (i == nchr) 0
    else round(min_gap/2)
  })
  endpoints %<>%
    mutate(
      gap_pre = gap_pre,
      gap_post = gap_post,
      fullwidth = gap_pre + width + gap_post,
      cumlen = c(0, cumsum(fullwidth)[-nchr]),
      mid = cumlen + floor(fullwidth/2),
      adj = cumlen - min + gap_pre
    )
  endpoints <-
    tibble("chr" = ordered(chr_lvls, levels = chr_lvls, labels = c(1:23, "X", "Y", "XY"))) %>%
    left_join(endpoints, by = "chr")

  # identify loci
  progBar("Identifying loci", 0.4)
  data %<>% mutate(colors = as.character(chr))
  if (!is.na(highlight_loci.thresh)) {
    loci <- rep_len(F, nrow(data))
    which_leads <- data %>% mutate(pval < highlight_loci.thresh) %>% pull() %>% which()
    cnt <- 1
    for (idx in which_leads) {
      progBar("Identifying loci (" %++%
                as.character(cnt) %++% "/" %++%
                as.character(length(which_leads)) %++% ")",
              0.4 + 0.1*0.9*cnt/length(which_leads))
      lead <- data[idx,]
      loci <- loci | (data %>%
        mutate(
          chr == lead$chr &
          abs(pos - lead$pos) < highlight_loci.width * 1000
        ) %>%
        pull())
      cnt <- cnt + 1
    }
    data[loci, "colors"] <- "highlight"
    data <- rbind(data[!loci,], data[loci,])
    remove(loci)
  }
  data %<>% mutate(colors = factor(colors, c(levels(chr), "highlight")))

  # prevent overplotting
  if (nrow(data) > 1e6 && overplot.frac < 1) {
    progBar("Removing overplotted points", 0.5)
    overplt_keep <- (data$pval < overplot.limit | data$colors == "highlight")
    if (overplot.frac > 0) {
      overplt_p <- data %>% pull(pval) %>% `[`(., !overplt_keep)
      overplt_pmax <- max(overplt_p)
      overplt_pmin <- min(overplt_p)
      overplt_p <- (overplt_p - overplt_pmin) / (overplt_pmax - overplt_pmin)
      p1_calc <- (1/2) * (3 - sqrt(9 - 8 * overplot.frac))
      overplt_p_1 <- overplt_p < p1_calc
      overplt_keep_prob <- overplt_p
      overplt_keep_prob[which(overplt_p_1)] <- 1 - ((1 - p1_calc) / p1_calc) * overplt_p[which(overplt_p_1)]
      overplt_keep_prob[which(!overplt_p_1)] <- p1_calc
      progBar("Removing overplotted points", 0.5)
      overplt_keep_bool <- rep_along(overplt_keep_prob, T)
      overplt_keep_bool[which(overplt_p_1)] <-
        sapply(overplt_keep_prob[which(overplt_p_1)], function(x) {
          sample(c(T,F), 1, prob = c(x, 1 - x))
        })
      overplt_keep_bool[which(!overplt_p_1)] <- sample(c(T,F), size = sum(!overplt_p_1), prob = c(p1_calc, 1 - p1_calc), replace = T)
      progBar("Removing overplotted points", 0.58)
      overplt_keep[which(!overplt_keep)] <- overplt_keep_bool
      warning("Removed ", format(sum(!overplt_keep_bool), big.mark = ","), "/", format(length(overplt_keep_bool), big.mark = ","),
              " overplotted points with p > ", format(overplot.limit),
              " (kept ", format(100*mean(overplt_keep_bool), digits = 2), "% of overplotted points)",
              call. = F, immediate. = F)
      remove(overplt_keep_bool, overplt_keep_prob, overplt_p_1, overplt_p)
    }
    data %<>% filter(overplt_keep)
  }

  # convert pos to xcoor
  progBar("Calculating positions of all points", 0.6)
  adj_vals <- endpoints$adj
  data %<>% mutate(pos = pos + adj_vals[chr])

  # bin xpos
  max_pos <- max(data$pos)
  data %<>% mutate(pos = as.integer(floor((pos/max_pos)*BIN_NUM)))
  midpoints <- floor((endpoints$mid/max_pos)*BIN_NUM)
  midpoints <- midpoints[!is.na(midpoints)]

  # convert yvals
  progBar("Converting to -log10", 0.7)
  data %<>% mutate(pval_log = -log10(pval))
  signif_line_log <- -log10(hline.signif)
  suggest_line_log <- -log10(hline.suggest)
  hlines_log <- c(suggest_line_log, signif_line_log)[!is.na(c(suggest_line_log, signif_line_log))]
  if (length(hlines_log) == 0) hlines_log <-  NA

  # determine labels
  progBar("Generating labels", 0.8)
  switch(label.mode,
    "signif" = data %<>% mutate(do_label = pval < hline.signif),
    "suggest" = data %<>% mutate(do_label = pval < hline.suggest),
    "fdr05" = data %<>% mutate(do_label = p.adjust(pval, method = "BH") < 0.05),
    "none" = data %<>% mutate(do_label = F)
  )
  # create label dataframe
  label_pmin <- data %>% filter(do_label) %>% pull(pval_log)
  label_pmin <- if (length(label_pmin)) min(label_pmin) else NA
  label_data <- data %>%
    filter(do_label | pval_log > 0.9 * label_pmin) %>%
    mutate(label = if_else(do_label, label, "")) %>%
    select(-c(do_label, pval))
  data %<>% select(-c(label, do_label, pval))

  # syntax simplification
  if (is_waive(limits_y)) limits_y <- NULL
  if (is_waive(breaks_y)) breaks_y <- NULL
  if (is_waive(chr.labels)) chr.labels <- NULL

  # get theme's text properties
  theme_text <- ggplot2::calc_element("text", ggplot2::theme_get())

  progBar("Compiling ggplot", 0.9)
  g <-
    ggplot2::ggplot(data) +
    ggplot2::aes(x = pos, y = pval_log, color = colors) +
    ggplot2::scale_x_continuous(
      breaks = midpoints,
      labels = chr.labels %||% unique_chr,
      limits = c(0, BIN_NUM),
      expand = ggplot2::expansion(mult = (4/5)*GAP_PROPORTION),
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.01, 0.05)),
      limits = limits_y %||% function(lim) {
        if (is.na(hline.signif)) return(lim)
        c(lim[1], max(floor(1.5*hline.signif), lim[2]))
      },
      breaks = breaks_y %||% function(lim) round(lim[1]):round(lim[2])
    ) +
    ggplot2::scale_color_manual(values = c(rep_len(chr.colors, length(unique_chr)), highlight_loci.color)) +
    ggplot2::labs(
      title = title,
      x = "Chromosome",
      y = bquote(-log[10]~italic(p))
    ) +
    (if (any(!is.na(hlines_log)))
      ggplot2::geom_hline(
        yintercept = hlines_log,
        linetype = "dashed",
        alpha = 0.5,
        size = 0.25
    )) +
    (if (!identical(label.mode, "none") && nrow(label_data) > 0)
      ggrepel::geom_text_repel(
        data = label_data,
        mapping = ggplot2::aes(label = label),
        force = 1,
        force_pull = 0,
        nudge_x = round(12 * BIN_NUM / dev.size("px")[1]),
        min.segment.length = unit(0.125, "lines"),
        box.padding = unit(0.025, "lines"),
        point.size = point.size,
        size = label.size * 0.35 * theme_text$size,
        lineheight = theme_text$lineheight,
        fontface = theme_text$face,
        family = theme_text$family,
        colour = theme_text$colour,
        alpha = 0.75,
        segment.color = theme_text$colour,
        segment.size = unit(0.2, "mm"),
        segment.alpha = 0.75,
        hjust = 0,
        vjust = 0.5,
        ylim = c(1.01*max(hlines_log), NA)
    )) +
    ggplot2::geom_point(size = point.size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.position = "none",
      panel.grid.major.x = ggplot2::element_blank()
    ) +
    gg_themelock() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text = ggplot2::element_text(size = ggplot2::rel(0.8))
    )

  progBar("Done", 1)
  progBar(NA, NA)

  # add ggGeomTextModify class to enable theming of text elements when using as part of a patchwork
  g <- as.ggGeomTextModify(g)

  return(g)
}

#' @title Color ramp for Manhattan plots
#' @export
manhattan_colors_GnBu <- function() {
  col_ramp_lims <- RColorBrewer::brewer.pal(n = 9, name = "GnBu")[c(5,6,7)]
  col_ramp <- grDevices::colorRampPalette(col_ramp_lims, space = "Lab")
  grey_ramp <- grDevices::colorRampPalette(c("grey75", "grey75"), space = "Lab")
  rampalt <- c(rbind(col_ramp(19)[3:15], grey_ramp(13)))
  # scales::show_col(rampalt)
  rampalt
}

#' @rdname manhattan_colors_GnBu
#' @export
manhattan_colors_Crimson <- function() {
  col_ramp <- grDevices::colorRampPalette(c("#a3504d", "#ba6854"), space = "Lab")
  grey_ramp <- grDevices::colorRampPalette(c("grey75", "grey75"), space = "Lab")
  rampalt <- c(rbind(col_ramp(13), grey_ramp(13)))
  # scales::show_col(rampalt)
  rampalt
}
