#' @title Forest plot for generalized linear models
#' @description Generate a forest plot with a table, optimized for logistic regression.
#'
#' @param glm_list A named list of the results of calls to `lm()` or `glm()`. Names will be used as labels in the table.
#' @param stats.predictor_var The name of the variable in the linear model that is being regressed on (i.e., the predictor variable).
#' @param stats.conf_interval Confidence interval. Default = 95%.
#' @param stats.effect_sig_digits Number of significant figures to for effect size and confidence interval
#' @param stats.sig_pvalue (Not used) significant p-value after correction for multiple comparisons.
#' @param label.effect_size Label/column header describing the effect size.
#' @param label.dependent_var Label/column header for the dependent variables (i.e., predicted variables).
#' @param plot.rel_width Width of the forest plot relative to the width of the table. Default = 0.33.
#' @param plot.point.size Size of the boxes in the forest plot. Default = 1.8.
#' @param plot.point.color Color of the boxes in the forest plot. Default = "dodgerblue4".
#' @param plot.row.color Background color of alternating rows of the table. Default = "dodgerblue4" with 4% transparency. Use NA to omit background colors.
#' @param plot.breaks Breaks to use on the horizontal axis of the forest plot.
#' @param plot.vertical.pos Vertical position of the indicator line on the forest plot. Default = 1 for logistic regression, 0 otherwise. Use NA to omit.
#' @param plot.vertical.label Boolean, whether to add a label on the horizontal axis corresponding to the vertical indicator, if not already included in the breaks.
#'
#' @import ggplot2
#' @import patchwork
#' @import dplyr
#' @importFrom grDevices adjustcolor
#' @importFrom grid unit unit.c
#' @importFrom utils modifyList
#' @importFrom rlang rep_along
#' @export
gg_glmforest <- function(glm_list,
                         stats.predictor_var,
                         stats.conf_interval = 0.95,
                         stats.effect_sig_digits = 3,
                         stats.sig_pvalue = waiver(),
                         label.effect_size = paste("OR per unit", stats.predictor_var),
                         label.dependent_var = "Dependent Var.",
                         plot.rel_width = 0.33,
                         plot.point.size = 1.8,
                         plot.point.color = "dodgerblue4",
                         plot.row.color = col_flatten_alpha(plot.point.color, 0.06),
                         plot.breaks = waiver(),
                         plot.vertical.pos = waiver(),
                         plot.vertical.label = waiver()) {

  ## ------------------------------------ validate input ------------------------------------
  # make sure everything is a glm/lm result
  if (!identical("list", class(glm_list))) glm_list <- list(glm_list)
  if (!all(sapply(glm_list, function(x) "lm" %in% class(x)))) {
    stop("Input object must be a list of lm or glm objects, as returned by lm (glm)")
  }
  num_models <- length(glm_list)
  if (is_waive(stats.sig_pvalue)) stats.sig_pvalue <- 0.05 / num_models

  # make sure all the models are the same type
  all_lm <- all(sapply(glm_list, function(x) identical("lm", class(x))))
  all_glm <- all(sapply(glm_list, function(x) "glm" %in% class(x)))
  all_glm_same <- if (all_glm) {
    fams <- sapply(glm_list, function(x) x$family$family)
    links <- sapply(glm_list, function(x) x$family$link)
    (length(unique(fams)) == 1 && length(unique(links)) == 1)
  }
  if (!(all_lm || all_glm) || (all_glm && !all_glm_same)) {
    stop("All models in the list must be of the same type (all lm, or if glm, all the same family and link")
  }

  # make sure the effect var is a term in all models
  stats.predictor_var <- as.character(stats.predictor_var)
  predictor_var_present <- all(sapply(glm_list, function(x) stats.predictor_var %in% names(x$coefficients)))
  if (!predictor_var_present) {
    stop("All models must have the predictor var ('", stats.predictor_var, "') in the model")
  }

  ## ------------------------------------ get var names ------------------------------------
  effect_var_names <- names(glm_list)
  if (is.null(effect_var_names)) effect_var_names <- rep_along(glm_list, "")
  effect_var_symbols <- sapply(glm_list, function(x) as.character(x$terms[[2]]))

  # if name is blank, retrieve from glm object
  effect_var_names <- ifelse(effect_var_names == "", effect_var_symbols, effect_var_names)

  ## ------------------------------------ extract stats ------------------------------------
  family <- if (all_lm) "gaussian" else glm_list[[1]]$family$family
  link <- if (all_lm) "identity" else glm_list[[1]]$family$link
  use_logistic <- identical(family, "binomial") && identical(link, "logit")
  num_cases <- sapply(glm_list, function(x) {
    evar <- as.character(x$terms[[2]])
    sum(x$model[, evar], na.rm = TRUE)
  })
  num_total <- sapply(glm_list, function(x) length(x$residuals))
  summaries <- lapply(glm_list, summary)
  coeffs <- sapply(summaries, function(x) x$coefficients[stats.predictor_var, "Estimate"])
  pval <- sapply(summaries, function(x) x$coefficients[stats.predictor_var, grepl("^Pr", colnames(x$coefficients))])
  stderr <- sapply(summaries, function(x) x$coefficients[stats.predictor_var, "Std. Error"])

  ## ------------------------------------ calculate conf intervals ------------------------------------
  conf_radius <- stderr * stats::qnorm(1 - 0.5 * (1 - stats.conf_interval))
  conf_lower <- coeffs - conf_radius
  conf_upper <- coeffs + conf_radius

  ## ------------------------------------ calculate odds (if logistic) ------------------------------------
  if (use_logistic) {
    coeffs <- exp(coeffs)
    conf_lower <- exp(conf_lower)
    conf_upper <- exp(conf_upper)
  }

  if (is_waive(plot.vertical.pos)) plot.vertical.pos <- if (use_logistic) 1 else 0
  if (is_waive(plot.vertical.label)) plot.vertical.label <- !is.na(plot.vertical.pos)

  ## ------------------------------------ define plot table ------------------------------------
  df_plot <- tibble(
    effect_var_names,
    coeffs,
    conf_lower,
    conf_upper,
    num_cases,
    num_total,
    pval
  ) %>%
    mutate(Index = row_number())

  ## ------------------------------------ create gg forest plot ------------------------------------
  vert_line <- {
    if (!is.na(plot.vertical.pos))
      geom_vline(
        xintercept = plot.vertical.pos,
        linewidth = 0.3,
        color = "black",
        linetype = "dashed",
        alpha = 0.8
      )
  }
  forest_core <-
    ggplot(
      df_plot,
      aes(
        y = effect_var_names,
        x = coeffs,
        xmin = conf_lower,
        xmax = conf_upper
      )
    ) +
    vert_line +
    geom_errorbarh(height = 0, size = 0.35, color = "gray30") + # adds the CIs
    geom_point(shape = 15, size = plot.point.size, color = plot.point.color) + # this adds the effect sizes to the plot
    scale_y_discrete(
      name = NULL,
      labels = NULL,
      expand = expansion(mult = 0, add = unit(0.5, "null")),
      limits = rev(df_plot$effect_var_names)
    ) +
    # Build a scale_x_continous:
    #  - trans: transformation function
    #  - limits: limits aligned with extrema of break points
    #  - break: custom break points with scales::breaks_extended()
    scale_x_continuous(
      trans = if (use_logistic) scales::log10_trans() else scales::identity_trans(),
      breaks = if (is_waive(plot.breaks)) {
        function(limits) {
          bks <- scales::breaks_extended(w = c(10, 100, 50, 1))(limits)
          if ((plot.vertical.label %||% F) && !(plot.vertical.pos %in% bks)) bks <- c(bks, plot.vertical.pos)
          if (!(limits[1] %in% bks)) bks <- c(limits[1], bks)
          if (!(limits[2] %in% bks)) bks <- c(bks, limits[2])
          bks
        }
      } else {
        plot.breaks
      },
      limits = function(range) {
        bks <- scales::breaks_extended(only.loose = TRUE, w = c(10, 1, 1, 10))(range)

        if (use_logistic && min(bks) == 0 && length(bks) > 1) {
          min_val <- bks[2]  # pick the next lowest break
        } else {
          min_val <- min(bks)
        }

        c(min_val, max(bks))
      },
      expand = expansion(mult = 0, add = 0)
    ) +
    theme( # locked (unmodifiable) theme elements
      axis.title = element_blank(),
      axis.line.x = element_line(),
      axis.ticks.x = element_line(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_blank(),
      plot.background = element_blank(),
      plot.margin = margin(t = 0, r = 6, b = 0, l = 6),
    ) +
    gg_themelock() +
    theme( # modifiable theme elements
      line = element_line(linewidth = 0.3),
      axis.text = element_text(size = rel(1), color = NULL),
      axis.text.x = element_text(size = rel(0.85)),
      axis.ticks.length.x = unit(5, "points"),
    )

  # store text properties for later use in tableGrob
  forest_theme_text <- calc_element("text", modifyList(theme_get(), remove_null(forest_core$theme)))

  # fix x-axis tick and text colors... no idea why this is needed
  forest_core <- forest_core +
    theme(
      axis.text = element_text(color = forest_theme_text$colour),
      axis.ticks = element_line(color = forest_theme_text$colour)
    )

  ## ------------------------------------ create table for tablegrob ------------------------------------
  # create plotmath strings for p value sci notation, unicode strings otherwise
  df_plot <-
    df_plot %>%
    mutate(
      p_sci = scibeautify(pval, sci_format = "x", output_format = "plotmath", justify_mode = "none", sci_mode = "auto", sig_digits = 2),
      p_nosci = scibeautify(pval, sci_format = "x", output_format = "unicode", justify_mode = "none", sci_mode = "off", sig_digits = 2),
      p_hassci = grepl("%*%", p_sci, fixed = T)
    )

  df_table <-
    df_plot %>%
    transmute(
      var = effect_var_names,
      num_cases = paste0(scibeautify(num_cases), " ", "(", round(100 * num_cases / num_total, 1), ")"),
      plot_spacer = "",
      coeffs = scibeautify(coeffs, justify_mode = "d", sci_mode = "off", sig_digits = stats.effect_sig_digits),
      ci = paste0(
        "(",
        scibeautify(conf_lower, sci_mode = "off", sig_digits = stats.effect_sig_digits),
        "\U2013",
        scibeautify(conf_upper, sci_mode = "off", sig_digits = stats.effect_sig_digits),
        ")"
      ),
      p = ifelse(p_hassci, p_sci, p_nosci),
    )

  ## ------------------------------------ create tablegrob ------------------------------------
  tbl_hjust <- mutate(
    df_table,
    across(everything(), ~0.5),
    across(c(var, ci), ~0),
    across(coeffs, ~1),
  ) %>% as.matrix()

  tbl_parse <- mutate(
    df_table,
    across(-p, ~F),
    across(p, ~ df_plot$p_hassci),
  ) %>% as.matrix()

  tbl_x <- 0.5 + (tbl_hjust - 0.5) * 0.9 # move text position towards the middle by 90%
  tbl_y <- ifelse(tbl_parse, 0.565, 0.5) # parsed numbers with scinotation seem to be shifted a little

  tbl_grob <- gridExtra::tableGrob(
    df_table,
    rows = NULL,
    cols = NULL,
    theme = gridExtra::ttheme_minimal(
      base_size = forest_theme_text$size,
      base_colour = forest_theme_text$colour,
      base_family = forest_theme_text$family,
      core = list(
        fg_params = list(
          parse = tbl_parse,
          hjust = tbl_hjust,
          vjust = 0.5,
          x = tbl_x,
          y = tbl_y
        ),
        bg_params = list(fill = c(plot.row.color, NA), col = NA)
      )
    )
  )

  ## ------------------------------------ create header grob ------------------------------------
  # get the column numbers of the plot_spacer, coeffs, and CI columns.
  # these will be condensed into one column the in header column
  header_cols <- {
    blank <- as.numeric(tidyselect::eval_select(c("plot_spacer"), df_table))
    cols <- sort(tidyselect::eval_select(c("plot_spacer", "coeffs", "ci"), df_table))
    start <- if (min(cols) > 1) 1:(min(cols) - 1) else NULL
    middle <- as.numeric(cols)
    end <- if (max(cols) < ncol(df_table)) (max(cols) + 1):ncol(df_table) else NULL
    condensed <- sort(c(start, blank, end))
    sort <- match(names(df_table)[condensed], c("var", "num_cases", "plot_spacer", "p"), nomatch = NULL)
    list(
      start = start,
      middle = middle,
      end = end,
      condensed = condensed,
      sort = sort
    )
  }

  header_names <- c(
    label.dependent_var,
    "Cases (%)",
    paste0(label.effect_size, " (", 100 * stats.conf_interval, "% CI)"),
    "bolditalic(P) * bold(\"\U2013value\")"
  )[header_cols$sort]

  header_grob <- gridExtra::tableGrob(
    matrix(header_names, nrow = 1, ncol = 4),
    rows = NULL,
    cols = NULL,
    theme = gridExtra::ttheme_minimal(
      base_size = forest_theme_text$size,
      base_colour = forest_theme_text$colour,
      base_family = forest_theme_text$family,
      core = list(
        fg_params = list(
          fontface = 2L,
          parse = c(F, F, F, T)[header_cols$sort],
          hjust = tbl_hjust[1, header_cols$condensed],
          x = tbl_x[1, header_cols$condensed],
          vjust = 0.5,
          y = 0.5
        )
      )
    )
  )

  ## ------------------------------------ adjust dimensions ------------------------------------
  spacer_col <- match("plot_spacer", names(df_table))

  # resize table widths to take up whole panel
  # ...and reserve the plot_spacer column for 33% of the width; this is where the plot will go
  tbl_grob$widths <- {
    col_widths <- grid::convertWidth(tbl_grob$widths, "npc", T)
    col_widths[spacer_col] <- plot.rel_width
    col_widths[-spacer_col] <- (1 - plot.rel_width) * col_widths[-spacer_col] / sum(col_widths[-spacer_col])
    unit(col_widths, "npc")
  }

  # define the horizontal position of the forest area in the tableGrob
  forest_hOffset <- {
    l_cols <- if (spacer_col > 1) 1:(spacer_col - 1) else NULL
    r_cols <- if (spacer_col < ncol(df_table)) (spacer_col + 1):ncol(df_table) else NULL
    list(
      l = if (length(l_cols)) grid::convertWidth(sum(tbl_grob$widths[l_cols]), "npc", T) else 0,
      r = if (length(r_cols)) grid::convertWidth(sum(tbl_grob$widths[r_cols]), "npc", T) else 0
    )
  }

  # modify widths of header cells
  header_grob$widths <- {
    r <- grid::convertWidth(tbl_grob$widths, "npc", T)
    r <- c(r[header_cols$start], sum(r[header_cols$middle]), r[header_cols$end])
    unit(r, "npc")
  }

  # set table row heights to evenly expand (do this before adding vertical margins)
  tbl_grob$heights <- rep(unit(1, "null"), nrow(tbl_grob))

  # add vertical margins to the top/bottom of the tableGrob (to vertically align with the forest plot)
  tbl_grob <- {
    # get vertical margins of the forest plot panel
    forestGrob <- ggplot2::ggplotGrob(forest_core)
    panel_pos <- ggplot2::find_panel(forestGrob)
    vert_margin <- list(
      t = sum(forestGrob$heights[1:(panel_pos$t - 1)]),
      b = sum(forestGrob$heights[(panel_pos$b + 1):nrow(forestGrob)])
    )
    # ... and also add the width of the x-axis line to the bottom margin
    line_element <- ggplot2::calc_element("axis.line.x", modifyList(ggplot2::theme_get(), forest_core$theme, keep.null = T))
    vert_margin$b <- vert_margin$b + unit(line_element$linewidth, "mm")

    gtable::gtable_add_padding(tbl_grob, unit.c(vert_margin$t, unit(0, "pt"), vert_margin$b, unit(0, "pt")))
  }

  ## ------------------------------------ final output ------------------------------------
  {
    patchwork::wrap_plots(
      a_header = ggplot() + annotation_custom(as.ggGeomTextModify(header_grob)) + theme_void(),
      b_table = (
        ggplot() + annotation_custom(as.ggGeomTextModify(tbl_grob)) + theme_void() +
        patchwork::inset_element(forest_core,
          left = forest_hOffset$l,
          top = 1,
          bottom = 0,
          right = 1 - forest_hOffset$r,
          align_to = "full"
        )
      )
    ) +
      patchwork::plot_layout(
        design = c(
          a_header = area(t = 1, l = 1, b = 1, r = 1),
          b_table = area(t = 2, l = 1, b = num_models + 2, r = 1)
        )
      )
  }
}
