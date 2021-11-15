#' @title Bland-Altman drawing function for R
#'
#' @description Bland-Altman drawing function. Depends on the blandr.statistics function in the package. Will generate a plot via the standard R plotting functions.
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @note Started 2015-11-14
#' @note Last update 2015-11-19
#' @note Originally designed for LAVAS and CVLA
#'
#' @param method1 A list of numbers.
#' @param method2 A list of numbers.
#' @inheritParams blandr.statistics
#' @inheritParams blandr.plot.limits
#' @inheritParams blandr.plot.ggplot
#'
#' @examples
#' # Generates two random measurements
#' measurement1 <- rnorm(100)
#' measurement2 <- rnorm(100)
#'
#' # Generates a plot, with no optional arguments
#' gg_bland_altman(measurement1, measurement2)
#' @export
gg_bland_altman <- function(method1,
                            method2,
                            method1name = "Method 1",
                            method2name = "Method 2",
                            plotTitle = "Bland-Altman plot for comparison of 2 methods",
                            sig.level = 0.95,
                            LoA.mode = 1,
                            ciDisplay = TRUE,
                            ciShading = TRUE,
                            ciFillColors = c("steelblue3", "gray85"),
                            normalLow = FALSE,
                            normalHigh = FALSE,
                            lowest_y_axis = FALSE,
                            highest_y_axis = FALSE,
                            point_size = 0.8,
                            overlapping = FALSE,
                            x.plot.mode = "means",
                            y.plot.mode = "difference",
                            plotProportionalBias = FALSE,
                            plotProportionalBias.se = TRUE,
                            assume.differences.are.normal = TRUE) {

  # Passes data to the blandr.statistics function to generate Bland-Altman statistics
  statistics.results <- blandr.statistics(method1, method2, sig.level, LoA.mode)

  # Passed data to the blandr.plot.limits function to generate plot limits
  # Only used for the basic R plots
  plot.limits <- blandr.plot.limits(statistics.results, lowest_y_axis, highest_y_axis)

  # Plots data

  # Pass data to the blandr.plot.ggplot function to use ggplot2 graphics system
  ba.plot <- blandr.plot.ggplot(
    statistics.results = statistics.results,
    method1name = method1name,
    method2name = method2name,
    plotTitle = plotTitle,
    ciDisplay = ciDisplay,
    ciShading = ciShading,
    ciFillColors = ciFillColors,
    normalLow = normalLow,
    normalHigh = normalHigh,
    overlapping = overlapping,
    x.plot.mode = x.plot.mode,
    y.plot.mode = y.plot.mode,
    point_size = point_size,
    plotProportionalBias = plotProportionalBias,
    plotProportionalBias.se = plotProportionalBias.se,
    assume.differences.are.normal = assume.differences.are.normal
  )
  return(ba.plot)
  # END OF FUNCTION
}

#' @title Data preparation for method comparison analysis
#'
#' @description Prepares the data and runs error checks before the calling function runs whatever method analysis mode is wants.
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @param method1 A list of numbers.
#' @param method2 A list of numbers.
#' @param sig.level Significance level. Is not optional in this function, as the calling package should have a default value to pass if needed
#'
#' @return method.comparison A data frame of paired values. These have been data checked, and empty rows omitted, from the originally supplied data.
#'
#' @examples
#' # Generates two random measurements
#' measurement1 <- rnorm(100)
#' measurement2 <- rnorm(100)
#'
#' # Calls the function - do note that this function was really
#' # meant to be called from other functions and not a stand-alone funtion
#' blandr.data.preparation(measurement1, measurement2, sig.level = 0.95)
#' @export
blandr.data.preparation <- function(method1, method2, sig.level) {

  # Put everything into a data frame for easier handling Stems from the error in some of
  # the sepsis data I had
  method.comparison <- data.frame(method1, method2)

  # Omit rows which have empty values
  method.comparison <- na.omit(method.comparison)

  # Data checks
  if (length(method.comparison$method1) != length(method.comparison$method2)) {
    stop("Method comparison analysis error: the 2 methods must have paired values.")
  }
  if (!is.numeric(method.comparison$method1)) {
    stop("Method comparison analysis error: the first method is not a number.")
  }
  if (!is.numeric(method.comparison$method2)) {
    stop("Method comparison analysis error: the second method is not a number.")
  }
  if (sig.level < 0) {
    stop("Method comparison analysis error: you can't have a significance level less than 0.")
  }
  if (sig.level < 0.8) {
    warning("Method comparison analysis warning: do you really want a significance level <0.8?")
  }
  if (sig.level > 1) {
    stop("Method comparison analysis error: you can't have a significance level greater than 1.")
  }
  if (sig.level == 1) {
    warning("Method comparison analysis warning: selecting a significance level of 1 suggest that probability testing might not be what you need.")
  }

  # Returns the data
  return(method.comparison)
}

#' @title Bland-Altman plotting function, using ggplot2
#'
#' @description Draws a Bland-Altman plot using data calculated using the other functions, using ggplot2
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @param statistics.results A list of statistics generated by the blandr.statistics function: see the function's return list to see what variables are passed to this function
#' @param method1name (Optional) Plotting name for 1st method, default "Method 1"
#' @param method2name (Optional) Plotting name for 2nd method, default "Method 2"
#' @param plotTitle (Optional) Title name, default "Bland-Altman plot for comparison of 2 methods"
#' @param point_size (Optional) Default = 0.8
#' @param ciDisplay (Optional) TRUE/FALSE switch to plot confidence intervals for bias and limits of agreement, default is TRUE
#' @param ciShading (Optional) TRUE/FALSE switch to plot confidence interval shading to plot, default is TRUE
#' @param ciFillColors (Optional) 2-vector of colors to use in the bias shading and confidence band shading
#' @param normalLow (Optional) If there is a normal range, entering a continuous variable will plot a vertical line on the plot to indicate its lower boundary
#' @param normalHigh (Optional) If there is a normal range, entering a continuous variable will plot a vertical line on the plot to indicate its higher boundary
#' @param overlapping (Optional) TRUE/FALSE switch to increase size of plotted point if multiple values using ggplot's geom_count, deafault=FALSE. Not currently recommend until I can tweak the graphics to make them better
#' @param x.plot.mode (Optional) Switch to change x-axis from being plotted by means (="means") or by either 1st method (="method1") or 2nd method (="method2"). Default is "means". Anything other than "means" will switch to default mode.
#' @param y.plot.mode (Optional) Switch to change y-axis from being plotted by difference (="difference") or by proportion magnitude of measurements (="proportion"). Default is "difference". Anything other than "proportional" will switch to default mode.
#' @param plotProportionalBias (Optional) TRUE/FALSE switch. Plots a proportional bias line. Default is FALSE.
#' @param plotProportionalBias.se (Optional) TRUE/FALSE switch. If proportional bias line is drawn, switch to plot standard errors. See stat_smooth for details. Default is TRUE.
#' @param assume.differences.are.normal (Optional, not operationally used currently) Assume the difference of means has a normal distribution. Will be used to build further analyses
#'
#' @return ba.plot Returns a ggplot data set that can then be plotted
#'
#' @import ggplot2
#'
#' @examples
#' # Generates two random measurements
#' measurement1 <- rnorm(100)
#' measurement2 <- rnorm(100)
#'
#' # Generates a ggplot
#' # Do note the ggplot function wasn't meant to be used on it's own
#' # and is generally called via the bland.altman.display.and.draw function
#'
#' # Passes data to the blandr.statistics function to generate Bland-Altman statistics
#' statistics.results <- blandr.statistics(measurement1, measurement2)
#'
#' # Generates a ggplot, with no optional arguments
#' blandr.plot.ggplot(statistics.results)
#'
#' # Generates a ggplot, with title changed
#' blandr.plot.ggplot(statistics.results, plotTitle = "Bland-Altman example plot")
#'
#' # Generates a ggplot, with title changed, and confidence intervals off
#' blandr.plot.ggplot(statistics.results,
#'   plotTitle = "Bland-Altman example plot",
#'   ciDisplay = FALSE, ciShading = FALSE
#' )
#' @export

blandr.plot.ggplot <- function(statistics.results,
                               method1name = "Method 1",
                               method2name = "Method 2",
                               plotTitle = "Bland-Altman plot for comparison of 2 methods",
                               point_size = 0.8,
                               ciDisplay = TRUE,
                               ciShading = TRUE,
                               ciFillColors = c("steelblue3", "gray85"),
                               normalLow = FALSE,
                               normalHigh = FALSE,
                               overlapping = FALSE,
                               x.plot.mode = "means",
                               y.plot.mode = "difference",
                               plotProportionalBias = FALSE,
                               plotProportionalBias.se = TRUE,
                               assume.differences.are.normal = TRUE) {

  # Does a check if ggplot2 is available
  # It should be as it is in the imports section but in CRAN checks some systems don't have it!
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  # Selects if x-axis uses means (traditional) or selects one of the methods
  # as the gold standard (non-traditional BA)
  # See Krouwer JS (2008) Why Bland-Altman plots should use X, not (Y+X)/2 when X is a reference method. Statistics in Medicine 27:778-780
  # NOT ENABLED YET
  x.axis <- statistics.results$means

  # Selects if uses differences (traditional) or proportions (non-traditional BA)
  if (y.plot.mode == "proportion") {
    y.axis <- statistics.results$proportion
  } else {
    y.axis <- statistics.results$differences
  }

  # Constructs the plot.data dataframe
  plot.data <- data.frame(x.axis, y.axis)

  # Rename to allow plotting
  # This was a hangover from an older version so I'm not sure we need it anymore
  # But not really a priority to check and remove now
  colnames(plot.data)[1] <- "x.axis"
  colnames(plot.data)[2] <- "y.axis"

  # Plot using ggplot
  ba.plot <- ggplot(plot.data, aes(x = x.axis, y = y.axis))

  # Drawing confidence intervals (OPTIONAL) -- put this here so that it appears under the points
  if (ciDisplay == TRUE) {
    ba.plot <- ba.plot +
      geom_hline(yintercept = statistics.results$biasUpperCI, linetype = 3) + # Bias - upper confidence interval
      geom_hline(yintercept = statistics.results$biasLowerCI, linetype = 3) + # Bias - lower confidence interval
      geom_hline(yintercept = statistics.results$upperLOA_upperCI, linetype = 3) + # Upper limit of agreement - upper confidence interval
      geom_hline(yintercept = statistics.results$upperLOA_lowerCI, linetype = 3) + # Upper limit of agreement - lower confidence interval
      geom_hline(yintercept = statistics.results$lowerLOA_upperCI, linetype = 3) + # Lower limit of agreement - upper confidence interval
      geom_hline(yintercept = statistics.results$lowerLOA_lowerCI, linetype = 3) # Lower limit of agreement - lower confidence interval

    # Shading areas for 95% confidence intervals (OPTIONAL)
    # This needs to be nested into the ciDisplay check
    if (ciShading == TRUE) {
      ba.plot <- ba.plot +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = statistics.results$biasLowerCI, ymax = statistics.results$biasUpperCI, fill = ciFillColors[1], alpha = 0.3) + # Bias confidence interval shading
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = statistics.results$upperLOA_lowerCI, ymax = statistics.results$upperLOA_upperCI, fill = ciFillColors[2], alpha = 0.3) + # Upper limits of agreement confidence interval shading
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = statistics.results$lowerLOA_lowerCI, ymax = statistics.results$lowerLOA_upperCI, fill = ciFillColors[2], alpha = 0.3) # Lower limits of agreement confidence interval shading
    }
  }

  # continue plotting
  ba.plot <- ba.plot +
    geom_point(size = point_size) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_hline(yintercept = 0, linetype = 1) + # "0" line
    geom_hline(yintercept = statistics.results$bias, linetype = 2) + # Bias
    geom_hline(yintercept = statistics.results$bias + (statistics.results$biasStdDev * statistics.results$sig.level.convert.to.z), linetype = 2) + # Upper limit of agreement
    geom_hline(yintercept = statistics.results$bias - (statistics.results$biasStdDev * statistics.results$sig.level.convert.to.z), linetype = 2) + # Lower limit of agreement
    ggtitle(plotTitle) +
    xlab("Means")

  # Re-titles the y-axis dependent on which plot option was used
  if (y.plot.mode == "proportion") {
    ba.plot <- ba.plot + ylab("Difference / Average %")
  } else {
    ba.plot <- ba.plot + ylab("Differences")
  }


  ### Function has finished drawing of confidence intervals at this line

  # If a normalLow value has been sent, plots this line
  if (normalLow != FALSE) {
    # Check validity of normalLow value to plot line
    if (is.numeric(normalLow) == TRUE) {
      ba.plot <- ba.plot + geom_vline(xintercept = normalLow, linetype = 4, col = 6)
    }
  }

  # If a normalHighvalue has been sent, plots this line
  if (normalHigh != FALSE) {
    # Check validity of normalHigh value to plot line
    if (is.numeric(normalHigh) == TRUE) {
      ba.plot <- ba.plot + geom_vline(xintercept = normalHigh, linetype = 4, col = 6)
    }
  }

  # If overlapping=TRUE uses geom_count
  # See the param description at the top
  if (overlapping == TRUE) {
    ba.plot <- ba.plot + geom_count()
  }

  # If plotProportionalBias switch is TRUE, plots a proportional bias line as well
  if (plotProportionalBias == TRUE) {

    # Check for validity of options passed to the plotProportionalBias.se switch
    # As if we throw an invalid option to ggplot it will just stop with an error
    if (plotProportionalBias.se != TRUE && plotProportionalBias.se != FALSE) {
      plotProportionalBias.se <- TRUE
    }

    # Plots line
    ba.plot <- ba.plot + ggplot2::geom_smooth(method = "lm", se = plotProportionalBias.se)
  } # End of drawing proportional bias line

  # Draws marginal histograms if option selected
  # Idea from http://labrtorian.com/tag/bland-altman/
  # REMOVED AS INTRODUCED SOME INCOMPATIBILITIES DEPENDENT ON USERS R VERSION
  # ALSO MASSIVELY INCREASED PACKAGE SIZE
  # if( marginalHistogram == TRUE ) { ba.plot <- ggMarginal( ba.plot , type="histogram" ) }

  # Return the ggplot2 output
  return(ba.plot)

  # END OF FUNCTION
}

#' @title Bland-Altman plot limits for R
#'
#' @description Works out plot limits for the Bland-Altman plots. Depends on the blandr.statistics function in the package.
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @param statistics.results A list of statistics generated by the blandr.statistics function: see the function's return list to see what variables are passed to this function
#' @param lowest_y_axis (Optional) Defaults to NULL If given a continuous variable will use this as the lower boundary of the y axis. Useful if need multiple plots with equivalent y-axes.
#' @param highest_y_axis (Optional) Defaults to NULL If given a continuous variable will use this as the upper boundary of the y axis. Useful if need multiple plots with equivalent y-axes.
#' @return x_upper The upper limit of the X-axis
#' @return x_lower The lower limit of the X-axis
#' @return y_upper The upper limit of the Y-axis
#' @return y_lower The lower limit of the Y-axis
#'
#' @examples
#' # Generates two random measurements
#' measurement1 <- rnorm(100)
#' measurement2 <- rnorm(100)
#'
#' # Passes data to the blandr.statistics function to generate Bland-Altman statistics
#' statistics.results <- blandr.statistics(measurement1, measurement2)
#'
#' # Calls the function
#' blandr.plot.limits(statistics.results)
#' @export

blandr.plot.limits <- function(statistics.results, lowest_y_axis = FALSE, highest_y_axis = FALSE) {

  # Calculates a margin for error - therefore labelled bounds
  x_bounds <- (max(statistics.results$means) - min(statistics.results$means)) * 0.1 ### this is a margin of error for drawing the x axis
  y_bounds <- (max(statistics.results$differences) - min(statistics.results$differences)) *
    0.1 ### this is a margin of error for drawing the y axis

  # Sets limits for plot on the x-axis
  x_upper <- max(statistics.results$means) + x_bounds
  x_lower <- min(statistics.results$means) - x_bounds

  # Sets limits for plot on the y-axis
  y_upper <- max(statistics.results$differences) + y_bounds
  y_lower <- min(statistics.results$differences) - y_bounds

  # Ensures that y-axis includes the whole range of confidence intervals
  if (y_upper <= statistics.results$upperLOA_upperCI) {
    y_upper <- statistics.results$upperLOA_upperCI + y_bounds
  }
  if (y_lower >= statistics.results$lowerLOA_lowerCI) {
    y_lower <- statistics.results$lowerLOA_lowerCI - y_bounds
  }

  # If the user has requested specific limits to the y-axis the following 2 lines execute
  # this
  if (highest_y_axis != FALSE) {
    y_upper <- highest_y_axis
  }
  if (lowest_y_axis != FALSE) {
    y_lower <- lowest_y_axis
  }

  return(
    list(x_upper = x_upper, x_lower = x_lower, y_upper = y_upper, y_lower = y_lower) # CLOSE OF LIST
  ) # CLOSE OF RETURN

  # END OF FUNCTION
}


#' @title Bland-Altman statistics for R
#'
#' @description Bland-Altman analysis function for R. Package created as existing
#' functions don't suit my needs, and don't generate 95\% confidence intervals
#' for bias and limits of agreement. This base function calculates the basic
#' statistics, and generates return values which can be used in the related
#' \code{blandr.display} and \code{bland.altamn.plot} functions. However
#' the return results can be used to generate a custom chart if desired.
#'
#' @author Deepankar Datta <deepankardatta@nhs.net>
#'
#' @note The function will give similar answers when used on the original Bland-Altman PEFR data sets. They won't be exactly the same as (a) for 95\% limits of agreement I have used +/-1.96, rather than 2, and (b) the computerised calculation means that the rounding that is present in each step of the original examples does not occur. This will give a more accurate answer, although I can understand why in 1986 rounding would occur at each step for ease of calculation.
#' @note The function depends on paired values.
#' @note It currently only can currently work out fixed bias.
#' @note Improvements for the future: proportional bias charts will need further work
#'
#' @note Started 2015-11-14
#' @note Last update 2016-02-04
#' @note Originally designed for LAVAS and CVLA
#'
#' @references Based on: (1) Bland, J. M., & Altman, D. (1986). Statistical methods for assessing agreement between two methods of clinical measurement. The Lancet, 327(8476), 307-310. http://dx.doi.org/10.1016/S0140-6736(86)90837-8
#' @references Confidence interval work based on follow-up paper: (2) Altman, D. G., & Bland, J. M. (2002). Commentary on quantifying agreement between two methods of measurement. Clinical chemistry, 48(5), 801-802. http://www.clinchem.org/content/48/5/801.full.pdf
#'
#' @param x Either a formula, or a vector of numbers corresponding to the results from method 1.
#' @param y A vector of numbers corresponding to the results from method 2. Only needed if \code{X} is a vector.
#' @param sig.level (Optional) Two-tailed significance level. Expressed from 0 to 1. Defaults to 0.95.
#' @param LoA.mode (Optional) Switch to change how accurately the limits of agreement (LoA) are calculated from the bias and its standard deviation. The default is LoA.mode=1 which calculates LoA with the more accurate 1.96x multiplier. LoA.mode=2 uses the 2x multiplier which was used in the original papers. This should really be kept at default, except to double check calculations in older papers.
#'
#' @return An object of class 'blandr' is returned. This is a list with the following elements:
#' \item{means}{List of arithmetic mean of the two methods}
#' \item{differences}{List of differences of the two methods}
#' \item{method1}{Returns the 'method1' list in the data frame if further evaluation is needed}
#' \item{method2}{Returns the 'method2' list in the data frame if further evaluation is needed}
#' \item{sig.level}{Significance level supplied to the function}
#' \item{sig.level.convert.to.z}{Significance level convert to Z value}
#' \item{bias}{Bias of the two methods}
#' \item{biasUpperCI}{Upper confidence interval of the bias (based on significance level)}
#' \item{biasLowerCI}{Lower confidence interval of the bias (based on significance level)}
#' \item{biasStdDev}{Standard deviation for the bias}
#' \item{biasSEM}{Standard error for the bias}
#' \item{LOA_SEM}{Standard error for the limits of agreement}
#' \item{upperLOA}{Upper limit of agreement}
#' \item{upperLOA_upperCI}{Upper confidence interval of the upper limit of agreement}
#' \item{upperLOA_lowerCI}{Lower confidence interval of the upper limit of agreement}
#' \item{lowerLOA}{Lower limit of agreement}
#' \item{lowerLOA_upperCI}{Upper confidence interval of the lower limit of agreement}
#' \item{lowerLOA_lowerCI}{Lower confidence interval of the lower limit of agreement}
#' \item{proportion}{Differences/means*100}
#' \item{no.of.observations}{Number of observations}
#' \item{regression.equation}{A regression equation to help determine if there is any proportional bias}
#' \item{regression.fixed.slope}{The slope value of the regression equation}
#' \item{regression.fixed.intercept}{The intercept value of the regression equation}
#'
#' @importFrom stats coef cor lm na.omit qnorm qt sd t.test
#'
#' @examples
#'
#' # Generates two random measurements
#' measurement1 <- rnorm(100)
#' measurement2 <- rnorm(100)
#'
#' # Generates Bland-Altman statistics data of the two measurements
#' blandr.statistics(measurement1, measurement2)
#' @rdname blandr.statistics
#' @export
blandr.statistics <- function(x,
                              y,
                              sig.level = 0.95,
                              LoA.mode = 1) {

  # This sends to the preparation function, which does some sense checks on the data And
  # makes sure that the values are prepared
  method1 <- x
  method2 <- y
  ba.data <- blandr.data.preparation(method1, method2, sig.level)

  # method1 and method2 are the measurements
  means <- (ba.data$method1 + ba.data$method2) / 2
  differences <- ba.data$method1 - ba.data$method2
  bias <- mean(differences)
  biasStdDev <- sd(differences)

  # Convert confidence interval to a two-tailed z-value Don't really need this but kept as
  # a remnant of old version to possibly use in the future
  alpha <- 1 - sig.level
  sig.level.two.tailed <- 1 - (alpha / 2)
  sig.level.convert.to.z <- qnorm(sig.level.two.tailed)

  # Compute the 95% limits of agreement (based on 1st paper) Don't use the significance
  # level supplied to the function here!  (The significance level is for confidence
  # intervals, not for limits of agreement!)  We want to know in the sample what limits 95%
  # of the pop resides within The LoA.mode switch can change before the +/-1.96 multiplier
  # used in more recent papers Or the estimated +/-2 multiplier used in the original paper
  # The default is LoA.mode which gives a LoA.multiplier of 1.96. In future we could use an
  # even more precise multipler for limits of agreement.
  if (LoA.mode == 2) {
    LoA.multiplier <- 2
  } else {
    LoA.multiplier <- 1.96
  }
  upperLOA <- bias + (LoA.multiplier * biasStdDev)
  lowerLOA <- bias - (LoA.multiplier * biasStdDev)

  # Confidence intervals (based on 2nd paper) Based on significance level supplied
  # (defaults to 95% CI) CI for mean
  biasSEM <- sd(differences) / sqrt(length(differences))
  biasCI <- qt(sig.level.two.tailed, df = length(differences) - 1) * biasSEM
  biasUpperCI <- bias + biasCI
  biasLowerCI <- bias - biasCI

  # CI for limits of agreement LOAVariance from Carkeet
  LOAVariance <- ((1 / length(differences)) + ((sig.level.convert.to.z^2) / (2 * (length(differences) -
    1)))) * biasStdDev^2
  LOA_SEM <- sqrt(LOAVariance)
  LOA_CI <- qt(sig.level.two.tailed, df = length(differences) - 1) * LOA_SEM
  upperLOA_upperCI <- upperLOA + LOA_CI
  upperLOA_lowerCI <- upperLOA - LOA_CI
  lowerLOA_upperCI <- lowerLOA + LOA_CI
  lowerLOA_lowerCI <- lowerLOA - LOA_CI

  # Difference/mean proportion
  proportion <- differences / means * 100

  # Number of observations
  no.of.observations <- length(means)

  # Works out numbers for proportional bias
  # Couldn't figure this out myself So took example code from
  # http://rforpublichealth.blogspot.co.uk/2013/11/ggplot2-cheatsheet-for-scatterplots.html
  m <- lm(differences ~ means)
  a <- signif(coef(m)[1], digits = 2)
  b <- signif(coef(m)[2], digits = 2)
  regression.equation <- paste("y(differences) = ", b, " x(means) + ", a, sep = "")

  results <-
    list(
      means = means,
      differences = differences,
      method1 = method1,
      method2 = method2,
      sig.level = sig.level,
      sig.level.convert.to.z = sig.level.convert.to.z,
      bias = bias,
      biasUpperCI = biasUpperCI,
      biasLowerCI = biasLowerCI,
      biasStdDev = biasStdDev,
      biasSEM = biasSEM,
      LOA_SEM = LOA_SEM,
      upperLOA = upperLOA,
      upperLOA_upperCI = upperLOA_upperCI,
      upperLOA_lowerCI = upperLOA_lowerCI,
      lowerLOA = lowerLOA,
      lowerLOA_upperCI = lowerLOA_upperCI,
      lowerLOA_lowerCI = lowerLOA_lowerCI,
      proportion = proportion,
      no.of.observations = no.of.observations,
      regression.equation = regression.equation,
      regression.fixed.slope = b,
      regression.fixed.intercept = a
    ) # CLOSE OF LIST

  class(results) <- "blandr"
  return(results)

  # END OF FUNCTION
}