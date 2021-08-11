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
#' @include blandr.statistics.r
#' @include blandr.plot.limits.r
#' @include blandr.plot.ggplot.r
#'
#' @examples
#' # Generates two random measurements
#' measurement1 <- rnorm(100)
#' measurement2 <- rnorm(100)
#'
#' # Generates a plot, with no optional arguments
#' ggplot_bland.altman( measurement1 , measurement2 )
#'
#' @export

ggplot_bland.altman <- function( method1 ,
                                 method2 ,
                                 method1name = "Method 1" ,
                                 method2name = "Method 2" ,
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
                                 overlapping = FALSE ,
                                 x.plot.mode = "means" ,
                                 y.plot.mode = "difference" ,
                                 plotProportionalBias = FALSE ,
                                 plotProportionalBias.se = TRUE ,
                                 assume.differences.are.normal = TRUE
) {

    # Passes data to the blandr.statistics function to generate Bland-Altman statistics
    statistics.results <- blandr.statistics(method1, method2, sig.level, LoA.mode)

    # Passed data to the blandr.plot.limits function to generate plot limits
    # Only used for the basic R plots
    plot.limits <- blandr.plot.limits( statistics.results , lowest_y_axis, highest_y_axis)

    # Plots data

    # Pass data to the blandr.plot.ggplot function to use ggplot2 graphics system
    ba.plot <- blandr.plot.ggplot( statistics.results = statistics.results ,
                              method1name = method1name ,
                              method2name = method2name ,
                              plotTitle = plotTitle ,
                              ciDisplay = ciDisplay ,
                              ciShading = ciShading ,
                              ciFillColors = ciFillColors,
                              normalLow = normalLow ,
                              normalHigh = normalHigh ,
                              overlapping = overlapping ,
                              x.plot.mode = x.plot.mode ,
                              y.plot.mode = y.plot.mode ,
                              point_size = point_size,
                              plotProportionalBias = plotProportionalBias ,
                              plotProportionalBias.se = plotProportionalBias.se ,
                              assume.differences.are.normal = assume.differences.are.normal
                              )
    return(ba.plot)
    # END OF FUNCTION
}
