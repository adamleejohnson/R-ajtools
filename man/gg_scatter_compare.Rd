% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_scatterCompare.R
\name{gg_scatter_compare}
\alias{gg_scatter_compare}
\title{Scatter plot for method comparisons}
\usage{
gg_scatter_compare(
  data,
  data.x,
  data.y,
  label.x,
  label.y,
  title,
  units,
  lim,
  line.color = "blue",
  line.size = 0.7,
  point.size = 0.7,
  ticks.breaks = NULL,
  ticks.minor_breaks = NULL,
  ...
)
}
\arguments{
\item{data}{Data frame}

\item{data.x}{String label for x-axis data}

\item{data.y}{String label for y-axis data}

\item{label.x}{Plot label for x-axis}

\item{label.y}{Plot label for y-axis}

\item{title}{Plot title}

\item{units}{Units to append to axes (default = NULL)}

\item{lim}{Axes limits in form of 2-vector to apply to both x and y axes (the plot should be square)}

\item{line.color}{Color of the fit line (default = "blue")}

\item{line.size}{Fit line size (default = 0.7)}

\item{point.size}{Scatter point size (default = 0.7)}

\item{ticks.breaks}{Vector of break points to use on the axes}

\item{ticks.minor_breaks}{Vector of minor break points to use on the axes}

\item{...}{Additional theme parameters}
}
\description{
Define a scatter plot generator for making comparisons between two different methods
}
