% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_mass_spectrum.R
\name{plot_mass_spectrum}
\alias{plot_mass_spectrum}
\title{plot_mass_spectrum}
\usage{
plot_mass_spectrum(
  data,
  plot_title = "",
  line_color = "black",
  line_size = 0.1,
  min_x = NULL,
  max_x = NULL
)
}
\arguments{
\item{data}{a tibble containing the following column names: "mz" and "rel_abund"}

\item{plot_title}{a character string containing the sample name}

\item{line_color}{a character string containing the line color}

\item{line_size}{a numeric value containing the line size}

\item{min_x}{a numeric containing the minimum mz value to plot (default = NULL)}

\item{max_x}{a numeric containing the maximum mz value to plot (default = NULL)}
}
\description{
This function plots the sample mass spectrum.The minimum and maximum mz
values can be specified to plot a specific region of the mass spectrum.
}
