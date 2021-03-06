% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_VK_flat.R
\name{plot_VK_flat}
\alias{plot_VK_flat}
\title{plot_VK_flat}
\usage{
plot_VK_flat(data, plot_title = "", panel = FALSE, var_panel, num_col = 2)
}
\arguments{
\item{data}{a tibble containing the following column names: "HtoC"
and "OtoC"}

\item{plot_title}{a character string containing the sample name}

\item{panel}{a logical value specifying whether a panel will be used (default
= FALSE)}

\item{var_panel}{a character string containing the column name specifying the
factor variable to be used for faceting}

\item{num_col}{an integer specifying the number of panel columns (default =
2)}
}
\description{
This function makes a traditional van Krevelen diagram (H/c vs. O/C). This
plot shows the distribution of the assigned formulas as a function of
elemental ratios, but contains no information about the intensities of the
detected ions. The darker areas are where the highest number of formulas are
plotted.
}
\details{
The area above the horizontal line at H/C = 1.5 is designated as the
aliphatic region in accordance with D'Andrilli et al. 2015 and Lv et al.
2017. The area below the diagonal line from H/C = 1.1 is designated as the
aromatic region based on a modified aromaticity index >= 0.5 in accordance
with Koch & Dittmar 2006.
}
