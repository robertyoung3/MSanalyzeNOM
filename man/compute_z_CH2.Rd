% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_z_CH2.R
\name{compute_z_CH2}
\alias{compute_z_CH2}
\title{compute_z_CH2}
\usage{
compute_z_CH2(df)
}
\arguments{
\item{df}{a tibble containing the following column name: "mz"}
}
\value{
df with a column containing the z* values
}
\description{
This function calculates z* values based on the nominal mass of a CH2
repeating unit, for use with Kendrick mass defects on the same basis for
formula assignment or visualization (Hsu et al. 1992, Stenson et al. 2003).
}
