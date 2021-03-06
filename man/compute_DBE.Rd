% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_DBE.R
\name{compute_DBE}
\alias{compute_DBE}
\title{compute_DBE}
\usage{
compute_DBE(df, elements = c("C", "H", "N", "O", "S"))
}
\arguments{
\item{df}{a tibble containing a table of the assigned elements with a column name
for each element (e.g., "C", "H", "N", "O" and "S")}

\item{elements}{a character vector of the elements used for formula assignment (default = CHNOS)}
}
\value{
df with a column containing DBE values
}
\description{
This function calculates double bond equivalents using the formula:
}
\details{
DBE= 1 + 0.5(sum [N_i(V_i - 2)])

where N_i is the element number and V_i is the valence for each element i.
}
