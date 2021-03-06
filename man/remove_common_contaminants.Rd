% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_common_contaminants.R
\name{remove_common_contaminants}
\alias{remove_common_contaminants}
\title{remove_common_contaminants}
\usage{
remove_common_contaminants(df)
}
\arguments{
\item{df}{a tibble containing the following column names: "chem_formula",
"class_hetero", "KMD_CH2" (from the compute_KMD_CH2() function),
"z_CH2" (from the compute_z_CH2() function), and "rel_abund"}
}
\value{
df after removing the specified contaminants
}
\description{
This function removes LAS surfactants and stearic and palmitic acids from the
assigned formulas. It can be useful for visualizing the remaining NOM
components, but should not be used without appropriate disclosure and
annotation. Removing these contaminants also doesn't remove their influence
on the other detected ions and assigned formulas, which can be reduced in
number due to the presence of high abundance contaminants.
}
