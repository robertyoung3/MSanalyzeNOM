#' DOM_formulas
#'
#' A dataset containing elemental composition and other data for a DOM sample
#' analyzed by NegESI FTICR MS.
#'
#' @format A tibble with 6,119 rows and 11 variables:
#' \itemize{
#'   \item {hetero_class, the number and identity of heteroatoms in the molecular formula}
#'   \item {chem_formula, the molecular formula}
#'   \item {mz, the experimental mass-to-charge ratio (m/z) of the detected ion}
#'   \item {theor_mz, the theoretical mass-to-charge ratio (m/z) of the assigned molecular formula}
#'   \item {ppm, the experimental error if the detected ion is the assigned molecular formula, in parts-per-million (ppm)}
#'   \item {rel_abund, the abundance of the detected ion in proportion to the most abundant ion, x 100}
#'   \item {C, the number of carbon atoms in the assigned molecular formula}
#'   \item {H, the number of hydrogen atoms in the assigned molecular formula}
#'   \item {N, the number of nitrogen atoms in the assigned molecular formula}
#'   \item {O, the number of oxygen atoms in the assigned molecular formula}
#'   \item {S, the number of sulfur atoms in the assigned molecular formula}
#' }
#' @source personal data
#' @usage data(DOM_formulas)
"DOM_formulas"