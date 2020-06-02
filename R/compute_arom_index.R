#' compute_arom_index
#'
#' This function is designed to compute the aromaticity index (AI) or modified
#' aromaticity index (ModAI) for the assigned molecular formulas in an MS
#' dataset. The calclulations are derived from Koch and Dittmar 2006
#' (http://dx.doi.org/10.1002/rcm.2386) with erratum
#' (http://dx.doi.org/10.1002/rcm.7433). The ModAI calculation treats one-half
#' of the oxygen as carboxyl oxygen, rather than carbonyl oxygen, in line with
#' published NMR data on marine DOM. This increases the number of compounds that
#' qualify as aromatic or condensed aromatic, but is still more conservative
#' than RDBE/C ratios.
#'
#' @param sheet a tibble containing MS data, including element numbers for each
#'   elemental composition (e.g., C = 12, H = 26)
#' @param AItype a character string where "AI" = aromaticity index or "ModAI" =
#'   modified aromaticity index
#'
#' @return a vector containing the aromaticity index for every elemental
#'   composition
#' @export
#'
#' @examples
compute_arom_index <- function (sheet, AItype = "ModAI") {
  if (AItype ==  "ModAI") {
    numerator <- 1 + sheet$C - 0.5* sheet$H - 0.5*sheet$N - 0.5*sheet$O - sheet$S
    denominator <- sheet$C - sheet$N - 0.5*sheet$O - sheet$S
  } else if (AItype ==  "AI") {
    numerator <- 1 + sheet$C - 0.5* sheet$H - 0.5*sheet$N - sheet$O - sheet$S
    denominator <- sheet$C - sheet$N - sheet$O - sheet$S
  } else {
    print("ERROR. Please specify type of aromaticity index (AI or ModAI).")
  }
  numerator[numerator < 0] <- 0
  denominator[denominator < 0] <- 0
  arom_index <- numerator/denominator
  arom_index[is.nan(arom_index)] <- 0
  return(arom_index)
}
