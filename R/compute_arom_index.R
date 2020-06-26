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
#' than RDBE/C ratios. A (modified) aromaticity index >= 0.5 indicates an
#' aromatic structure, and a (modified) aromaticity index >= 0.67 indicates a
#' condensed aromatic structure.
#'
#' @param df a tibble containing MS data, including element numbers for each
#'   elemental composition (e.g., C = 12, H = 26)
#' @param elements a character vector of the elements used for formula assignment (default = CHNOS)
#' @param AItype a character string where "AI" = aromaticity index or "ModAI" =
#'   modified aromaticity index
#'
#' @importFrom rlang .data
#'
#' @return a vector containing the aromaticity index for every assigned formula
#' @export
compute_arom_index <- function (df, elements = c("C", "H", "N", "O", "S"), AItype = "ModAI") {
  #create a table of elements, ordered by elements
  temp <- df %>%
    dplyr::select(elements)

  formula_elements <- c("C", "H", "N", "O", "P", "S")

  # identify elements that are not in formula elements
  # print error message
  # long-term: sum valence 1 elements to include halogens (X)
  for (i in 1:length(elements)) {
    if (!elements[[i]] %in% formula_elements) {
      message("The aromaticity index calculation is currently limited to CHNOPS elements.")
    }
  }

  # identify formula elements that are not in elements
  # add zero column
  for (i in 1:length(formula_elements)) {
    if (!formula_elements[[i]] %in% elements) {
      temp[ formula_elements[[i]] ] <- 0
    }
  }

  # compute the arom_index numerators and denominators
  if (AItype ==  "AI") {
    numerator <- 1 + temp$C - 0.5* temp$H - 0.5*temp$N - temp$O - 0.5*temp$P - temp$S
    denominator <- temp$C - temp$N - temp$O - temp$P - temp$S
  } else {
    # ModAI
    numerator <- 1 + temp$C - 0.5* temp$H - 0.5*temp$N - 0.5*temp$O - 0.5*temp$P - temp$S
    denominator <- temp$C - temp$N - 0.5*temp$O - temp$P - temp$S
  }

  # compute and clean up the arom_index
  numerator[numerator < 0] <- 0
  denominator[denominator < 0] <- 0
  arom_index <- numerator/denominator
  arom_index[is.nan(arom_index)] <- 0

  # assign arom_index to df
  if (AItype ==  "AI") {
    df$AI <- arom_index
  } else {
    df$ModAI <- arom_index
  }

  return(df)
}
