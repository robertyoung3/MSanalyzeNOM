#' compute_AvgOSC
#'
#' This function calculates the average oxidation state of carbon (AvgOSC) for a
#' molecular formula, based on the formulas in Kroll et al. 2011, Riedel et al.
#' 2012., and Lavonen et al. 2013. It is currently limited to CHNOS elements and
#' their associated oxidation states (H = 1, N = -3, O = -2, S = -2). P was
#' reserved due to its number of oxidation states (most common = -3, 3, and
#' especially 5). The halogens (X = -1) were also reserved.
#'
#' @param df a tibble containing MS data, including element numbers for each
#'   elemental composition (e.g., C = 12, H = 26)
#' @param elements a character vector of the elements used for formula
#'   assignment (default = CHNOS)
#'
#' @importFrom rlang .data
#'
#' @return a vector containing the AvgOSC for every assigned formula
#' @export
compute_AvgOSC <- function (df, elements = c("C", "H", "N", "O", "S")) {
  #create a table of elements, ordered by elements
  temp <- df %>%
    dplyr::select(elements)

  formula_elements <- c("C", "H", "N", "O", "S")

  # identify elements that are not in formula elements
  # print error message
  # long-term: add halogens (X) and possibly P
  for (i in 1:length(elements)) {
    if (!elements[[i]] %in% formula_elements) {
      message("The AvgOSC calculation is currently limited to CHNOS elements.")
    }
  }

  # identify formula elements that are not in elements
  # add zero column
  for (i in 1:length(formula_elements)) {
    if (!formula_elements[[i]] %in% elements) {
      temp[ formula_elements[[i]] ] <- 0
    }
  }
  # AvgOSC = -sum(atom number (atom oxidation state)) / C number
  # AvgOSC = -((H * 1) + (N * -3) + (O * -2) + (S * -2) + (X * -1)) / C
    df %>%
      dplyr::mutate(AvgOSC = -(.data$H + (-3*.data$N) + (-2*.data$O) + (-2*.data$S))/.data$C)
}