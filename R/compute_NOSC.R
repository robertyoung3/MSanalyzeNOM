#' compute_NOSC
#'
#' This function calculates the average oxidation state of carbon (NOSC) for a
#' molecular formula, based on the formulas in Kroll et al. 2011, Riedel et al.
#' 2012., and Lavonen et al. 2013. It is currently limited to CHNOS elements and
#' their associated oxidation states (H = 1, N = -3, O = -2, S = -2). P was
#' reserved due to its number of oxidation states (most common = -3, 3, and
#' especially 5). The halogens (X = -1) were also reserved.
#'
#' @param df a tibble containing a table of the assigned elements with a column name
#' for each element (e.g., "C", "H", "N", "O" and "S")
#' @param elements a character vector of the elements used for formula
#'   assignment (default = CHNOS)
#'
#' @importFrom rlang .data
#'
#' @return a vector containing the NOSC for every assigned formula
#' @export
compute_NOSC <- function (df, elements = c("C", "H", "N", "O", "S")) {
  #create a table of elements, ordered by elements
  temp <- df %>%
    dplyr::select(elements)

  formula_elements <- c("C", "H", "N", "O", "S")

  # identify elements that are not in formula elements
  # print error message
  # long-term: add halogens (X)
  for (i in 1:length(elements)) {
    if (!elements[[i]] %in% formula_elements) {
      message("The NOSC calculation is currently limited to CHNOS elements.")
    }
  }

  # identify formula elements that are not in elements
  # add zero column
  for (i in 1:length(formula_elements)) {
    if (!formula_elements[[i]] %in% elements) {
      temp[ formula_elements[[i]] ] <- 0
    }
  }
  # NOSC = -sum(atom number (atom oxidation state)) / C number
  # NOSC = -(1*H + -3*N + -2*O + -2*S + -1*X) / C
    df %>%
      dplyr::mutate(NOSC = -(.data$H + (-3*.data$N) + (-2*.data$O) + (-2*.data$S))/.data$C)
}