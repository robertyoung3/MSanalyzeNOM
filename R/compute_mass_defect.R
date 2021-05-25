#' compute_mass_defect
#'
#' This function computes the mass defects of the assigned formulas.
#'
#' @param data a tibble containing the following column name: "theor_mass"
#'
#' @importFrom rlang .data
#'
#' @return data with a new column containing the computed percent abundances
#' @export
compute_mass_defect <- function (data) {
  data %>%
    dplyr::mutate(mass_defect = (.data$theor_mass -
                                   round(.data$theor_mass, digits = 0)))
}

