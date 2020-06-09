#' get_perc_abund
#'
#' This function computes percent abundances from the relative abundances of
#' assigned formulas.
#'
#' @param data a tibble containing the assigned molecular formulas
#'
#' @importFrom rlang .data
#'
#' @return data with a new column containing the computed percent abundances
#' @export
get_perc_abund <- function (data) {
data %>%
    dplyr::mutate(perc_abund = (.data$rel_abund / sum(.data$rel_abund)) * 100)
}
