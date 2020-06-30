#' compute_perc_abund
#'
#' This function computes percent abundances from the relative abundances of
#' assigned formulas.
#'
#' @param data a tibble containing the following column name: "rel_abund"
#'
#' @importFrom rlang .data
#'
#' @return data with a new column containing the computed percent abundances
#' @export
compute_perc_abund <- function (data) {
data %>%
    dplyr::mutate(perc_abund = (.data$rel_abund / sum(.data$rel_abund)) * 100)
}
