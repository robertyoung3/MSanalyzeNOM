#' compute_100norm_rel_abund
#'
#' This function normalizes the relative abundances of the #' assigned formulas
#' to 100.
#'
#' @param data a tibble containing the following column name: "rel_abund"
#'
#' @importFrom rlang .data
#'
#' @return data with a new column containing the normalized abundances
#' @export
compute_100norm_rel_abund <- function (data) {
data %>%
    dplyr::mutate(rel_abund = (.data$rel_abund / max(.data$rel_abund)) * 100)
}
