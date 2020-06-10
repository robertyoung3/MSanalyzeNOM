#' get_cum_perc_abund
#'
#' This function arranges percent abundances from least to greatest and computes
#' their cumulative percent abundances.
#'
#' @param data a tibble containing the assigned molecular formulas
#'
#' @importFrom rlang .data
#'
#' @return data with a new column containing the cumulative percent abundances
#' @export
get_cum_perc_abund <- function (data) {
  data %>%
    dplyr::arrange(.data$perc_abund) %>%
    dplyr::mutate(cum_perc_abund = cumsum(.data$perc_abund))
}
