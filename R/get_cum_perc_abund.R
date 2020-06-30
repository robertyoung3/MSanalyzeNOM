#' get_cum_perc_abund
#'
#' This internal function arranges percent abundances from least to greatest and computes
#' their cumulative percent abundances. It is called by get_25perc_groups.
#'
#' @param data a tibble containing the following column name: "perc_abund"
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
