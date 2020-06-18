#' get_25perc_groups
#'
#' This function groups assigned formulas into four groups ("top 25%", "second
#' 25%", "third 25%", and "bottom 25%") by their percent contribution to total
#' assigned abundance.
#'
#' @param data a tibble containing the assigned molecular formulas
#'
#' @importFrom rlang .data
#'
#' @return data with a new column containing the 25% group to which each
#'   assigned formula belongs, ordered from "Top 25%" to "Bottom 25%".
#' @export
get_25perc_groups <- function(data) {
  data %>%
    get_perc_abund() %>%
    get_cum_perc_abund() %>%
    dplyr::arrange(.data$cum_perc_abund) %>%
    dplyr::mutate(group_25perc = cut(.data$cum_perc_abund,
                                     breaks = c(0, 25, 50, 75, Inf),
                                     labels = c("Bottom 25%", "Third 25%", "Second 25%", "Top 25%"),
                                     ordered_result = TRUE)) %>%
    dplyr::mutate(group_25perc = forcats::fct_rev(.data$group_25perc)) %>%
    dplyr::select(-.data$cum_perc_abund)
}
