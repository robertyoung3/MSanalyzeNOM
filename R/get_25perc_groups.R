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
#' @return data with a new column containing the "25perc_group" to which each
#'   assigned formula belongs
#' @export
get_25perc_groups <- function(data) {
  data %>%
    dplyr::arrange(.data$cum_perc_abund) %>%
    dplyr::mutate(`25perc_group` = cut(.data$cum_perc_abund,
                                       breaks = c(0, 25, 50, 75, Inf),
                                       labels = c("Bottom 25%", "Third 25%", "Second 25%", "Top 25%"),
                                       ordered_result = TRUE)) %>%
    dplyr::mutate(`25perc_group` = factor(.data$`25perc_group`,
                                          levels = c("Top 25%", "Second 25%", "Third 25%", "Bottom 25%")))
}
