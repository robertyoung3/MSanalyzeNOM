#' compute_DBEtoC
#'
#' This function computes the DBE/C ratio, which is used as an estimate for
#' aromaticity.
#'
#' @param data a tibble containing the following column names: "DBE" and "C"
#'
#' @importFrom rlang .data
#'
#' @return data with a new column containing the computed DBE/C ratios
#' @export
compute_DBEtoC <- function(data) {
  data %>%
    dplyr::mutate(DBEtoC = .data$DBE / .data$C)
}