#' compute_nominal_mz
#'
#' This function computes nominal mz values from a column containing mz values
#' (typically, either the assigned_formulas or the all_detected_ions dataframe.
#'
#' @param data a tibble containing the following column name: "mz"
#'
#' @importFrom rlang .data
#'
#' @return data with a new column containing the computed percent abundances
#' @export
compute_nominal_mz <- function (data) {
  data %>%
    dplyr::mutate(nominal_mz = round(.data$mz, digits = 0))
}
