#' get_all_detected_ions
#'
#' This function combines the mz's and relative abundances of all detected ions
#' in the sample, including the detected ions that could not be assigned
#' formulas ("no hits").
#'
#' @param no_hits a tibble containing mz's and relative abundances for the "no
#'   hits" under the following column names: "mz" and "rel_abund"
#' @param assigned_formulas a tibble containing mz's and relative abundances for
#'   the assigned formulas under the following column names: "mz" and "rel_abund"
#'
#' @importFrom rlang .data
#'
#' @return a tibble containing the combined mz's and relative abundances
#' @export
get_all_detected_ions <- function(no_hits, assigned_formulas) {
  assigned_formulas %>%
    dplyr::select(.data$mz, .data$rel_abund) %>%
    dplyr::bind_rows(no_hits) %>%
    dplyr::arrange(.data$mz) %>%
    # normalize rel_abund
    dplyr::mutate(rel_abund = .data$rel_abund / max(.data$rel_abund) * 100)
}