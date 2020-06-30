#' remove_common_contaminants
#'
#' This function removes LAS surfactants and stearic and palmitic acids from the
#' assigned formulas. It can be useful for visualizing the remaining NOM
#' components, but should not be used without appropriate disclosure and
#' annotation. Removing these contaminants also doesn't remove their influence
#' on the other detected ions and assigned formulas, which can be reduced in
#' number due to the presence of high abundance contaminants.
#'
#' @param df a tibble containing the following column names: "chem_formula",
#'   "class_hetero", "KMD_CH2_theor" (from the compute_KMD_CH2() function),
#'   "z_CH2" (from the compute_z_CH2() function), and "rel_abund"
#'
#' @importFrom rlang .data
#'
#' @return df after removing the specified contaminants
#' @export
remove_common_contaminants <- function(df) {
  df %>%
    #remove stearic and palmitic acid
    dplyr::filter(!.data$chem_formula %in% c("C18H36O2", "C16H32O2")) %>%
    # remove LAS surfactants
    dplyr::filter(!(.data$class_hetero == "O3 S1" & .data$KMD_CH2_theor == "0.1788"
                    & .data$z_CH2 == "-11")) %>%
    # normalize rel_abund to new highest value & perc_abund
    dplyr::mutate(rel_abund = .data$rel_abund / max(.data$rel_abund) * 100,
                  perc_abund = .data$rel_abund/ sum(.data$rel_abund) * 100)
}