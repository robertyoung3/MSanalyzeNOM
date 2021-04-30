#' compute_2sample_peak_differences
#'
#' This function takes two dataframes containing assigned formulas, and returns
#' a dataframe containing the joined formulas, with relative abundance = 0 when
#' an assigned formula was not present in one of the samples.
#'
#' The dataframe also returns a column containing the differential relative
#' abundance when sample 1 is subtracted from sample 2. Values > 0 have greater
#' relative abundances in sample 2, and values < 0 have greater relative
#' abundances in sample 1.
#'
#' The function expects all of the static columns that are returned by the
#' get_sample_data() function, and will return an error if any of those column
#' names are missing from either sample. Specifically, those columns are:
#'
#' "class_element", "class_hetero", "chem_formula", "theor_MI_mass", "theor_mz",
#' "DBE", "C", "H", "N", "O", "S", "DBEtoC", "ModAI", "HtoC", "OtoC", "NtoC",
#' "KMD_CH2", "z_CH2", "NOSC", "mass_defect"
#'
#'
#' @param combined_samples a dataframe containing assigned formulas from
#' get_sample_data() for two different samples
#'
#' @importFrom rlang .data
#'
#' @return a dataframe containing the joined formulas and the differences in their
#' relative abundances
#'
#' @export
compute_2sample_peak_differences <- function (combined_samples){
  common_col_names <- c("class_element", "class_hetero", "chem_formula", "theor_MI_mass",
                        "theor_mz", "DBE", "C", "H", "N", "O", "S", "DBEtoC", "ModAI",
                        "HtoC", "OtoC", "NtoC", "KMD_CH2", "z_CH2", "NOSC", "mass_defect")

  if (sum(!common_col_names %in% colnames(combined_samples)) > 0) {
    stop("Does not contain the expected column names.")
  }

  temp_names <- unique(combined_samples$sample_name)
  if (length(temp_names) < 2) {
    stop("Does not contain at least 2 samples.")
  }

  sample_1 <- combined_samples %>%
    dplyr::filter(.data$sample_name == temp_names[1])
  sample_2 <- combined_samples %>%
    dplyr::filter(.data$sample_name == temp_names[2])

  comparison_DOM_formulas <- sample_1 %>%
    dplyr::full_join(sample_2, by = common_col_names) %>%
    dplyr::mutate(rel_abund.x = tidyr::replace_na(.data$rel_abund.x, 0),
                  rel_abund.y = tidyr::replace_na(.data$rel_abund.y, 0),
                  rel_abund_y_minus_x = .data$rel_abund.y - .data$rel_abund.x)
}