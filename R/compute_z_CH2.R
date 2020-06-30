#' compute_z_CH2
#'
#' This function calculates z* values based on the nominal mass of a CH2
#' repeating unit, for use with Kendrick mass defects on the same basis for
#' formula assignment or visualization (Hsu et al. 1992, Stenson et al. 2003).
#'
#' @param df a tibble containing the following column name: "mz"
#'
#' @return df with a column containing the z* values
#' @export
compute_z_CH2 <- function(df) {
  df$z_CH2 <- round(df$mz, digits = 0)
  df$z_CH2 <- df$z_CH2 %% 14
  df$z_CH2 <- df$z_CH2 - 14
  return(df)
}
