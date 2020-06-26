#' compute_z_CH2
#'
#' This function calculates z scores based on the nominal mass of a CH2
#' repeating unit, for use with Kendrick mass defects on the same basis for
#' formula assignment or visualization (Stenson et al. 2003).
#'
#' @param df a tibble containing MS data
#'
#' @return df with a column containing the z scores
#' @export
compute_z_CH2 <- function(df) {
  df$z_CH2 <- round(df$mz, digits = 0)
  df$z_CH2 <- df$z_CH2 %% 14
  df$z_CH2 <- df$z_CH2 - 14
  return(df)
}
