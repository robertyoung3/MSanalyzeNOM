#' compute_KMD_CH2
#'
#' This function computes the Kendrick mass defect (KMD) for a measured or
#' theoretical mass where CH2 (14.01565u) is the repeating unit (Kendrick 1963
#' and Hughey et al. 2001). The theoretical value is identical across members of
#' the same homologous CH2 series, and is useful when grouping for plots and
#' other purposes. The measured value is useful, especially in combination with
#' the appropriate z score, for identifying homologous CH2 series while
#' assigning formulas to measured m/z values. The number of digits can be
#' specified to limit rounding errors in the KMD calculation.
#'
#' @param df a tibble containing the following column names: "mz" and "theor_mz"
#' @param KMDtype a character string in c("meas", "theor") identifying the
#'   desired calculation
#' @param num_digits an integer indicating the number of desired digits for the
#'   KMD value (default = 4)
#'
#' @return df with a column containing the desired KMD values
#' @export
compute_KMD_CH2 <- function (df, KMDtype = "theor", num_digits = 4) {
  if (!KMDtype %in% c("meas", "theor")) {
    stop('Select "meas" to compute KMD_CH2 for the measured mz, or "theor" for the theoretical mz.')
  }
  if (KMDtype == "theor") {
    mass <- "theor_mz"
    col_name <- "KMD_CH2_theor"
  } else {
    mass <- "mz"
    col_name <- "KMD_CH2_meas"
  }
  mass_C <- 12
  mass_H <- 1.0078250
  mass_CH2 <- mass_C + 2*mass_H
  nominal_mass_CH2 <- round(mass_CH2, digits = 0)

  Kendrick_mass <- df[[mass]] * (nominal_mass_CH2/mass_CH2)
  nominal_Kendrick_mass <- round(Kendrick_mass, digits = 0)
  df[[col_name]] <- round(nominal_Kendrick_mass - Kendrick_mass,
                          digits = num_digits)
  df$z_CH2 <- nominal_Kendrick_mass %% nominal_mass_CH2
  df$z_CH2 <- df$z_CH2 - nominal_mass_CH2

  return(df)
}
