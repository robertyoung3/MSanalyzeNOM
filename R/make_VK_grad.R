#' make_VK_grad
#'
#' This function makes a traditional van Krevelen diagram (H/c vs. O/C). This
#' plot shows the distribution of the assigned formulas as a function of
#' elemental ratios, and shows the intensities of the detected ions on a
#' gradient color scale.
#'
#' The area above the horizontal line at H/C = 1.5 is designated as the
#' aliphatic region in accordance with D'Andrilli et al. 2015 and Lv et al.
#' 2017. The area below the diagonal line from H/C = 1.1 is designated as the
#' aromatic region based on a modified aromaticity index >= 0.5 in accordance
#' with Koch & Dittmar 2006.
#'
#' @param data a tibble containing the assigned molecular formulas
#' @param plot_title a character string containing the sample name
#'
#' @importFrom rlang .data
#'
#' @export
make_VK_grad <- function(data, plot_title = "") {
  data <- data %>%
    dplyr::arrange(.data$rel_abund) %>%
    plot_VK_grad() %>%
    print()
}
