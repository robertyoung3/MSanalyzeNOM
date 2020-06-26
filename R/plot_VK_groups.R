#' plot_VK_groups
#'
#' This function makes a traditional van Krevelen diagram (H/c vs. O/C). This
#' plot shows the distribution of the assigned formulas as a function of
#' elemental ratios, and shows the intensities of the detected ions by their
#' percent contribution to total assigned abundance.
#'
#' The area above the horizontal line at H/C = 1.5 is designated as the
#' aliphatic region in accordance with D'Andrilli et al. 2015 and Lv et al.
#' 2017. The area below the diagonal line from H/C = 1.1 is designated as the
#' aromatic region based on a modified aromaticity index >= 0.5 in accordance
#' with Koch & Dittmar 2006.
#'
#' @param data a tibble containing the assigned molecular formulas
#' @param var a character string containing the group factor column that will be
#'   used to provide color (default = group_25perc)
#' @param plot_title a character string containing the sample name
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 aes element_text element_blank
#'
#' @export
plot_VK_groups <- function(data, var = "group_25perc", plot_title = "") {
  ggplot2::ggplot(data, aes(x = .data$OtoC, y = .data$HtoC)) +
    ggplot2::geom_point(aes(color = .data[[var]]), size = 1, na.rm = TRUE, alpha = 0.8) +
    # Green-Blue color scheme should work for color-blind people
    # https://venngage.com/blog/color-blind-friendly-palette/
    ggplot2::scale_color_brewer(palette = "GnBu", direction = -1) +
    ggthemes::theme_tufte(base_size = 14, base_family = "sans") +
    ggplot2::theme(plot.title = element_text(size = 16, face = "bold"),
                   axis.title = element_text(face = "bold"),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12)) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4))) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::labs(x = "O/C", y = "H/C") +
    ggplot2::scale_x_continuous(limits = c(0, 1.4), breaks = seq(0.0, 1.2, by = 0.3)) +
    ggplot2::scale_y_continuous(limits = c(0, 2.5), breaks = seq(0.0, 2.5, by = 0.5)) +
    ggplot2::geom_hline(yintercept = 1.5) +
    ggplot2::geom_abline(intercept = 1.1, slope = -0.44) +
    ggplot2:: annotate("text", x = 1.3, y = c(1.6, 0.37), label = c("ALIPH", "AROM"),
                       fontface = 2, size = 4)
}
