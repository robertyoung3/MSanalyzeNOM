#' plot_VK_flat
#'
#' This function makes a traditional van Krevelen diagram (H/c vs. O/C). This
#' plot shows the distribution of the assigned formulas as a function of
#' elemental ratios, but contains no information about the intensities of the
#' detected ions. The darker areas are where the highest number of formulas are
#' plotted.
#'
#' The area above the horizontal line at H/C = 1.5 is designated as the
#' aliphatic region in accordance with D'Andrilli et al. 2015 and Lv et al.
#' 2017. The area below the diagonal line from H/C = 1.1 is designated as the
#' aromatic region based on a modified aromaticity index >= 0.5 in accordance
#' with Koch & Dittmar 2006.
#'
#' @param data a tibble containing the following column names: "HtoC"
#'   and "OtoC"
#' @param plot_title a character string containing the sample name
#' @param panel a logical value specifying whether a panel will be used (default
#'   = FALSE)
#' @param var_panel a character string containing the column name specifying the
#'   factor variable to be used for faceting
#' @param num_col an integer specifying the number of panel columns (default =
#'   2)
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 aes element_text element_blank
#'
#' @export
plot_VK_flat <- function(data, plot_title = "", panel = FALSE,
                         var_panel, num_col = 2) {
  VK <- ggplot2::ggplot(data, aes(x = .data$OtoC, y = .data$HtoC)) +
    ggplot2::geom_point(size = 1, na.rm = TRUE, alpha = 0.1) +
    ggthemes::theme_tufte(base_size = 14, base_family = "sans") +
    ggplot2::theme(plot.title = element_text(size = 16, face = "bold"),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   strip.text = element_text(size = 14, face = "bold"),
                   axis.title = element_text(face = "bold")) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::labs(x = "O/C", y = "H/C") +
    ggplot2::scale_x_continuous(limits = c(0, 1.5), breaks = seq(0.0, 1.2, by = 0.3)) +
    ggplot2::scale_y_continuous(limits = c(0, 2.5), breaks = seq(0.0, 2.5, by = 0.5)) +
    ggplot2::geom_hline(yintercept = 1.5) +
    ggplot2::geom_abline(intercept = 1.1, slope = -0.44) +
    ggplot2:: annotate("text", x = 1.4, y = 1.6, label = "MOSTLY\nALIPH",
                       size = 3.5, vjust = "outward") +
    ggplot2:: annotate("text", x = 1.4, y = 0.37, label = "MOSTLY\nAROM",
                       size = 3.5, vjust = "outward")

  # implement panel
  if (panel == TRUE) {
    VK + ggplot2::facet_wrap(~ .data[[var_panel]], ncol = num_col)
  } else {
    VK
  }
}
