#' plot_VK_differential
#'
#' This function makes a traditional van Krevelen diagram (H/c vs. O/C), but
#' shows the intensities of the peak abundance intensities between 2 samples
#' gradient color scale (as a function of relative or abundance).
#'
#' The area above the horizontal line at H/C = 1.5 is designated as the
#' aliphatic region in accordance with D'Andrilli et al. 2015 and Lv et al.
#' 2017. The area below the diagonal line from H/C = 1.1 is designated as the
#' aromatic region based on a modified aromaticity index >= 0.5 in accordance
#' with Koch & Dittmar 2006.
#'
#' @param data a tibble containing the following column names: "HtoC"
#'   and "OtoC"
#' @param plot_title a character string containing the sample name (default =
#'   none)
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 aes element_text element_blank
#'
#' @export
plot_VK_differential <- function(data, plot_title = "") {

  # plotting greatest differences on top
  data <- data %>%
    dplyr::arrange(abs(.data$rel_abund_y_minus_x))

  # black to orange appears to be color-bind friendly
  # https://bconnelly.net/posts/creating_colorblind-friendly_figures/
  VK <- ggplot2::ggplot(data, aes(x = .data$OtoC, y = .data$HtoC)) +
    ggplot2::geom_point(aes(color = .data$rel_abund_y_minus_x), size = 2, na.rm = TRUE, alpha = 0.6) +
    ggplot2::scale_color_gradient2(name = "\u0394 rel abund",
                                   low = "orange", mid = "white", high = "black") +
    ggthemes::theme_tufte(base_size = 14, base_family = "sans") +
    ggplot2::theme(plot.title = element_text(size = 16, face = "bold"),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 12),
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
  VK
}
