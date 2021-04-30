#' plot_Kroll_differential
#'
#' This function makes a Kroll diagram that plots average carbon oxidation
#' states against carbon number (Kroll et al. 2011), but shows the intensities
#' of the peak abundance intensities between 2 samples gradient color scale
#' (as a function of relative abundance).
#'
#' @param data a tibble containing the following column names: "HtoC"
#'   and "OtoC"
#' @param plot_title a character string containing the sample name (default =
#'   none)
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 aes element_text
#'
#' @export
plot_Kroll_differential <- function(data, plot_title = "") {

  # plotting greatest differences on top
  data <- data %>%
    dplyr::arrange(abs(.data$rel_abund_y_minus_x))

  # black to orange appears to be color-bind friendly
  # https://bconnelly.net/posts/creating_colorblind-friendly_figures/
  Kroll <- ggplot2::ggplot(data, aes(x = .data$C, y = .data$NOSC)) +
    ggplot2::geom_point(aes(color = .data$rel_abund_y_minus_x), size = 2, na.rm = TRUE, alpha = 0.6) +
    ggplot2::scale_color_gradient2(name = "\u0394 rel abund",
                                   low = "orange", mid = "white", high = "black") +
    ggthemes::theme_tufte(base_size = 14, base_family = "sans") +
    ggplot2::theme(plot.title = element_text(size = 16, face = "bold"),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 12),
                   axis.title = element_text(face = "bold")) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::labs(x = "C", y = "NOSC") +
    ggplot2::scale_y_continuous(limits = c(-2.5, 2.5), breaks = seq(-2, 2, by = 1))

  Kroll
}
