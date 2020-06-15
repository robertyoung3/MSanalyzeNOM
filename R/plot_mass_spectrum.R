#' plot_mass_spectrum
#'
#' This function plots the sample mass spectrum.The maximum x value can be
#' specified to produce figures for multiple samples on the same scale.
#'
#' @param data a tibble containing the assigned molecular formulas
#' @param plot_title a character string containing the sample name
#' @param max_x a numeric containing the maximum x value to plot (default =
#'   NULL)
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 aes element_text element_blank
#'
#' @return
#' @export
plot_mass_spectrum <- function(data, plot_title, max_x = NULL) {
  if (is.null(max_x)) {
    x_ticks <- pretty(data$mz)
  } else {
    x_ticks <- pretty(100:max_x)
  }
  ggplot2::ggplot(data, aes(x = .data$mz,
                            ymin = 0, ymax = .data$rel_abund)) +
    ggplot2::geom_linerange(size = 0.1) +
    ggthemes::theme_tufte(base_size = 18, base_family = "sans") +
    ggplot2::theme(plot.title = element_text(size = 16, face = "bold"),
                   axis.title = element_text(face = "bold")) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::labs(x = "m/z", y = "rel. abund.") +
    ggplot2::scale_x_continuous(limits = range(x_ticks),
                                breaks = x_ticks)
}

