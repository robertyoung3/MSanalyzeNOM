#' plot_mass_spectrum
#'
#' This function plots the sample mass spectrum.The minimum and maximum mz
#' values can be specified to plot a specific region of the mass spectrum.
#'
#' @param data a tibble containing the assigned molecular formulas
#' @param plot_title a character string containing the sample name
#' @param min_x a numeric containing the minimum mz value to plot (default = NULL)
#' @param max_x a numeric containing the maximum mz value to plot (default = NULL)
#'
#' @importFrom rlang .data
#' @export
plot_mass_spectrum <- function(data, plot_title, min_x = NULL, max_x = NULL) {
  if (!is.null(min_x) & !is.null(max_x)) {
    data <- data %>%
      dplyr::filter(.data$mz >= min_x & .data$mz <= max_x)
  } else if (is.null(min_x) & !is.null(max_x)) {
    data <- data %>%
      dplyr::filter(.data$mz <= max_x)
  } else if (!is.null(min_x) & is.null(max_x)) {
    data <- data %>%
      dplyr::filter(.data$mz >= min_x)
  }

  ggplot2::ggplot(data, aes(x = .data$mz,
                            ymin = 0, ymax = .data$rel_abund)) +
    ggplot2::geom_linerange(size = 0.1) +
    ggthemes::theme_tufte(base_size = 18, base_family = "sans") +
    ggplot2::theme(plot.title = element_text(size = 16, face = "bold"),
                   axis.title = element_text(face = "bold")) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::labs(x = "m/z", y = "rel. abund.")
}
