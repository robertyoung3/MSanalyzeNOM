#' plot_Kroll_flat
#'
#' This function makes a Kroll diagram that plots average carbon oxidation
#' states against carbon number (Kroll et al. 2011). It contains no information
#' about the intensities of the detected ions. The darker areas are where the
#' highest number of formulas are plotted.
#'
#' @param data a tibble containing the following column names: "NOSC" (from
#'   the computeNOSC() function) and "C"
#' @param plot_title a character string containing the sample name
#' @param panel a logical value specifying whether a panel will be used (default
#'   = FALSE)
#' @param var_panel a character string containing the column name specifying the
#'   factor variable to be used for faceting
#' @param num_col an integer specifying the number of panel columns (default =
#'   2)
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 aes element_text
#'
#' @export
plot_Kroll_flat <- function(data, plot_title = "", panel = FALSE,
                            var_panel, num_col = 2) {
  Kroll <- ggplot2::ggplot(data, aes(x = .data$C, y = .data$NOSC)) +
    ggplot2::geom_point(size = 1, na.rm = TRUE, alpha = 0.1) +
    ggthemes::theme_tufte(base_size = 14, base_family = "sans") +
    ggplot2::theme(plot.title = element_text(size = 16, face = "bold"),
                   legend.title = element_text(size = 12, face = "bold"),
                   legend.text = element_text(size = 12),
                   strip.text = element_text(size = 14, face = "bold"),
                   axis.title = element_text(face = "bold")) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::labs(x = "C", y = "NOSC") +
    ggplot2::scale_y_continuous(limits = c(-2.5, 2.5), breaks = seq(-2, 2, by = 1))
#    ggplot2::annotate("text", x = 5, y = 2.5, label = "CO[2] == 4", parse = TRUE,
#                      family = "serif", size = 4, hjust = "inward") +
#    ggplot2::annotate("text", x = 5, y = -2.5, label = "CH[4] == -4", parse = TRUE,
#                      family = "serif", size = 4, hjust = "inward")

  # implement panel
  if (panel == TRUE) {
    Kroll + ggplot2::facet_wrap(~ .data[[var_panel]], ncol = num_col)
  } else {
    Kroll
  }
}
