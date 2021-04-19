#' plot_Kroll_gradient
#'
#' This function makes a Kroll diagram that plots average carbon oxidation
#' states against carbon number (Kroll et al. 2011). It also shows the
#' intensities of the detected ions on a gradient color scale (typically as a
#' function of relative or percent abundance).
#'
#' @param data a tibble containing the following column names: "NOSC" (from
#'   the computeNOSC() function) and "C"
#' @param var a character string containing the continuous variable column name
#'   used to establish the color gradient (default = "rel_abund")
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
plot_Kroll_gradient <- function(data, var = "rel_abund", plot_title = "",
                                panel = FALSE, var_panel, num_col = 2) {
  # plotting highest values on top
  data <- data %>%
    dplyr::arrange(.data[[var]])

  # the following code cleans up the label for expected continuous variables,
  # and is intended to be expanded when necessary
  label <- dplyr::case_when(
    var == "rel_abund" ~ "rel. abund.",
    var == "perc_abund" ~ "perc. abund.",
    TRUE ~ var
  )

  # RColorBrewer::brewer.pal(n = 9, name = "GnBu")
  # "#F7FCF0" "#E0F3DB" "#CCEBC5" "#A8DDB5" "#7BCCC4" "#4EB3D3" "#2B8CBE" "#0868AC" "#084081"
  Kroll <- ggplot2::ggplot(data, aes(x = .data$C, y = .data$NOSC)) +
    ggplot2::geom_point(aes(color = .data[[var]]), size = 2, na.rm = TRUE, alpha = 0.8) +
    ggplot2::scale_color_gradient(name = label,
                                  low = "#F7FCF0", high = "#084081") +
    ggthemes::theme_tufte(base_size = 14, base_family = "sans") +
    ggplot2::theme(plot.title = element_text(size = 16, face = "bold"),
                   legend.title = element_text(size = 12, face = "bold"),
                   legend.text = element_text(size = 12),
                   strip.text = element_text(face = "bold"),
                   axis.title = element_text(face = "bold")) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::labs(x = "C", y = "NOSC") +
    ggplot2::scale_y_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by = 1)) +
    ggplot2::annotate("text", x = 5, y = 2.5, label = "CO[2] == 4", parse = TRUE,
                      family = "serif", size = 4, hjust = "inward") +
    ggplot2::annotate("text", x = 5, y = -2.5, label = "CH[4] == -4", parse = TRUE,
                      family = "serif", size = 4, hjust = "inward")

  # implement panel
  if (panel == TRUE) {
    Kroll + ggplot2::facet_wrap(~ .data[[var_panel]], ncol = num_col)
  } else {
    Kroll
  }
}
