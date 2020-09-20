#' plot_VK_gradient
#'
#' This function makes a traditional van Krevelen diagram (H/c vs. O/C). This
#' plot shows the distribution of the assigned formulas as a function of
#' elemental ratios, and shows the intensities of the detected ions on a
#' gradient color scale (typically as a function of relative or percent
#' abundance).
#'
#' The area above the horizontal line at H/C = 1.5 is designated as the
#' aliphatic region in accordance with D'Andrilli et al. 2015 and Lv et al.
#' 2017. The area below the diagonal line from H/C = 1.1 is designated as the
#' aromatic region based on a modified aromaticity index >= 0.5 in accordance
#' with Koch & Dittmar 2006.
#'
#' @param data a tibble containing the following column names: "HtoC"
#'   and "OtoC"
#' @param var a character string containing the continuous variable column name
#'   used to establish the color gradient (default = "rel_abund")
#' @param plot_title a character string containing the sample name (default =
#'   none)
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
plot_VK_gradient <- function(data, var = "rel_abund", plot_title = "", panel = FALSE,
                             var_panel, num_col = 2) {
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
  VK <- ggplot2::ggplot(data, aes(x = .data$OtoC, y = .data$HtoC)) +
    ggplot2::geom_point(aes(color = .data[[var]]), size = 1, na.rm = TRUE, alpha = 0.8) +
    ggplot2::scale_color_gradient(name = label,
                                  low = "#F7FCF0", high = "#084081") +
    ggthemes::theme_tufte(base_size = 14, base_family = "sans") +
    ggplot2::theme(plot.title = element_text(size = 16, face = "bold"),
                   legend.title = element_text(size = 12, face = "bold"),
                   legend.text = element_text(size = 12),
                   strip.text = element_text(face = "bold"),
                   axis.title = element_text(face = "bold")) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::labs(x = "O/C", y = "H/C") +
    ggplot2::scale_x_continuous(limits = c(0, 1.4), breaks = seq(0.0, 1.2, by = 0.3)) +
    ggplot2::scale_y_continuous(limits = c(0, 2.5), breaks = seq(0.0, 2.5, by = 0.5)) +
    ggplot2::geom_hline(yintercept = 1.5) +
    ggplot2::geom_abline(intercept = 1.1, slope = -0.44) +
    # the following two annotations have to be separated to work with faceting
    ggplot2:: annotate("text", x = 1.3, y = 1.6, label = "ALIPH",
                       size = 4, vjust = "outward") +
    ggplot2:: annotate("text", x = 1.3, y = 0.42, label = "AROM",
                       size = 4, vjust = "outward")

  # implement panel
  if (panel == TRUE) {
    VK + ggplot2::facet_wrap(~ .data[[var_panel]], ncol = num_col)
  } else {
    VK
  }
}
