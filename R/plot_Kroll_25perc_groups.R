#' plot_Kroll_25perc_groups
#'
#' This function makes a Kroll diagram that plots average carbon oxidation
#' states against carbon number (Kroll et al. 2011). It also shows the
#' intensities of the detected ions by their percent contribution to total
#' assigned abundance.
#'
#' @param data a tibble containing the following column names: "AvgOSC" (from
#'   the computeAvgOSC() function), "C", "group_25perc" (from the
#'   get_25perc_groups() function), and "rel_abund"
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
plot_Kroll_25perc_groups <- function(data, plot_title = "", panel = FALSE,
                                     var_panel, num_col = 2) {
  # plotting highest values on top
  data <- data %>%
    dplyr::arrange(.data$rel_abund)

  # RColorBrewer::display.brewer.pal(n = 9, name = "GnBu")
  # RColorBrewer::brewer.pal(n = 9, name = "GnBu")
  # assign colors to levels
  blueGreenPalette <- c("#084081", "#4EB3D3", "#A8DDB5", "#E0F3DB")
  names(blueGreenPalette) <- levels(.data$group_25perc)

  if (max(.data$c) < 40) {
    xlim <- 40
  } else {xlim <- max(.data$C) / 8}

  #create plot
  Kroll <- ggplot2::ggplot(data, aes(x = .data$C, y = .data$AvgOSC)) +
    ggplot2::geom_point(aes(color = .data$group_25perc), size = 2, na.rm = TRUE, alpha = 0.8) +
    ggplot2::scale_color_manual(name = "Ranked by\n% Abund.", values = blueGreenPalette) +
    ggthemes::theme_tufte(base_size = 14, base_family = "sans") +
    ggplot2::theme(plot.title = element_text(size = 16, face = "bold"),
                   legend.title = element_text(size = 12, face = "bold"),
                   legend.text = element_text(size = 12),
                   strip.text = element_text(face = "bold"),
                   axis.title = element_text(face = "bold")) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4))) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::labs(x = "C", y = "AvgOSC") +
    ggplot2::scale_y_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by = 1)) +
    ggplot2::annotate("text", x = xlim, y = 2.5, label = "CO[2] == 4", parse = TRUE,
             fontface = 2, size = 4) +
    ggplot2::annotate("text", x = xlim, y = -2.5, label = "CH[4] == -4", parse = TRUE,
             fontface = 2, size = 4)

  # implement panel
  if (panel == TRUE) {
    Kroll + ggplot2::facet_wrap(~ .data[[var_panel]], ncol = num_col)
  } else {
    Kroll
  }
}
