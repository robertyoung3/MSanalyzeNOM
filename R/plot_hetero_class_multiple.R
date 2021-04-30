#' plot_hetero_class_multiple
#'
#' This function creates a heteroatom class bar plot for multiple samples. The total
#' number of heteroatom classes is typically very large, so it's useful to filter
#' the data to focus on the classes of interest first.
#'
#' @param data a tibble containing containing the following column names: "class_hetero",
#'   "perc_abund", and "sample_name"
#' @param plot_title a character string containing the sample name
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 aes element_text element_blank
#'
#' @export
plot_hetero_class_multiple <- function(data, plot_title = "") {
  data <- tibble::as_tibble(data)
  data$class_hetero <- forcats::fct_drop(data$class_hetero)

  bar_plot <- ggplot2::ggplot(data, aes(x = .data$class_hetero, y = .data$perc_abund)) +
    ggplot2::geom_col(aes(fill = .data$sample_name),
                      position = ggplot2::position_dodge2(preserve = "single", padding = 0)) +
    ggsci::scale_fill_aaas() +
    ggthemes::theme_tufte(base_size = 14, base_family = "sans") +
    ggplot2::theme(plot.title = element_text(size = 16, face = "bold"),
                   axis.title.x = element_blank(),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                   axis.title.y = element_text(face = "bold"),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "bottom") +
    ggplot2::ggtitle(plot_title) +
    ggplot2::labs(y = "% relative abundance")
  bar_plot
}