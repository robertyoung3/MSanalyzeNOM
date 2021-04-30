#' plot_hetero_class_single
#'
#' This function creates a heteratom class bar chart for one sample. The total
#' number of heteroatom classes is typically very large, so it's useful to filter
#' the data to focus on the classes of interest first.
#'
#' @param data a tibble containing containing the following column names: "class_hetero"
#'   and "perc_abund"
#' @param plot_title a character string containing the sample name
#' @param bar_color a character string specifying the bar color
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 aes element_text element_blank
#'
#' @export
plot_hetero_class_single <- function(data, plot_title = "", bar_color = "dark green") {
  data <- tibble::as_tibble(data)
  data$class_hetero <- forcats::fct_drop(data$class_hetero)

  bar_plot <- ggplot2::ggplot(data,
                              aes(x = .data$class_hetero, y = .data$perc_abund)) +
    ggplot2::geom_col(fill = bar_color) +
    ggthemes::theme_tufte(base_size = 14, base_family = "sans") +
    ggplot2::theme(plot.title = element_text(size = 16, face = "bold"),
                   axis.title.x = element_blank(),
                   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                   axis.title.y = element_text(face = "bold"),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 12)) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::labs(y = "% relative abundance")
  bar_plot
}