#' reorder_class_hetero
#'
#' This function orders heteroatom classes from O1 to Omax through CHNO_S1 to
#' CHNOS_max for use in summarizing and visualizing heteroatom class data. It
#' orders the factors based on a computed value, "level", which is discarded
#' after reordering. The levels increase in a nested fashion by number of O,
#' then number of N, and then number of S. The function is currently limited to
#' the default class of elements: C, H, N, O, S.
#'
#' @param df a tibble containing a table of the assigned elements with a column name
#' for each element (e.g., "C", "H", "N", "O" and "S")
#'
#' @importFrom rlang .data
#'
#' @return a vector the heteroatom classes as ordered factors
#' @export
reorder_class_hetero <- function(df) {
  max_N <- seq(1:(max(df$N) + 1)) - 1
  max_O <- seq(1:(max(df$O) + 1)) - 1
  max_S <- seq(1:(max(df$S) + 1)) - 1

  df$class_hetero <- factor(df$class_hetero)
  df$level <- NA

  n <- 1
  for (i in max_S) {
    for (j in max_N) {
      for (k in max_O) {
        df$level[df$N == j & df$O == k & df$S == i] <- n
        n = n + 1
      }
    }
  }
  df$class_hetero <- forcats::fct_reorder(df$class_hetero, df$level, min)
  df <- df %>%
    dplyr::select(-.data$level)
  return(df)
}