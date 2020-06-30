#' compute_elemental_ratios
#'
#' This function computes elemental ratios from user-specified elements.
#'
#' @param data a tibble containing column names for the desired elements (e.g.,
#'   "H" or "C")
#' @param num character string for the element in the numerator (default = "H")
#' @param denom character string for the element in the denominator (default =
#'   "C")
#'
#' @return data with a new column containing the computed elemental ratio
#' @export
compute_elemental_ratios <- function(data, num = "H", denom = "C") {
  label <- stringr::str_c(num, "to", denom)
  data[[label]] <- data[[num]] / data[[denom]]
  return(data)
}
