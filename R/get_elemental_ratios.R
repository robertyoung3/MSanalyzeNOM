#' get_elemental_ratios
#'
#' @param data a tibble containing the MS data for all assigned elemental compositions
#' @param num character string for the element in the numerator (default = "H")
#' @param denom character string for the element in the denominator (default = "C")
#'
#' @return data with a new column containing the computed elemental ratio
#' @export
#'
#' @examples
#' df <- get_elemental_ratios(DOM_formulas_abbrev)
#' head(df)
get_elemental_ratios <- function(data, num = "H", denom = "C") {
  label <- stringr::str_c(num, "to", denom)
  data[[label]] <- data[[num]] / data[[denom]]
  return(data)
}
