#' get_elemental_ratios
#'
#' @param sample_df a tibble containing the MS data for all assigned elemental compositions
#' @param num character string for the element in the numerator (default = "H")
#' @param denom character string for the element in the denominator (default = "C")
#'
#' @return sample_df with a column for the computed elemental ratio
#' @export
#'
#' @examples
#' sample_df <- get_elemental_ratios(DOM_formulas_abbrev)
#' head(sample_df)
get_elemental_ratios <- function(sample_df, num = "H", denom = "C") {
 label <- stringr::str_c(num, "to", denom)
 sample_df[[label]] <- sample_df[[num]] / sample_df[[denom]]
 return(sample_df)
}
