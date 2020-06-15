#' get_CHNOS_element_class
#'
#' This function groups assigned formulas into element classes (e.g., CHO or
#' CHNOS) without regard to specific element numbers. It is intended to be used
#' for grouping in plotting, and has been written for the default element list
#' (CHNOS).
#'
#' @param data a tibble containing the assigned molecular formulas
#'
#' @importFrom rlang .data
#'
#' @return data with a new column containing the element classes
#' @export
get_CHNOS_element_class <- function(data) {
  data %>%
    dplyr::mutate(
      class_element = dplyr::case_when(
        .data$O >= 1 & .data$N == 0 & .data$S == 0 ~ "CHO",
        .data$O >= 1 & .data$N >= 1 & .data$S == 0 ~ "CHNO",
        .data$O >= 1 & .data$N == 0 & .data$S >= 1 ~ "CHOS",
        .data$O >= 1 & .data$N >= 0 & .data$S >= 0 ~ "CHNOS",
        .data$O == 0 & .data$N >= 0 & .data$S == 0 ~ "CHN",
        .data$O == 0 & .data$N == 0 & .data$S >= 0 ~ "CHS",
        .data$O == 0 & .data$N >= 0 & .data$S >= 0 ~ "CHNS",
        TRUE ~ "HC")) %>%
    dplyr::mutate(class_element = factor(.data$class_element)) %>%
    dplyr::select(.data$class_element, tidyselect::everything())
}
