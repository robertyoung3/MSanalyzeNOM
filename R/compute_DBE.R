#' compute_DBE
#'
#' This function calculates double bond equivalents using the formula:
#'
#' DBE= 1 + 0.5(sum [N_i(V_i - 2)])
#'
#' where N_i is the element number and V_i is the valence for each element i.
#'
#' @param df a tibble containing a table of the assigned elements with a column name
#' for each element (e.g., "C", "H", "N", "O" and "S")
#' @param elements a character vector of the elements used for formula assignment (default = CHNOS)
#'
#' @importFrom rlang .data
#'
#' @return df with a column containing DBE values
#' @export
compute_DBE <- function(df, elements = c("C", "H", "N", "O", "S")) {
  #create a table of elements, ordered by elements
  temp <- df %>%
    dplyr::select(elements)

  formula_elements <- c("C", "H", "N", "O", "P", "S")

  # identify elements that are not in formula elements
  # print error message
  for (i in 1:length(elements)) {
    if (!elements[[i]] %in% formula_elements) {
      message("The DBE calculation is currently limited to CHNOPS elements.")
    }
  }

  # extract valences from elemental_data for elements
  # then order by elements (same as temp)
  valences <- elemental_data %>%
    dplyr::filter(.data$element %in% elements) %>%
    dplyr::select(.data$element, .data$valence) %>%
    dplyr::distinct(.data$element, .keep_all = TRUE) %>%
    tidyr::pivot_wider(names_from = .data$element,
                       values_from = .data$valence) %>%
    dplyr::select(elements)

  # compute DBE in 2 steps
  temp$DBE <- 0
  # step 1: sum element number * (valence - 2) for each element
  for (i in 1:length(valences)) {
    temp$DBE <- temp$DBE + temp[[i]] * (valences[[i]] - 2)
  }

  # step 2: use sum in complete calculation (1 + sum/2)
  temp$DBE <- 1 + (temp$DBE / 2)

  # add DBE to df and return
  df$DBE <- temp$DBE
  return(df)
}