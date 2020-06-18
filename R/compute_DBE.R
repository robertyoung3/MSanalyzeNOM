#' compute_DBE
#'
#' This function calculates double bond equivalents using the formula:
#'
#' DBE= 1 + 0.5(sum [N_i(V_i - 2)])
#'
#' where N_i is the element number and V_i is the valence for each element i.
#'
#' @param df a tibble containing the assigned molecular formulas
#' @param elements a character vector of the elements used for formula assignment
#'
#' @importFrom rlang .data
#'
#' @return df with a column containing DBE values
#' @export
compute_DBE <- function(df, elements) {
  #create a table of elements, ordered by elements
  temp <- df %>%
    dplyr::select(elements)

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
  # round down in case DBE contains decimal value (e.g., 4.5)
  temp$DBE <- floor(1 + (temp$DBE / 2))

  # add DBE to df and return
  # keeping original to compare against PetroOrg for a few samples
  if ("DBE" %in% colnames(df)) {
    df$RDBE <- temp$DBE
  } else {
    df$DBE <- temp$DBE
  }

  return(df)
}