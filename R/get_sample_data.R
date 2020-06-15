#' get_sample_data
#'
#' This function reads sample Excel files exported by the MagLab's PetroOrg
#' software after molecular formula (i.e., elemental composition) assignment,
#' and returns a named list containing file and sample information, selected
#' mass spectrometry parameters, mass spectrometry data for the assigned
#' molecular formulas, mass spectrometry data for detected ions that could not
#' be assigned molecular formulas ("no hits"), and a table of all detected ions.
#'
#' This function was built for MS data from Fourier transform ion cyclotron
#' resonance (FTICR) mass spectrometry using electrospray ionization in negative
#' ion mode (NegESI). This is the default ion type. Because natural organic
#' matter (NOM) is generally comprised primarily of the elements C, H, N, O, and
#' S, they comprise the default elements used for molecular formula assignment.
#'
#' If the form of the files exported by PetroOrg changes, this function will
#' need revisions.
#'
#' @param file a character string containing the filename and path for the Excel
#'   file created by PetroOrg&copy after molecular formula assignment
#' @param ion_technique a character string containing the MS ionization
#'   technique used (default = NegESI)
#' @param element_list a character vector containing the elements that were used
#'   for molecular formula assignments (default = C, H, N, O, S)
#'
#' @importFrom rlang .data
#'
#' @return sample_data a named list containing 8 elements
#' @export
get_sample_data <- function(file = file.choose(), ion_technique = "NegESI",
                            element_list = c("C", "H", "N", "O", "S")) {
  sample_data <- list(sample_name = tools::file_path_sans_ext(basename(file)),
                      file_name = basename(file),
                      orig_file_path = file,
                      ion_technique = ion_technique,
                      element_list = element_list)

  ## (1) read no hits data
  sample_data$no_hits <- file %>%
    readxl::read_excel(sheet = "No Hit", col_names = FALSE, skip = 2) %>%
    dplyr::select(2, 4)
  colnames(sample_data$no_hits) <- c("mz", "rel_abund")

  ## (2) read assigned formula data
  #### (a) get sheet names
  sheetnames <- readxl::excel_sheets(file)

  #### (b) subset to exclude isotopes and sheetnames with 3 or more consecutive letters (data summaries)
  sheetnames <- sheetnames[stringr::str_detect(sheetnames, "13C|34S|[[:alpha:]]{3,}", negate = TRUE)]

  #### (c) create named vector for mapping
  names(sheetnames) <- trimws(sheetnames)

  #### (d) read and clean data by mapping through sheet names
  temp <- sheetnames %>%
    purrr::map_dfr(~readxl::read_excel(path = file, sheet = ., col_names = TRUE, skip = 2), .id = "class_hetero") %>%
    janitor::clean_names() %>%
    dplyr::select(-tidyselect::one_of("x1", "exp_m_z", "signal2noise", "molecular_formula"),
                  -tidyselect::ends_with("_c")) %>%
    dplyr::rename(mz = .data$recal_m_z,
                  theor_mz = .data$theor_mass,
                  ppm_error = .data$error,
                  rel_abund = .data$rel_abundance,
                  DBE = .data$dbe)

  #### (e) convert class_hetero from characters to factors
  temp$class_hetero <- factor(temp$class_hetero)

  #### (f) rename element columns with first element in prior column
  for (i in element_list) {
    colnames(temp)[which(temp[1,] == i) + 1] <- i
  }

  #### (g) discard unnamed columns (e.g., "x10")
  temp <- temp %>%
    dplyr::select(-tidyselect::matches("x[[:digit:]]+"))

  #### (h) generate molecular formulas & strip empty elements
  temp$chem_formula <- ""
  for (i in element_list) {
    temp$chem_formula <- temp$chem_formula %>%
      stringr::str_c(i, temp[[i]]) %>%
      stringr::str_replace(stringr::str_c(i, "0"), "")
  }

  #### (i) reorder columns
  temp <- temp %>%
    dplyr::select(.data$class_hetero, .data$chem_formula, tidyselect::everything())

  #### (j) assign temp to sample_data
  sample_data$assigned_formulas <- temp

  ## (3) generate table of all detected ions
  sample_data$all_detected_ions <- get_all_detected_ions(sample_data$no_hits,
                                                         sample_data$assigned_formulas)

  ## (4)return named list
  return(sample_data)
}
