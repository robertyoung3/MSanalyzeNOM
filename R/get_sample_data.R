#' get_sample_data
#'
#' This function reads sample Excel files exported by the MagLab's PetroOrg
#' software after molecular formula (i.e., elemental composition) assignment,
#' and returns a named list containing file and sample information, selected
#' mass spectrometry parameters, mass spectrometry data for the assigned
#' molecular formulas, and mass spectrometry data for detected ions that could
#' not be assigned molecular formulas ("no hits").
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
#' @param target_file a character string containing the filename and path for
#'   the Excel file created by PetroOrg&copy after molecular formula assignment
#' @param ion_technique a character string containing the MS ionization
#'   technique used (default = NegESI)
#' @param element_list a character vector containing the elements that were used
#'   for molecular formula assignments (default = C, H, N, O, S)
#'
#' @return sample_data a named list containing file and sample information, MS
#'   parameter information, a tibble containing the MS data for all assigned
#'   elemental compositions, and a tibble containing the MS data for all
#'   detected ions that could not be assigned formulas ("no hits")
#' @export
get_sample_data <- function(target_file = file.choose(), ion_technique = "NegESI",
                            element_list = c("C", "H", "N", "O", "S")) {
  sample_data <- list(sample_name = tools::file_path_sans_ext(basename(target_file)),
                      file_name = basename(target_file),
                      orig_file_path = target_file,
                      ion_technique = ion_technique,
                      element_list = element_list)

  ## (1) read no hits data
  sample_data[["no_hits"]] <- target_file %>%
    readxl::read_excel(sheet = "No Hit", col_names = FALSE, skip = 2) %>%
    dplyr::select(2, 4)
  colnames(sample_data[["no_hits"]]) <- c("mz", "rel_abund")

  ## (2) read assigned formula data
  #### (a) get sheet names
  sheetnames <- readxl::excel_sheets(target_file)

  #### (b) subset to exclude isotopes and sheetnames with 3 or more consecutive letters (data summaries)
  sheetnames <- sheetnames[stringr::str_detect(sheetnames, "13C|34S|[[:alpha:]]{3,}", negate = TRUE)]

  #### (c) create named vector for mapping
  names(sheetnames) <- trimws(sheetnames)

  #### (d) read and clean data by mapping through sheet names
  temp <- sheetnames %>%
    purrr::map_dfr(~readxl::read_excel(path = target_file, sheet = ., col_names = TRUE, skip = 2), .id = "hetero_class") %>%
    janitor::clean_names() %>%
    dplyr::select(-tidyselect::one_of("x1", "exp_m_z", "signal2noise", "dbe", "molecular_formula"),
                  -tidyselect::ends_with("_c")) %>%
    dplyr::rename(mz = recal_m_z,
                  theor_mz = theor_mass,
                  ppm_error = error,
                  rel_abund = rel_abundance)

  #### (e) rename element columns with first element in prior column
  for (i in element_list) {
    colnames(temp)[which(temp[1,] == i) + 1] <- i
  }

  #### (f) discard unnamed columns (e.g., "x10")
  temp <- temp %>%
    dplyr::select(-tidyselect::matches("x[[:digit:]]+"))

  #### (g) generate molecular formulas & strip empty elements
  temp[["chem_formula"]] <- ""
  for (i in element_list) {
    temp[["chem_formula"]] <- temp[["chem_formula"]] %>%
      stringr::str_c(i, temp[[i]]) %>%
      stringr::str_replace(stringr::str_c(i, "0"), "")
  }

  #### (h) reorder columns
  temp <- temp %>%
    dplyr::select(hetero_class, chem_formula, tidyselect::everything())

  #### (i) assign temp to sample_data
  sample_data[["assigned_formulas"]] <- temp

  ## (3)return named list
  return(sample_data)
}
