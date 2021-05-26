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
#' need to be revised.
#'
#' @param file a character string containing the filename and path for the Excel
#'   file created by PetroOrg after molecular formula assignment
#' @param ion_technique a character string containing the MS ionization
#'   technique used (default = NegESI)
#' @param elements_used a character vector containing the elements that were used
#'   for molecular formula assignments (default = C, H, N, O, S)
#'
#' @importFrom rlang .data
#'
#' @return sample_data a named list containing 8 elements
#' @export
get_sample_data <- function(file = file.choose(), ion_technique = "NegESI",
                            elements_used = c("C", "H", "N", "O", "S")) {
  sample_data <- list(sample_name = tools::file_path_sans_ext(basename(file)),
                      file_name = basename(file),
                      orig_file_path = file,
                      ion_technique = ion_technique,
                      elements_used = elements_used)

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

  #### (e) rename element columns with first element in prior column
  for (i in elements_used) {
    colnames(temp)[which(temp[1,] == i) + 1] <- i
  }

  #### (f) discard unnamed columns (e.g., "x10")
  temp <- temp %>%
    dplyr::select(-tidyselect::matches("x[[:digit:]]+"))

  #### (g) generate molecular formulas & strip empty elements
  temp$chem_formula <- ""
  for (i in elements_used) {
    temp$chem_formula <- temp$chem_formula %>%
      stringr::str_c(i, temp[[i]]) %>%
      stringr::str_replace(stringr::str_c(i, "0"), "")
  }

  #### (h) convert class_hetero from characters to factors
  temp <- reorder_class_hetero(temp)

  #### (i) compute neutral mass
  ## PosESI (with adducts) and APPI (with radicals) are reserved
  mass_proton <- 1.00727647
  if (sample_data$ion_technique == "NegESI") {
    temp$theor_mass <- temp$theor_mz + mass_proton
  }

  #### (j) compute nominal mz
  temp <- temp %>%
    compute_nominal_mz()

  #### (k) normalize relative abundances
  temp <- temp %>%
    compute_100norm_rel_abund()

  #### (l) compute percent abundances
  temp <- temp %>%
    compute_perc_abund()

  #### (m) reorder columns
  temp <- temp %>%
    dplyr::select(.data$class_hetero, .data$chem_formula, .data$theor_mass, .data$nominal_mz, tidyselect::everything())

  #### (n) get_CHNOS_element_class
  temp <- temp %>%
    get_CHNOS_element_class()

  #### (o) compute DBE
  temp <- temp %>%
    compute_DBE()

  #### (p) compute DBE to C
  temp <- temp %>%
    compute_DBEtoC()

  #### (q) compute modified aromaticity index
  temp <- temp %>%
    compute_arom_index(AItype = "ModAI")

  #### (r) compute most used elemental ratios
  temp <- temp %>%
    compute_elemental_ratios(num = "H") %>%
    compute_elemental_ratios(num = "O") %>%
    compute_elemental_ratios(num = "N")

  #### (s) get 25% groups for plotting
  temp <- temp %>%
    get_25perc_groups()

  #### (t) compute KMD_CH2 and z_CH2
  temp <- temp %>%
    compute_KMD_CH2()

  #### (u) compute NOSC
  temp <- temp %>%
    compute_NOSC()

  #### (v) compute mass defect
  temp <- temp %>%
    compute_mass_defect()

  #### (w) order formulas by theor_mass
  temp <- temp %>%
    dplyr::arrange(.data$theor_mass)

  #### (x) assign temp to sample_data
  sample_data$assigned_formulas <- temp


  ## (3) generate table of all detected ions
  sample_data$all_detected_ions <- get_all_detected_ions(sample_data$no_hits,
                                                         sample_data$assigned_formulas)

  sample_data$all_detected_ions <-  sample_data$all_detected_ions %>%
    compute_nominal_mz()

  ## (4)return named list
  return(sample_data)
}
