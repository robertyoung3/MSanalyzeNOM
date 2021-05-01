# MSanalyzeNOM
Utilities for Analyzing Complex Natural Organic Matter (NOM) Mixtures After Formula Assignment

### Description

The `MSanalyzeNOM` package contains functions for analyzing the mass spectrometry (MS) data of natural organic matter (NOM) samples after formula assignment. It was specifically created to analyze MS data from NOM samples analyzed by Fourier transform ion cyclotron resonance (FTICR) MS at the [National High Magnetic Field Laboratory](https://nationalmaglab.org/user-facilities/icr) (MagLab) in Tallahassee, Florida, USA. 

The `get_sample_data()` function reads Excel files exported by PetroOrg&copy;, software that was created at the MagLab for mass calibration, molecular formula assignment, visualization, and other purposes. The Excel file contains sheets with summary data, a sheet with mass spectrometry data for detected ions that could not be assigned molecular formulas ("no hits"), and numerous sheets containing mass spectrometry data for the assigned molecular formulas, which have been separated by heteroatom class (the number and identity of heteroatoms contained in the assigned molecular formulas). The `get_sample_data()` function returns a named list containing file and sample information, selected mass spectrometry parameters, mass spectrometry data related to the assigned molecular formulas, and mass spectrometry data related to the "no hits". To facilitate the plotting of mass spectra, it combines the detected m/z's and relative abundances from the "no hits" and assigned formulas into a list element named "all_detected_ions". Finally, the `get_sample_data()` function computes various values like the nominal (i.e., average) carbon oxidation state (NOSC) useful for exploring and visualizing the composition of the NOM sample.

Most of the other functions are available for visualizing one or more NOM samples in mass spectra, van Krevelen and Kroll diagrams, and for exploring the NOM data in combination with `tidyverse` functions.
