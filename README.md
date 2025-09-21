# Thesis Project: Temperature Exposure and Cardiometabolic Health

## Overview
This repository contains the analytical material that supports Aleksandr Dulepov's
thesis work on the association between apparent temperature exposures and
cardiometabolic health outcomes in the Northern Finland Birth Cohort 1966. The
workflow is implemented in R/Quarto notebooks that:

* assemble and harmonise questionnaire, clinical, environmental and metabolomics
  information into an analysis-ready cohort dataset;
* perform extensive data quality checks, derive categorical variables, and impute
  missing information through multiple imputation by chained equations;
* model the relationships between temperature metrics and systolic/diastolic
  blood pressure and carotid intima-media thickness (CIMT);
* evaluate the robustness of the findings with sensitivity analyses including
  alternative model specifications and complete-case comparisons.

## Repository structure

| File | Description |
| --- | --- |
| `thesis_dataset_preparation.R` | End-to-end data wrangling script that extracts the necessary variables from the NFBC1966 source files, creates labelled factors, and saves the combined cohort data frame. |
| `Clean_imp_project.qmd` | Quarto notebook covering exploratory data analysis, data cleaning, and the multiple imputation workflow used to generate analysis datasets. |
| `Data_analysis.qmd` | Quarto notebook that fits the primary regression models on the multiply imputed datasets and generates descriptive tables and figures. |
| `Sensitivity_analysis.qmd` | Quarto notebook that repeats key analyses under alternative assumptions (e.g., complete-case data, transformed outcomes) to assess robustness. |

All notebooks expect the helper script `Functions_rmph.R` (not included in this
repository) to be available on the working directory or on the R search path.

## Getting started

1. **Install software.** Use R (\>= 4.2) with the Quarto CLI (\>= 1.4) for
   rendering the notebooks.
2. **Install packages.** The notebooks rely on tidyverse tooling (`dplyr`,
   `ggplot2`, `tidyr`, `purrr`, `forcats`), data-labelling utilities (`labelled`,
   `tableone`, `table1`), missing-data and imputation packages (`naniar`, `mice`,
   `VIM`, `miceadds`), modelling helpers (`mgcv`, `olsrr`, `sandwich`, `lmtest`),
   visualisation libraries (`ggplot2`, `visdat`, `pheatmap`, `sjPlot`), and
   reporting utilities (`broom`, `stargazer`, `kableExtra`, `htmlTable`). Install
   any additional packages mentioned at the beginning of each notebook/script.
3. **Configure data paths.** Update the hard-coded UNC paths that point to the
   NFBC1966 secure storage (e.g. `//kaappi.oulu.fi/...`) so that they reference
   your local copies of the source data. The project does **not** ship with the
   cohort data files.
4. **Provide helper functions.** Place the project-specific helper file
   `Functions_rmph.R` in the repository root or adjust `source()` calls to match
   its location.
5. **Render notebooks.** From the repository root run, for example:

   ```bash
   quarto render Clean_imp_project.qmd
   quarto render Data_analysis.qmd
   quarto render Sensitivity_analysis.qmd
   ```

   Rendering will recreate the figures, tables, and model objects used in the
   thesis. Depending on access rights, you may need to pre-compute the imputed
   datasets and store them as `.rds` files at the paths referenced in the
   notebooks.

## Data access
The NFBC1966 data used in this project are governed by strict access controls.
Researchers who wish to reproduce the analyses must request access to the
cohort data through the NFBC project office and download the required files to a
secure environment before running the scripts in this repository.

## Reproducibility tips
* Version-control any local modifications to the Quarto notebooks to preserve a
  transparent history of analytical decisions.
* When updating imputation settings, record the random seeds and package
  versions to ensure results can be compared across runs.
* Consider rendering the notebooks with `quarto render <file> --to html` during
  development for quicker iteration, and switch to the published formats once
  the workflow stabilises.
