# Master Thesis Repository

## Overview
This repository contains the code for the analysis used in my master's thesis **Joint Density Modelling of Income and Wealth** in Statistics at *ETH Zurich*. 
Since the administrative tax data used in this thesis is highly confidential, this repository contains only the code.

## Repository Structure
- `00_utils.R`: Utility functions used across the project.
- `01_setup.R`: Setup script for initializing the project environment.

- `/_utils`: Utility scripts specific to certain methods.
  - `copula.R`: Utilities for copula models.
  - `mbo.R`: Utilities for model-based optimization.
 
- `/_dataprep`: Scripts for data preparation.
  - `01_dataprep.R`: Initial data preparation script.
  - `02_dataprep2.R`: Further preparations and scaling data.

- `/_fitting`: Model fitting scripts.
  - `/Copula`: Scripts for fitting copula models.
    - `01_fit_copula_residuals.R`: Fit copula models to residuals and predictions of regression models.
    - `02_fit_copula_data.R`: Fit copula models to income and wealth data and compare simulated data with original data.
  - `/LM`: Linear models fitting scripts.
      - `01_lm.R`: Fit baseline LM models (main results).
      - `02_lm_cv.R`: Fit LM models on different partitions of data and compute CV R-squared (robustness check).
      - `03_fit_lm_nonnested.R`: Fit LM models that are not nested (robustness check).
  - `/MARS`: Scripts for fitting MARS models.
     - `01_mars.R`: Fit MARS models (baseline as well as grouped by sign of income and wealth).
  - `/RF`: Random Forest model fitting and tuning scripts.
      - `01_rf_grid.R`: Initial grid-search.
      - `02_rf_grid2.R`: Refined grid-search using caret.
      - `03_rf_mbo.R`: Hyperparameter tuning via model-based optimization (MBO).
      - `04_rf_fit_mbo.R`: Fit final result of MBO and create plots.

- `/_analysis`: Directory containing scripts for data analysis.
  - `01_combine_models.R`: Script for combining models.
  - `02_sanity_checks.R`: Sanity checks for the analysis.
  - `03_descriptive.R`: Descriptive statistics analysis.
  - `04_analysis2.R`: Additional analysis script.
