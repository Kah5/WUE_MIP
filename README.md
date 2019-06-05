# WUE_MIP

## This repository contains code to read in regional runs of ecosystem models from Paleon Project, calculate ecosytem water use efficiency and growth changes, and compare to ecologically sampled tree ring data and & ITRDB tree ring data.

## Key Scripts:

### Scripts to read data include:
#### 01_extract_vars_prelim.R: code to extract paleon outputs from netcdf files
#### ITRDB_downlowd.R: Downloads ITRDB data within a specified domain for our region
#### 

### Code to calculate WUE and make WUE change figures:

WUE_GWBI_plots.R

### Split up into testing and training datasets:
#### Species_GWBI_split_train_test.R & Species_ED2_GWBI_split_train_test.R: Splits Gross Woody Biomass increment into testing and training data for models.
#### Split_train_test_ITRDB.R Splits ITRDB increment into testing and training data for models.

### Code to estimate parameters for growth responses to climate:
#### 08b_total_gwbi_models_ecological.R: Estimates growth model on ecologically sampled Quercus spp. tree rings in the midwest
#### Fit_all_pft_guess_time_re.R & Fit_all_pft_ED2_time.R: Fits climate response & lagged growth models with random effects for the time period
####
