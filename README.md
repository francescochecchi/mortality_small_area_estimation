## A method for small-area estimation of population mortality in settings affected by crises
Francesco Checchi, Adrienne Testa, Amy Gimma, Emilie Koum-Besson, Abdihamid Warsame

London School of Hygiene and Tropical Medicine

# Explanation of R analysis scripts and input data files

## Background
This repository contains R scripts and input datasets/parameter files (all .xslx or .csv) needed to replicate analysis of crisis-attributable mortality during a food insecurity crisis in Somalia (2016-2018). Input files for Somalia are provided here to illustrate and allow for replication of the small-area estimation method used for the analysis. However, the method has been also applied to crises in South Sudan, Nigeria and the Democratic Republic of Congo. With the exception of script `mortality_sae_2_reconstruct_pop_som.r`, all other R scripts are generic, and the structure of input files ia also standardised, i.e. can support application of the estimation method in a new crisis. A full presentation of the estimation method is provided in [this paper](https://www.researchgate.net/publication/352199853_A_Method_for_Small-Area_Estimation_of_Population_Mortality_in_Settings_Affected_by_Crises).

## Description of input data/parameter files
[xxx] refers to the country code - in this case, `som` for Somalia.

*	`[xxx]_analysis_strata.xlsx`, which simply contains the list of administrative level 0 (if needed), 1 (e.g. state/region) and 2 (e.g. district/county) geographical strata; further levels may be added, in which case corresponding parameters should also be added under the <general_parameters> worksheet of `[xxx]_analysis_parameters.xlsx`;
*	`[xxx]_survey_metadata.xlsx`, which contains unique IDs and various meta-variables for each SMART survey included in the analysis;
*	Individual raw datasets of SMART surveys, as exported using [ENA software](https://smartmethodology.org/survey-planning-tools/smart-emergency-nutrition-assessment/) into .csv format (these should be named as per the unique survey ID variable in the `[xxx]_survey_metadata.xlsx` file, and stored in a sub-directory that must be called [survey_datasets](/survey_datasets/); no modifications are needed on the ENA-exported datasets, but we have encountered occasional errors (e.g. non-numeric entries or lone non-empty cells in unmarked columns). Somalia survey datasets are fully clean, but if data from a new crisis are analysed, errors such as the above, if not previously cleaned, will probably throw an error during execution of script 1, necessitating some investigation and manual data cleaning (script 1 will show the survey ID where the error occurs);
*	`[xxx]_predictor_data.xlsx`: this file contains a master table of all predictor datasets, with characteristics of each and options for how to manage them (script 3), including reshaping and aggregation, imputation, smoothing, interpolation, creation of lags, etc.;
*	`[xxx]_demog_data.xlsx`: while script 2 (population reconstruction) as mentioned below will need to be developed for each crisis, maintaining the same data input structure as shown in the sample dataset is recommended to avoid script execution problems; generally this will include a table of all population and displacement datasets used, a worksheet with specific demographic parameters (e.g. birth rate), and the datasets themselves;
*	`[xxx]_analysis_parameters.xlsx`: this is the main file enabling the user to interact with the analysis, and consists of a <general_parameters> worksheet (needed for all scripts) where various parameters needed across the analysis are declared; a <predictor_parameters> worksheet (scripts 4-6) wherein all variables to be considered in model fitting should be listed, with options to force their inclusion or exclusion, retain specific lags, categorise them, consider them in interaction terms, etc.; a <counterfactual_parameters> worksheet (scripts 5-6) where best, worst and most likely scenario values for both predictors and population input datasets are declared, so as to create corresponding counterfactual scenarios; and a <sensitivity_parameters> worksheet (script 6) where the user can specify sensitivity ranges for specific datasets (identified by their R object name).

Generally, dictionaries within each file are not just informational, but also determine which specific variables within each worksheet are read by the scripts. As such, they should be modified with care, and any additional variable or parameter added to any worksheet should be reflected within the corresponding dictionary.

## Description of R scripts

| R script<br>(preceded by `mortality_sae_`| Sub-steps implemented | Required data inputs | Outputs | Notes |
| -------- | --------------------- | -------------------- | ------- | ----- |
| `0_control_code.R` | - install R packages; <br> - read input datasets; <br> - read and declare parameters; <br> - source other dependent scripts (below) | - `[xxx]_analysis_strata.xlsx` <br> - `[xxx]_survey_metadata.xlsx` <br> - Individual raw datasets of SMART surveys (see above) <br> - `[xxx]_analysis_parameters.xlsx` <br> - `[xxx]_predictor_data.xlsx` <br> - `[xxx]_demog_data.xlsx` | none | Any dependent script can also run by executing the script itself or a portion of it. |
| `0_functions.R` | - declare bespoke functions used by different scripts |  | none | |
| `1_manage_surveys.R` | - reanalyse each survey and estimate additional demographic indicators; <br> - explore survey availability across crisis person-time; <br> - prepare survey observations for further analysis steps | - `[xxx]_survey_metadata.xlsx` <br> - Individual raw datasets of SMART surveys (see above) | - Re-analysed survey estimates <br> - Graphs and table of descriptive survey patterns <br> - Merged and reshaped survey observations | Would need to be modified if mortality sources other than SMART surveys are introduced. |
| `2_reconstruct_pop_[xxx].R` | - reconstruct population denominators for each stratum by combining census estimates, internal displacement and refugee data; <br> - estimate the proportion of IDPs as well as in- and out-migration rates | - `[xxx]_analysis_strata.xlsx` <br> - `[xxx]_analysis_parameters.xlsx` <br> - `[xxx]_demog_data.xlsx` |
| `3_manage_predictors.R` | - merge predictors into one time series; <br> - transform predictor values into rate; <br> - visualise completeness and apply completeness cut-offs; <br> - perform specified manual and automated imputations; <br> - compute rolling means and lags; <br> - smooth and/or interpolate; <br> - prepare datasets for model fitting | - `[xxx]_analysis_strata.xlsx` <br> - `[xxx]_analysis_parameters.xlsx` <br> - `[xxx]_predictor_data.xlsx`|
| `4_predictive_model.R` | - explore predictor distributions; <br> - categorise predictors; <br> - univariate analysis (categorical vs. continuous, best-fitting lag, screening out predictors with low association); <br> - brute force search across all candidate models; <br> - select best fixed-effects model based on cross-validation; <br> - explore interactions; <br> - fit mixed model and select between fixed-effects only and mixed option; <br> - calculate robust standard errors if fixed-effects model is selected; <br> - compute and graph various metrics of model performance; <br> - save model for subsequent steps | - `[xxx]_analysis_parameters.xlsx` <br> - `[xxx]_predictor_data.xlsx` |
| `5_estimate_mortality.R` | - create counterfactual datasets for each scenario; <br> - implement excess death toll estimation for three counterfactual scenarios (best, worst, most likely); <br> - aggregate death toll estimates as desired and create graphs and tables | - `[xxx]_analysis_parameters.xlsx` <br> - `[xxx]_demog_data.xlsx` |
| `6_sensitivity_analyses.R` | - sensitivity analysis of population and displacement data; <br> - sensitivity analysis of under 5y mortality underreporting; <br> - implemented by creating new input datasets that incorporate sensitivity assumptions, and re-running all analysis steps | - `[xxx]_analysis_parameters.xlsx` |








