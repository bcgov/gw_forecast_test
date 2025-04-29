# ==============================================================================
# Script name:      01_ConfigInputs.R
# ------------------------------------------------------------------------------
# Script version:   
# 2025-04-01:       v3
#
# ------------------------------------------------------------------------------
# Notes/Objectives:
# loads common libraries, sets default switches, colours, ggplot formatting
# 
# ==============================================================================
#
## LOAD CRAN PACKAGES --------------------------------------------------
pkgs <- c(#'tidyverse',
  'dplyr',
  'ggplot2',
  'tidyr',
  'lubridate',
  'readr',
  'stringr',
  'tibble',
  'forcats',
  'padr',
  'zoo',
  'ggplot2',
  'patchwork',
  'readxl',
  'RSQLite',
  'randomForest',
  'ggnewscale',
  'forecast',
  'mgcv',
  # 'stats',
  'xts',
  'powerjoin',
  'foreach',
  'doParallel',
  'reshape',
  "janitor",
  "ggpubr",
  'sp',
  'sf',
  'nnet',
  'cowplot',
  'aws.s3',
  'pak'
  #'remotes',
)

#Queries and installs missing packages
# options(timeout = 1200)
new.packages <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# Non-cran packages
pkgs_github <- c('weathercan', 'bcsnowdata')
new.packages_github <- pkgs_github[!(pkgs_github %in% installed.packages()[,"Package"])]

if ('weathercan' %in% new.packages_github) {
  pak::pak("ropensci/weathercan")
}
if ('bcsnowdata' %in% new.packages_github) {
  pak::pak("bcgov/bcsnowdata")
}

# load packages
pkgs <- c(pkgs, pkgs_github)
lapply(pkgs, library, character.only = TRUE) 



# MANUAL INPUTS ----------------------------------------------------------------

# Choose location to save files
figure_location <- "Output/"
data_location <- "data/"
model_path <- "models/"
user_input_location <- "user_inputs/"


# Fill in well information based on location 

pgown_well_info_all <- read_csv(paste0(user_input_location,"Forecasting_Model_Data.csv")) %>%
  filter(!is.na(Climate_station_Id) & !is.na(Lag_time)) %>%
  mutate(Climate_Infilled_id = ifelse(is.na(Climate_Infilled_id), 0, Climate_Infilled_id),
         Climate_secondary_Infilled = ifelse(is.na(Climate_secondary_Infilled), 0, Climate_secondary_Infilled),
         Climate_tertiary_Infilled = ifelse(is.na(Climate_tertiary_Infilled), 0, Climate_tertiary_Infilled),
         Climate_quaternary_Infilled = ifelse(is.na(Climate_quaternary_Infilled), 0, Climate_quaternary_Infilled)) 



Regional_group_list <- pgown_well_info_all %>%
  dplyr::select(Regional_group) %>%
  distinct(Regional_group) %>%
  dplyr::pull(Regional_group)

Regional_group_list <- as.list(Regional_group_list)


# User set number of days to forecast
forecast_days <- c(14,30,60,90)

# Number of cores (for parallel computing)
num_cores <- 4

# OPTIONS ----------------------------------------------------------------

options(digits = 3, scipen = 5, warn = 0)

## AWS
# library(aws.s3)
# 
# b <- 'rfc-conditions/gw_forecasting/outputs/'
# r <- ""
