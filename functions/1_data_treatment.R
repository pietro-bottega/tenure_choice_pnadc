library(PNADcIBGE)
library(survey)
library(survey)
library(dplyr)
library(tidyr)
library(stringr)
library(gt)

source("functions/custom_functions.R")


# 1. IMPORT PNADC DATA

variables <- c('UPA', 'V1008', 'V1014', #identifiers
               'V1022', 'V2005', #filters
               'S01001', 'S01017', 'S01020', 'S01020A', #tenure
               'VD5008', 'VD5007', 'VD3005', 'VD4046', #income
               'S01023', 'S01024', 'S01025', 'S01028', 'S01029', 'S01031', 
               'S01011A', 'V2001', 'S01002', 'S01003', 'S01012A', 
               'S01014', 'S01010', 'S01005', #wealth index
               'V2010', 'VD4009', 'V2007', #vulnerability
               'V2009', 'VD2003', 'VD2004', # household and lifecycle characteristics
               'V1023', 'UF') # location

pnadc <- get_filtered_pnadc(
  target_year = 2025,
  target_interview = 1,
  target_vars = variables
)

message("data loaded")

# 2. TREAT y VARIABLES
pnadc <- classify_tenure_condition(pnadc)

message("y variable created")

# 3. TREAT x VARIABLES

## Household and lifecycle characteristics
pnadc <- classify_structure(pnadc)

## Income and wealth
pnadc <- apply_deflator(pnadc) # Income variables
pnadc <- turn_numeric(pnadc) # Years of study
pnadc <- build_wealth_index(pnadc) # Wealth index

# Vulnerability and credit constraints
pnadc <- build_head_dependency(pnadc) # Head dependency
pnadc <- classify_worker_status(pnadc) # Worker status
pnadc <- classify_single_mom(pnadc) # Single mom with child below 14

# Location
pnadc <- classify_metropolitan_area(pnadc) # Metropolitan area
pnadc <- classify_macroregion(pnadc) # Macro region

message("x variables created")

# 4. RENAME AND DROP VARIABLES UNNECESSARY

cols_unused <- c("V1022", "V1023", "V2003", "V2005", "V2007",
                 "VD2004", "VD4009", "VD4046", "VD5007", "VD5008",
                 "CO1", "CO1e", "CO2", "CO2e", "CO3", "bath_ratio")

pnadc <- drop_variables(pnadc, cols_unused)


var_new_names <- c(
  "age" = "V2009",
  "race" = "V2010",
  "household_size" = "VD2003"
)

pnadc <- rename_variables(pnadc, var_new_names)

message("cleaned survey object")

