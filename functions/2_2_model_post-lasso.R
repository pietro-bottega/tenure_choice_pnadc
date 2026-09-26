library(here)
library(glmnet)
library(svyVGAM)
library(PNADcIBGE) 
library(survey) 
library(dplyr) 
library(tidyr) 
library(stringr) 
library(gt)
library(tibble)

source(here("functions","custom_functions.R"))

# 1. POST LASSO FOR NATIONAL MODEL 

formula_national <- create_formula(
  y = "tenure_condition",
  x = clean_select_vars_national
)

post_lasso_national <- svy_vglm(
  formula = formula_national,
  design = pnadc,
  family = multinomial(refLevel = 1)
)

saveRDS(post_lasso_national, "post_lasso_national.rds")
post_lasso_national <- readRDS("post_lasso_national.rds")

# 2. REGIONAL MODELS

# 2.1. NORTE

pnadc_norte <- subset(pnadc, macroregion == "norte")

formula_norte <- create_formula(
  y = "tenure_condition",
  x = clean_select_vars_norte
)

post_lasso_norte <- suppressWarnings(
  svy_vglm(
    formula = formula_norte,
    design = pnadc_norte,
    family = multinomial(refLevel = 1)
  )
)

