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

# 1. PREPARE DATA

clean_select_vars_national <- clean_lasso_names(select_vars_national, relevant_variables)

# 2. POST LASSO FOR NATIONAL MODEL 

formula_national <- create_formula(
  y = "tenure_condition",
  x = clean_select_vars_national
)

post_lasso_national <- svy_vglm(
  formula = formula_national,
  design = pnadc,
  weights = V1032_balanced,
  family = multinomial(refLevel = 1)
)

saveRDS(post_lasso_national, "post_lasso_national.rds")