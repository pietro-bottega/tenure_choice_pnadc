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

pnadc <- rescale_income_for_interpretation(pnadc)

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

# 2.2. NORDESTE

pnadc_nordeste <- subset(pnadc, macroregion == "nordeste")

formula_nordeste <- create_formula(
  y = "tenure_condition",
  x = clean_select_vars_nordeste
)

post_lasso_nordeste <- suppressWarnings(
  svy_vglm(
    formula = formula_nordeste,
    design = pnadc_nordeste,
    family = multinomial(refLevel = 1)
  )
)

# 2.3. SUDESTE

pnadc_sudeste <- subset(pnadc, macroregion == "sudeste")

formula_sudeste <- create_formula(
  y = "tenure_condition",
  x = clean_select_vars_sudeste
)

post_lasso_sudeste <- suppressWarnings(
  svy_vglm(
    formula = formula_sudeste,
    design = pnadc_sudeste,
    family = multinomial(refLevel = 1)
  )
)

# 2.4. SUL

pnadc_sul <- subset(pnadc, macroregion == "sul")

formula_sul <- create_formula(
  y = "tenure_condition",
  x = clean_select_vars_sul
)

post_lasso_sul <- suppressWarnings(
  svy_vglm(
    formula = formula_sul,
    design = pnadc_sul,
    family = multinomial(refLevel = 1)
  )
)

# 2.4. CENTROESTE

pnadc_centroeste <- subset(pnadc, macroregion == "centro-oeste")

formula_centroeste <- create_formula(
  y = "tenure_condition",
  x = clean_select_vars_centroeste
)

post_lasso_centroeste <- suppressWarnings(
  svy_vglm(
    formula = formula_centroeste,
    design = pnadc_centroeste,
    family = multinomial(refLevel = 1)
  )
)