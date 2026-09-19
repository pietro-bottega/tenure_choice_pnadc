library(here)
library(glmnet)
library(svyVGAM)
library(VGAM)
library(PNADcIBGE) 
library(survey) 
library(dplyr) 
library(tidyr) 
library(stringr) 
library(gt)
library(tibble)

source(here("functions","custom_functions.R"))

regression_variables <- c("age", "race", "household_size", "family_structure", "household_income", "household_income_pcapita", "education_years", "wealth_index", "head_dependency", "worker_status", "single_mom", "metropolitan_area")

regions <- c("norte", "nordeste", "sudeste", "sul", "centro-oeste")

# ---- Compute the full national category set ONCE, before the loop,
#      and pass it into every regional call via levels_all_override

levels_all_national <- levels(factor(pnadc$variables$tenure_condition))

# ---- Sanity check BEFORE running anything: make sure no region has
#      zero observations of some category.

region_category_counts <- table(
  pnadc$variables$macroregion,
  pnadc$variables$tenure_condition
)

print(region_category_counts)

# ---- Sanity check: enough distinct clusters per region for k = 10 folds.

sapply(regions, function(reg) {
  length(unique(subset(pnadc, macroregion == reg)$variables$UPA))
})

# National

nested_cv_results_national <- run_nested_cv(
  design_obj            = pnadc,
  regression_variables  = regression_variables,
  cluster_var           = "UPA",
  group_var             = "macroregion",
  k = 10,
  inner_nfolds = 10
)

# Regional

regional_results <- list()

for (reg in regions) {
  message(sprintf("=== Running region: %s ===", reg))
  
  region_design <- subset(pnadc, macroregion == reg)
  
  rare_rows <- which(region_design$variables$race == "9")
  force_ids_reg <- unique(region_design$variables[["UPA"]][rare_rows])
  
  regional_results[[reg]] <- run_nested_cv(
    design_obj           = region_design,
    regression_variables = regression_variables,
    cluster_var          = "UPA",
    y_var                = "tenure_condition",
    levels_all_override  = levels_all_national,
    force_train_ids = force_ids_reg,
    k    = 10,
    seed = 123
  )
}