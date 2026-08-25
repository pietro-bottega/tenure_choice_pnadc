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

all_columns <- colnames(pnadc)
relevant_variables <- all_columns[!startsWith(all_columns, "V1032")]

# Doing one hot encoding and generating a matrix with the survey object
pnadc_matrix_national <- create_matrix_national(pnadc)

#2. SELECT VARIABLES

message("Executing lasso for the national aggregate")

lasso_national <- run_lasso_national(pnadc_matrix_national)

# Getting lambda value
lambda_1se_national <- lasso_national$lambda.1se

# Get selected vars
coefs_national <- coef(lasso_national, s = "lambda.1se")
select_vars_national <- get_selected_vars(coefs_national)

message("Executing lasso for the regional models")

regional_models <- run_regional_lasso(pnadc)

norte_coefs <- coef(regional_models[["norte"]], s = "lambda.1se")
select_vars_norte <- get_selected_vars(norte_coefs)

nordeste_coefs <- coef(regional_models[["nordeste"]], s = "lambda.1se")
select_vars_nordeste <- get_selected_vars(nordeste_coefs)

centroeste_coefs <- coef(regional_models[["centro-oeste"]], s = "lambda.1se")
select_vars_centroeste <- get_selected_vars(centroeste_coefs)

sudeste_coefs <- coef(regional_models[["sudeste"]], s = "lambda.1se")
select_vars_sudeste <- get_selected_vars(sudeste_coefs)

sul_coefs <- coef(regional_models[["sul"]], s = "lambda.1se")
select_vars_sul <- get_selected_vars(sul_coefs)

message("All coeficients extracted")