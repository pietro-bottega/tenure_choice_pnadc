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

clean_lasso_names <- function(matrix, variables) {
  
  # Create an empty vector
  clean_vector <- c()
  
  # Iterate over variables in the matrix, one hot encoded
  for (matrix_name in matrix) {
    
    # Iterate over the list of original variables
    for (orig_var in variables) {
      
      # If the matrix name starts exactly with the original variable name
      if (startsWith(matrix_name, orig_var)) {
        clean_vector <- c(clean_vector, orig_var)
        break # Exit the inner loop 
      }
    }
  }
  
  # Get unique from multiple categories
  clean_vector <- unique(clean_vector)
  
  return(clean_vector)
}

# 2. POST LASSO FOR NATIONAL MODEL 

clean_select_vars_national <- clean_lasso_names(select_vars_national, relevant_variables)

create_formula <- function(y, x) {
  right_side <- paste(x, collapse = " + ")
  equation <- paste(y, "~", right_side)
  formula <- as.formula(equation)
  return(formula)
}

formula_national <- create_formula(
  y = "tenure_condition",
  x = clean_select_vars_national
)

post_lasso_nationl <- svy_vglm(
  formula = formula_national,
  design = pnadc,
  family = multinomial
)