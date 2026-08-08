get_filtered_pnadc <- function(target_year, target_interview, target_vars) {
  # 1. Download data
  pnadc_raw <- PNADcIBGE::get_pnadc(
    year = target_year,
    interview = target_interview,
    vars = target_vars,
    labels = FALSE
  )
  
  # 2. Apply filters to get urban and household heads
  pnadc_filtered <- subset(pnadc_raw, V1022 == 1 & V2005 == '01')
  
  return(pnadc_filtered)
}

classify_tenure_condition <- function(design_obj) {
  
  # update() evaluates expressions in the context of the survey design data
  updated_design <- update(
    design_obj,
    
    # 1. Ensure columns are strings
    S01001  = str_replace(as.character(S01001), "\\.0$", ""),
    S01017  = str_replace(as.character(S01017), "\\.0$", ""),
    S01020  = str_replace(as.character(S01020), "\\.0$", ""),
    S01020A = str_replace(as.character(S01020A), "\\.0$", ""),
    
    # 2. Apply the specific tenure classification rules
    tenure_condition = case_when(
      # i) Proprietários formais
      S01017 %in% c('1', '2') & S01020 == '1' & S01020A == '1' ~ 'Proprietário Formal',
      
      # ii) Inquilinos formais
      S01001 %in% c('1', '2') & S01017 %in% c('3', '4', '5', '6') ~ 'Inquilino Formal',
      
      # iii) Proprietários informais
      S01017 %in% c('1', '2') & (S01020 == '2' | S01020A == '2') ~ 'Proprietário Informal',
      
      # iv) Inquilinos informais
      S01001 == '3' & S01017 %in% c('3', '4', '5', '6') ~ 'Inquilino Informal',
      
      # v) Outros
      TRUE ~ 'Outros'
    )
  )
  
  return(updated_design)
}

classify_structure <- function(design_obj) {
  
  # update() evaluates expressions in the context of the survey design data
  updated_design <- update(
    design_obj,
    
    # 1. Ensure columns are strings
    VD2004  = str_replace(as.character(VD2004), "\\.0$", ""),
    
    # 2. Apply the specific structure classification rules
    family_structure = case_when(
      # If "Unipessoal" structure
      VD2004 == '1' ~ "Unipessoal",
      
      # Handle missing data
      is.na(VD2004) ~ NA_character_,
      
      # Everything else is classified as "Não Unipessoal"
      TRUE ~ "Não Unipessoal"
    )
  )
  
  return(updated_design)
}

apply_deflator <- function(design_object) {
  updated_design <- update(
    design_object,
    VD5007_real = VD5007 * CO2,
    VD5008_real = VD5008 * CO2,
    VD4046_real = VD4046 * CO2
  )
  
  return(updated_design)
}

turn_numeric <- function(design_object) {
  updated_design <- update(
    design_object,
    VD3005_num = as.numeric(as.character(VD3005))
    )
  
  return(updated_design)
}

build_wealth_index <- function(design_obj) {
  
  # --- 1. CALCULATE BATH PER CAPITA QUARTILE ---
  temp_design <- update(design_obj, 
                        bath_per_capita = as.numeric(as.character(S01011A)) / 
                          as.numeric(as.character(V2001)))
  
  # Extract the weighted quartiles
  bath_quants <- svyquantile(~bath_per_capita, temp_design, 
                             quantiles = c(0.25, 0.50, 0.75, 1), 
                             na.rm = TRUE)
  
  # Extract the exact numeric cutoffs dynamically
  q1 <- bath_quants$bath_per_capita[1]
  q2 <- bath_quants$bath_per_capita[2]
  q3 <- bath_quants$bath_per_capita[3]
  q4 <- bath_quants$bath_per_capita[4]
  
  # Clean up temporary design
  rm(temp_design)
  gc()
  
  # --- 2. EXTRACT DATA ---
  # Use raw data to lower RAM usage
  raw_data <- design_obj$variables
  
  # --- 3: TRANSFORM DATA ---
  raw_data <- raw_data %>%
    mutate(
      across(c(S01023, S01024, S01025, S01028, S01029, S01031, 
               S01011A, V2001, S01002, S01003, S01012A, 
               S01014, S01010, S01005), ~as.numeric(as.character(.))),
      
      # 1. DURABLE GOODS SCORE
      score_fridge = case_when(
        S01023 == 2 ~ 1.5,
        S01023 == 1 ~ 1.0,
        TRUE ~ 0
      ),
      
      score_durables = score_fridge + 
        as.numeric(S01024 == 1) + 
        as.numeric(S01025 == 1) + 
        as.numeric(S01028 == 1) + 
        as.numeric(S01029 == 1) + 
        as.numeric(S01031 == 1),
      
      # 2. BATHROOMS PER PERSON SCORE (Using dynamic cutoffs)
      bath_ratio = S01011A / V2001,
      
      score_baths = case_when(
        bath_ratio <= q4 ~ 1.0,        # First quartile 100-75
        bath_ratio <= q3 ~ 0.5,        # Second quartile 75-50
        bath_ratio <= q2 ~ -0.5,       # Third quartile 50-25
        bath_ratio >  q1 ~ -1.0,       # Forth quartile 25-0
        TRUE ~ 0                       # Fallback for NAs
      ),
      
      # 3. INADEQUATE HOUSING PENALTIES
      penalty_walls  = as.numeric(S01002 != 1),
      penalty_roof   = as.numeric(!S01003 %in% c(1, 2, 3, 4)),
      penalty_sewage = as.numeric(!S01012A %in% c(1, 2, 3)),
      penalty_elec   = as.numeric(S01014 != 1),
      penalty_piped  = as.numeric(S01010 != 1),
      penalty_crowd  = as.numeric((V2001 / S01005) >= 3),
      
      total_penalty  = penalty_walls + penalty_roof + 
        penalty_sewage + penalty_elec + penalty_piped + penalty_crowd,
      
      # 4. FINAL WEALTH INDEX
      wealth_index   = score_durables + score_baths - total_penalty
    ) %>%
    
    # 4. DROP COLUMNS
    select(-score_fridge, -score_durables, -score_baths, 
           -starts_with("penalty_"), -total_penalty,
           
           # Drop the raw housing/durable goods variables you no longer need
           -S01023, -S01024, -S01025, -S01028, -S01029, -S01031, 
           -S01011A, -S01002, -S01003, -S01012A, -S01014, 
           -S01010, -S01005, -V2001)
  
  # 5. REASSIGN AND CLEAN UP ---
  # Put the mutated data back into the survey design object safely
  design_obj$variables <- raw_data
  
  # Final garbage collection
  gc()
  
  return(design_obj)
}

build_head_dependency <- function(design_obj) {
  
  # 1. GET DATA
  raw_data <- design_obj$variables
  
  # 2. CALCULATION
  raw_data <- raw_data %>%
    mutate(
      VD4046_num = as.numeric(as.character(VD4046_real)),
      VD5007_num = as.numeric(as.character(VD5007_real)),
      
      head_dependency = if_else(
        VD5007_num > 0, 
        VD4046_num / VD5007_num, 
        NA_real_
      )
    ) %>%
    # Drop temporary calculation columns
    select(-VD4046_num, -VD5007_num)
  
  # 3. Reassign to survey object
  design_obj$variables <- raw_data
  
  return(design_obj)
}