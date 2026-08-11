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
      VD2004 == '1' ~ 0,
      
      # Handle missing data
      is.na(VD2004) ~ NA_real_,
      
      # Everything else is classified as "Não Unipessoal"
      TRUE ~ 1
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

classify_worker_status <- function(design_object) {
  
  updated_design <- update(
    design_object,
    worker_status = case_when(
      # Trabalhador formal
      VD4009 %in% c("01", "03") ~ "Trabalhador formal",
      
      # Servidor público
      VD4009 %in% c("05", "07") ~ "Servidor público",
      
      # Empregador
      VD4009 %in% c("08") ~ "Empregador",
      
      # Informal (Explicit codes)
      VD4009 %in% c("02", "04", "06", "09", "10") ~ "Informal",
      
      # Catching explicit NAs and applying the new rule
      is.na(VD4009) ~ "Informal",
      
      # Catch-all for any unexpected blanks or undefined codes
      TRUE ~ "Informal"
    )
  )
  
  return(updated_design)
}

classify_single_mom <- function(filtered_design_obj, survey_year = 2025, survey_quarter = 1) {
  
  # 1. Download data
  unfiltered_data <- PNADcIBGE::get_pnadc(
    year = survey_year,
    interview = survey_quarter,
    vars = c('UPA', 'V1008', 'V1014', 'V2009', 'V2007', 'V2005'),
    labels = TRUE,
    design = FALSE
  )
  
  # 2. Create a Household Summary
  message("Summarizing household structures...")
  household_summary <- unfiltered_data %>%
    group_by(UPA, V1008, V1014) %>%
    summarize(
      has_child_under_14 = any(as.numeric(as.character(V2009)) < 14 & 
                                 grepl("Filho|Enteado", V2005, ignore.case = TRUE), na.rm = TRUE),
      
      has_spouse = any(grepl("Cônjuge", V2005, ignore.case = TRUE), na.rm = TRUE),
      .groups = 'drop'
    )
  
  # 3. Extract data payload
  message("Applying classification to the survey design object...")
  filtered_data <- filtered_design_obj$variables
  
  # 4. Merge, Mutate, and Classify
  filtered_data <- filtered_data %>%
    left_join(household_summary, by = c("UPA", "V1008", "V1014")) %>%
    mutate(
      has_child_under_14 = replace_na(has_child_under_14, FALSE),
      has_spouse = replace_na(has_spouse, FALSE)
    ) %>%
    mutate(
      single_mom = case_when(
        V2007 == "2" & has_child_under_14 == TRUE & has_spouse == FALSE ~ 1,
        TRUE ~ 0
      )
    ) %>%
    # Drop temporary columns
    select(-has_child_under_14, -has_spouse)
  
  # 5. Plug the updated data into the survey object
  filtered_design_obj$variables <- filtered_data
  
  message("'single_mom' column has been added.")
  return(filtered_design_obj)
}

classify_metropolitan_area <- function(design_obj) {
  
  updated_design <- update(
    design_obj,
    metropolitan_area = case_when(
      as.numeric(as.character(V1023)) %in% c(1, 2, 3) ~ 1,
      as.numeric(as.character(V1023)) %in% c(4) ~ 0,
      TRUE ~ NA_real_ 
    )
  )
  
  return(updated_design)
}

classify_macroregion <- function(design_obj) {
  
  updated_design <- update(
    design_obj,
    # as.character() ensures this works perfectly whether UF is a factor, string, or number
    macroregion = case_when(
      as.character(UF) %in% c(11, 12, 13, 14, 15, 16, 17) ~ "Norte",
      as.character(UF) %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29) ~ "Nordeste",
      as.character(UF) %in% c(31, 32, 33, 55) ~ "Sudeste",
      as.character(UF) %in% c(41, 42, 43) ~ "Sul",
      as.character(UF) %in% c(50, 51, 52, 53) ~ "Centro-Oeste",
      TRUE ~ NA_character_
    )
  )
  
  return(updated_design)
}