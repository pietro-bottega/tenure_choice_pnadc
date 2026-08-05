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