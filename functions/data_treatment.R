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

generate_tenure_table <- function(design_obj, tenure_col = "tenure_condition") {
  
  # 1. Calculate raw frequencies using the survey design
  # Construct a formula dynamically (e.g., ~tenure_condition)
  formula_str <- as.formula(paste0("~", tenure_col))
  freqs <- as.data.frame(svytable(formula_str, design = design_obj))
  colnames(freqs) <- c("category", "freq")
  
  # Helper function to extract frequencies safely
  get_freq <- function(cat_name) {
    val <- freqs$freq[freqs$category == cat_name]
    if (length(val) == 0) return(0) else return(val)
  }
  
  # 2. Define Valid categories and Missing category
  valid_cats <- c(
    'Proprietário Formal', 
    'Inquilino Formal', 
    'Proprietário Informal', 
    'Inquilino Informal'
  )
  
  # 3. Separate frequencies into Valid and Missing totals
  total_valid <- sum(sapply(valid_cats, get_freq))
  missing_freq <- get_freq('Outros')
  grand_total <- total_valid + missing_freq
  
  # 4. Build the rows step-by-step
  rows <- list()
  cum_pct <- 0
  
  # A. Add Valid categories
  for (cat in valid_cats) {
    f <- get_freq(cat)
    pct <- if (grand_total > 0) (f / grand_total) * 100 else 0
    valid_pct <- if (total_valid > 0) (f / total_valid) * 100 else 0
    cum_pct <- cum_pct + valid_pct
    
    rows[[length(rows) + 1]] <- data.frame(
      Level_1 = "Válido",
      Condicao = cat,
      Frequencia = f,
      Percentual = pct,
      Percentual_valido = valid_pct,
      Percentual_acumulado = cum_pct
    )
  }
  
  # B. Add Total Valid row
  rows[[length(rows) + 1]] <- data.frame(
    Level_1 = "Válido",
    Condicao = "Total",
    Frequencia = total_valid,
    Percentual = if (grand_total > 0) (total_valid / grand_total) * 100 else 0,
    Percentual_valido = 100.0,
    Percentual_acumulado = NA
  )
  
  # C. Add Missing row
  rows[[length(rows) + 1]] <- data.frame(
    Level_1 = "Faltante",
    Condicao = "", # Leaves condition blank just like Python
    Frequencia = missing_freq,
    Percentual = if (grand_total > 0) (missing_freq / grand_total) * 100 else 0,
    Percentual_valido = NA,
    Percentual_acumulado = NA
  )
  
  # D. Add Grand Total row
  rows[[length(rows) + 1]] <- data.frame(
    Level_1 = "Total",
    Condicao = "",
    Frequencia = grand_total,
    Percentual = 100.0,
    Percentual_valido = NA,
    Percentual_acumulado = NA
  )
  
  # Combine everything into a single data frame
  df_table <- bind_rows(rows)
  
  # 5. Format the output using gt
  styled_table <- df_table %>%
    # This creates the visual grouping
    gt(groupname_col = "Level_1") %>% 
    cols_label(
      Condicao = "Condição de ocupação",
      Frequencia = "Frequência",
      Percentual = "Percentual",
      Percentual_valido = "Percentual válido",
      Percentual_acumulado = "Percentual acumulado"
    ) %>%
    # Format raw counts (no decimals, comma separated)
    fmt_number(
      columns = c(Frequencia),
      decimals = 0,
      sep_mark = ".",
      dec_mark = ","
    ) %>%
    # Format percentages (1 decimal place)
    fmt_number(
      columns = c(Percentual, Percentual_valido, Percentual_acumulado),
      decimals = 1,
      sep_mark = ".",
      dec_mark = ","
    ) %>%
    # Replace NAs with blank spaces
    sub_missing(
      columns = everything(),
      missing_text = ""
    ) %>%
    # Add a slight visual distinction to the row groups
    tab_options(
      row_group.font.weight = "bold",
      row_group.background.color = "#f9f9f9"
    )
  
  return(styled_table)
}