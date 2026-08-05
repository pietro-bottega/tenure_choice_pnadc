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

library(survey)

# Define the function
generate_summary <- function(var_name, design_obj) {
  
  # 1. Convert the string variable name into a formula (e.g., "V2009" becomes ~V2009)
  fmla <- as.formula(paste0("~", var_name))
  
  # 2. Calculate statistics using the dynamic formula
  mean_val <- svymean(fmla, design_obj, na.rm = TRUE)
  var_val  <- svyvar(fmla, design_obj, na.rm = TRUE)
  sd_val   <- sqrt(var_val)
  quantiles_val <- svyquantile(fmla, design_obj, quantiles = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  
  # 3. Print the formatted results
  cat(sprintf("--- Weighted Summary (%s) ---\n\n", var_name))
  print(mean_val)
  
  cat("\nStandard Deviation:\n\n")
  print(sd_val)
  
  cat("\nQuantiles:\n\n")
  print(quantiles_val)
}