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
    'proprietario_formal', 
    'inquilino_formal', 
    'proprietario_informal', 
    'inquilino_informal'
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
    Condicao = "", 
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

generate_summary_table <- function(design_obj) {
  
  # 1. Define the variables you want to analyze
  target_vars <- c("V2009", "VD2003", "VD3005_num", 
                   "VD5007_real", "VD5008_real", 
                   "wealth_index", "head_dependency")
  
  # Initialize an empty list to store the results
  results_list <- list()
  
  # 2. Loop through each variable to calculate the statistics
  for (var in target_vars) {
    
    # Check if the variable actually exists in the survey object to prevent crashes
    if (var %in% names(design_obj$variables)) {
      
      # Convert the string to a formula dynamically (e.g., "V2009" becomes ~V2009)
      fmla <- as.formula(paste0("~", var))
      
      # Calculate the weighted statistics
      # Note: We use na.rm = TRUE to handle missing data gracefully
      mean_obj   <- svymean(fmla, design_obj, na.rm = TRUE)
      var_obj    <- svyvar(fmla, design_obj, na.rm = TRUE)
      median_obj <- svyquantile(fmla, design_obj, quantiles = 0.5, na.rm = TRUE)
      
      # Extract the pure numeric values
      mean_val   <- as.numeric(mean_obj[1])
      var_val    <- as.numeric(var_obj[1])
      
      # Calculate standard deviation by taking the square root of the variance
      sd_val     <- sqrt(var_val)
      
      # svyquantile returns a slightly more complex list structure in newer versions
      median_val <- as.numeric(median_obj[[var]][1])
      
      # Store in a temporary data frame
      results_list[[var]] <- data.frame(
        Variable = var,
        Mean = mean_val,
        Median = median_val,
        Standard_Deviation = sd_val,
        stringsAsFactors = FALSE
      )
      
    } else {
      warning(sprintf("Variable '%s' not found in the design object. Skipping.", var))
    }
  }
  
  # 3. Combine all the individual rows into one clean data frame
  summary_df <- bind_rows(results_list)
  
  # 4. Render as a beautifully formatted 'gt' table
  final_table <- summary_df %>%
    gt() %>%
    tab_header(
      title = "Weighted Summary Statistics",
      subtitle = "Mean, Median, and Standard Deviation for Selected PNADC Variables"
    ) %>%
    # Format the column labels to look clean (replaces underscore with a space)
    cols_label(
      Standard_Deviation = "Standard Deviation"
    ) %>%
    # Format numbers to 2 decimal places for readability
    fmt_number(
      columns = c(Mean, Median, Standard_Deviation),
      decimals = 2
    ) %>%
    # Add some styling to the column headers
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    )
  
  return(final_table)
}