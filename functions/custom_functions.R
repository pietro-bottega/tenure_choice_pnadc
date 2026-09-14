# 1. DATA IMPORTATION AND TREATMENT  -------------------------------------------------------------------------------------

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
      S01017 %in% c('1', '2') & S01020 == '1' & S01020A == '1' ~ 'proprietario_formal',
      
      # ii) Inquilinos formais
      S01001 %in% c('1', '2') & S01017 %in% c('3', '4', '5', '6') ~ 'inquilino_formal',
      
      # iii) Proprietários informais
      S01017 %in% c('1', '2') & (S01020 == '2' | S01020A == '2') ~ 'proprietario_informal',
      
      # iv) Inquilinos informais
      S01001 == '3' & S01017 %in% c('3', '4', '5', '6') ~ 'inquilino_informal',
      
      # v) Outros
      TRUE ~ 'Outros'
    )
  )
  
  return(updated_design)
}

consolidate_informal_condition <- function(design_obj) {
  updated_design <- update(
    design_obj,
    
    tenure_condition = as.factor(case_when(
      
      # consolidate informal conditions
      tenure_condition %in% c('proprietario_informal', 'inquilino_informal') ~ 'morador_informal',
      
      TRUE ~ as.character(tenure_condition)
    
      ))
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
      VD2004 == '1' ~ "unipessoal",
      
      # Handle missing data
      is.na(VD2004) ~ "nao_informado",
      
      # Everything else is classified as "Não Unipessoal"
      TRUE ~ "nao_unipessoal"
    )
  )
  
  return(updated_design)
}

apply_deflator <- function(design_object) {
  updated_design <- update(
    design_object,
    household_income = VD5007 * CO2,
    household_income_pcapita = VD5008 * CO2,
    VD4046_real = VD4046 * CO2
  )
  
  return(updated_design)
}

turn_numeric <- function(design_object) {
  updated_design <- update(
    design_object,
    education_years = as.numeric(as.character(VD3005))
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
        bath_ratio >= q3 ~ 1.0,        # First quartile 100-75
        bath_ratio >= q2 ~ 0.5,        # Second quartile 75-50
        bath_ratio >= q1 ~ -0.5,       # Third quartile 50-25
        bath_ratio <  q1 ~ -1.0,       # Forth quartile 25-0
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
           -S01010, -S01005, -V2001, -S01017, -S01020, -S01020A)
  
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
      household_income = as.numeric(as.character(household_income)),
      
      VD4046_num = replace_na(VD4046_num, 0),
      household_income = replace_na(household_income, 0),
      
      head_dependency = if_else(
        household_income > 0, 
        VD4046_num / household_income, 
        0
      )
    ) %>%
    # Drop temporary calculation columns
    select(-VD4046_num)
  
  # 3. Reassign to survey object
  design_obj$variables <- raw_data
  
  return(design_obj)
}

classify_worker_status <- function(design_object) {
  
  updated_design <- update(
    design_object,
    worker_status = case_when(
      # Trabalhador formal
      VD4009 %in% c("01", "03") ~ "trabalhador_formal",
      
      # Servidor público
      VD4009 %in% c("05", "07") ~ "servidor_publico",
      
      # Empregador
      VD4009 %in% c("08") ~ "empregador",
      
      # Informal (Explicit codes)
      VD4009 %in% c("02", "04", "06", "09", "10") ~ "informal",
      
      # Catching explicit NAs and checking if retired
      is.na(VD4009) & V5004A %in% c("1", 1) ~ "aposentado",
      is.na(VD4009) & V5004A %in% c("2", 2) ~ "nao_ocupado",
      
      # Catch-all for any unexpected blanks or undefined codes
      TRUE ~ "nao_informado"
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
        V2007 == "2" & has_child_under_14 == TRUE & has_spouse == FALSE ~ "mae_solteira",
        TRUE ~ "nao_mae_solteira"
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
      as.numeric(as.character(V1023)) %in% c(1, 2, 3) ~ "area_metropolitana",
      as.numeric(as.character(V1023)) %in% c(4) ~ "nao_area_metropolitana",
      TRUE ~ "nao_informado"
    )
  )
  
  return(updated_design)
}

classify_macroregion <- function(design_obj) {
  
  updated_design <- update(
    design_obj,
    # as.character() ensures this works perfectly whether UF is a factor, string, or number
    macroregion = case_when(
      as.character(UF) %in% c(11, 12, 13, 14, 15, 16, 17) ~ "norte",
      as.character(UF) %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29) ~ "nordeste",
      as.character(UF) %in% c(31, 32, 33, 35) ~ "sudeste",
      as.character(UF) %in% c(41, 42, 43) ~ "sul",
      as.character(UF) %in% c(50, 51, 52, 53) ~ "centro-oeste",
      TRUE ~ "nao_informado"
    )
  )
  
  return(updated_design)
}

drop_variables <- function(design_obj, cols_to_drop) {
    design_obj$variables <- design_obj$variables[, !(names(design_obj$variables) %in% cols_to_drop), drop = FALSE]
    
    return(design_obj)
  }

rename_variables <- function(design_obj, rename_mapping) {
  
  # Get current column names
  current_names <- colnames(design_obj$variables)
  
  # Loop through the mapping and replace names
  for (new_name in names(rename_mapping)) {
    old_name <- rename_mapping[[new_name]]
    
    # Find where the old name is and replace it
    current_names[current_names == old_name] <- new_name
  }
  
  # Assign the new names back to the internal dataset
  colnames(design_obj$variables) <- current_names
  
  return(design_obj)
}

remove_missing <- function(design_obj) {
  
  design_valid <- subset(design_obj, tenure_condition != "Outros")
  
  design_valid <- update(
    design_valid, 
    tenure_condition = factor(tenure_condition)
  )
  
  return(design_valid)
}

# 2. MODELLING -------------------------------------------------------------------------------------

get_selected_vars <- function(coefs) {
  selected_vars <- c()
  
  for (i in 1:length(coefs)) {
    matriz_coef <- coefs[[i]]
    # get lines with non zero coefs
    active_name <- rownames(matriz_coef)[which(matriz_coef != 0)]
    selected_vars <- c(selected_vars, active_name)
  }
  
  final_vars <- unique(selected_vars)
  final_vars <- final_vars[final_vars != "(Intercept)"]
  
  return(final_vars)
}

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

create_formula <- function(y, x) {
  right_side <- paste(x, collapse = " + ")
  equation <- paste(y, "~", right_side)
  formula <- as.formula(equation)
  return(formula)
}


## 2.1. PREDICTION  -------------------------------------------------------------------------------------

## =================================================================
## NESTED cluster-respecting K-fold CV for the national multinomial model
## =================================================================

# ----------------------------------------------------------------
# 1. Cluster-respecting fold assignment
#    force_train_ids: cluster IDs that should NEVER be selected as a
#    held-out test fold -- they get fold value 0, which never matches
#    any fold_i in 1:k, so `row_fold != fold_i` is always TRUE for them
#    (always in training) and `row_fold == fold_i` is always FALSE
#    (never in test). Use this for clusters containing the only
#    occurrences of an extremely rare category: evaluating on n<10
#    observations is statistically uninformative regardless of method,
#    so nothing is lost by excluding them from test, while their
#    contribution to model fitting is preserved in every fold.
# ----------------------------------------------------------------
create_cluster_folds <- function(design_obj, cluster_var, k = 10, seed = 123,
                                 force_train_ids = NULL) {
  set.seed(seed)
  
  cluster_ids <- design_obj$variables[[cluster_var]]
  unique_clusters <- unique(cluster_ids)
  
  is_forced <- as.character(unique_clusters) %in% as.character(force_train_ids)
  foldable_clusters <- unique_clusters[!is_forced]
  forced_clusters    <- unique_clusters[is_forced]
  
  foldable_clusters <- sample(foldable_clusters)
  
  fold_assignment <- rep(1:k, length.out = length(foldable_clusters))
  cluster_to_fold  <- setNames(fold_assignment, as.character(foldable_clusters))
  
  if (length(forced_clusters) > 0) {
    forced_map <- setNames(rep(0, length(forced_clusters)), as.character(forced_clusters))
    cluster_to_fold <- c(cluster_to_fold, forced_map)
    message(sprintf("%d cluster(s) forced into training-only (never held out as test).",
                    length(forced_clusters)))
  }
  
  row_fold <- unname(cluster_to_fold[as.character(cluster_ids)])
  # Fold "0" here (if present) is the forced-training-only group, not a
  # real evaluation fold.
  print(table(row_fold))
  
  return(row_fold)
}

# ----------------------------------------------------------------
# 1b. train-only class rebalancing
# ----------------------------------------------------------------
compute_train_class_weights <- function(y_train, base_weight_train) {
  # 1. Natural class proportions, using ONLY this fold's training rows
  frequency  <- table(y_train)
  proportion <- prop.table(frequency)
  
  # 2. Inverse-frequency multiplier, anchored to the least-penalized
  #    (i.e. most common) class = 1x
  inverted_weight <- 1 / proportion
  weight_penalty  <- inverted_weight / min(inverted_weight)
  
  # 3. Map each training row to its class's multiplier
  penalties <- as.vector(weight_penalty[as.character(y_train)])
  penalties[is.na(penalties)] <- 1  # fallback safeguard
  
  # 4. Anchor so the total TRAINING weight is preserved
  raw_total    <- sum(base_weight_train * penalties)
  pop_before   <- sum(base_weight_train)
  scale_factor <- pop_before / raw_total
  penalties_adj <- penalties * scale_factor
  
  list(
    multiplier_raw     = weight_penalty,   # named vector, per class -- for reporting
    multiplier_applied = penalties_adj      # per-row, anchored -- what's actually used
  )
}

# ----------------------------------------------------------------
# 2. Survey-weighted precision, recall, and F1 per class
# ----------------------------------------------------------------
weighted_class_metrics <- function(y_true, y_pred, w, levels_all) {
  y_true <- factor(y_true, levels = levels_all)
  y_pred <- factor(y_pred, levels = levels_all)
  
  out <- data.frame(
    class     = levels_all,
    precision = NA_real_,
    recall    = NA_real_,
    f1        = NA_real_,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(levels_all)) {
    cl <- levels_all[i]
    tp <- sum(w[y_true == cl & y_pred == cl])
    fp <- sum(w[y_true != cl & y_pred == cl])
    fn <- sum(w[y_true == cl & y_pred != cl])
    
    precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
    recall    <- if ((tp + fn) > 0) tp / (tp + fn) else 0
    f1 <- if ((precision + recall) > 0) {
      2 * precision * recall / (precision + recall)
    } else 0
    
    out$precision[i] <- precision
    out$recall[i]     <- recall
    out$f1[i]          <- f1
  }
  
  out
}

# ----------------------------------------------------------------
# 2b. Survey-weighted confusion matrix
# ----------------------------------------------------------------
build_weighted_confusion_matrix <- function(y_true, y_pred, w, levels_all) {
  y_true <- factor(y_true, levels = levels_all)
  y_pred <- factor(y_pred, levels = levels_all)
  
  cm <- matrix(0, nrow = length(levels_all), ncol = length(levels_all),
               dimnames = list(true = levels_all, predicted = levels_all))
  
  for (i in seq_along(levels_all)) {
    for (j in seq_along(levels_all)) {
      cm[i, j] <- sum(w[y_true == levels_all[i] & y_pred == levels_all[j]])
    }
  }
  
  cm  # plain numeric matrix, population-weighted counts (built from ORIGINAL weights)
}

row_normalize_confusion_matrix <- function(cm) {
  pct <- sweep(cm, 1, rowSums(cm), FUN = "/") * 100
  as.matrix(pct)
}

# ----------------------------------------------------------------
# 2c. Consistent factor levels across folds
# ----------------------------------------------------------------
force_factor_levels <- function(df, vars) {
  level_map <- list()
  for (v in vars) {
    if (is.character(df[[v]]) || is.factor(df[[v]])) {
      level_map[[v]] <- levels(factor(df[[v]]))
    }
  }
  level_map
}

apply_factor_levels <- function(df, level_map) {
  for (v in names(level_map)) {
    df[[v]] <- factor(df[[v]], levels = level_map[[v]])
  }
  df
}

# ----------------------------------------------------------------
# 3. Single fold, NESTED + TRAIN-ONLY CLASS REBALANCING
# ----------------------------------------------------------------
run_fold_nested <- function(fold_i, row_fold, df, regression_variables,
                            weight_var, y_var, levels_all, seed,
                            inner_nfolds = 10) {
  
  train_idx <- which(row_fold != fold_i)
  test_idx  <- which(row_fold == fold_i)
  
  train_df <- df[train_idx, ]
  test_df  <- df[test_idx, ]
  
  # .wt = ORIGINAL survey weight for BOTH partitions.
  train_df$.wt <- train_df[[weight_var]]
  test_df$.wt  <- test_df[[weight_var]]
  
  train_df[[y_var]] <- factor(train_df[[y_var]], levels = levels_all)
  test_df[[y_var]]  <- factor(test_df[[y_var]],  levels = levels_all)
  
  # ---- class multiplier computed from TRAIN_DF ONLY.
  #      test_df is never touched by this -- it keeps .wt as the plain
  #      original survey weight, used later only for evaluation.
  class_w <- compute_train_class_weights(
    y_train           = train_df[[y_var]],
    base_weight_train = train_df$.wt
  )
  train_df$.wt_train <- train_df$.wt * class_w$multiplier_applied
  
  # ---- Step A: LASSO variable selection, using ONLY this fold's
  #      training data, weighted by the CLASS-ADJUSTED training weight
  #      (.wt_train), not the plain survey weight.
  X_matrix <- tryCatch(
    model.matrix(~ . - 1, data = train_df[, regression_variables, drop = FALSE]),
    error = function(e) e
  )
  if (inherits(X_matrix, "error")) {
    warning(sprintf("Fold %d: failed to build design matrix: %s",
                    fold_i, conditionMessage(X_matrix)))
    return(NULL)
  }
  
  set.seed(seed + fold_i)  # reproducible inner CV per fold
  lasso_fit <- tryCatch(
    glmnet::cv.glmnet(
      x = X_matrix,
      y = train_df[[y_var]],
      family  = "multinomial",
      weights = train_df$.wt_train,   # <-- class-adjusted, TRAIN ONLY
      nfolds  = inner_nfolds
    ),
    error = function(e) e
  )
  if (inherits(lasso_fit, "error")) {
    warning(sprintf("Fold %d: LASSO failed to fit: %s", fold_i, conditionMessage(lasso_fit)))
    return(NULL)
  }
  
  # lambda.1se: most parsimonious model within one SE of minimum CV
  coefs <- coef(lasso_fit, s = "lambda.1se")
  selected_raw   <- get_selected_vars(coefs)
  selected_clean <- clean_lasso_names(selected_raw, regression_variables)
  
  fold_formula <- create_formula(y_var, selected_clean)
  
  # ---- Step B: refit multinomial on the selected variables, still
  #      using the CLASS-ADJUSTED training weight -- this is the model
  #      that actually gets to "see" the rebalanced classes while
  #      learning coefficients.
  fit <- tryCatch(
    VGAM::vglm(fold_formula, data = train_df, weights = .wt_train,
               family = VGAM::multinomial(refLevel = 1)),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    warning(sprintf("Fold %d: vglm refit failed to converge: %s",
                    fold_i, conditionMessage(fit)))
    return(NULL)
  }
  
  # ---- Prediction is wrapped in tryCatch too: a rare category (e.g. a
  #      residual code with only a handful of national/regional
  #      observations) can have zero rows in this fold's training data
  #      purely by chance, even after force_factor_levels() -- vglm
  #      never learned that level exists, and predictvglm() errors the
  #      moment the held-out fold contains it. Catching this here lets
  #      the fold skip gracefully instead of crashing the whole run.
  #      The force_train_ids mechanism in create_cluster_folds() is the
  #      preferred fix (prevents this from happening at all for known
  #      rare-category clusters); this tryCatch is the safety net for
  #      anything not pre-emptively handled that way.
  pred_probs <- tryCatch(
    VGAM::predictvglm(fit, newdata = test_df, type = "response"),
    error = function(e) e
  )
  
  if (inherits(pred_probs, "error")) {
    warning(sprintf("Fold %d: prediction failed (likely an unseen factor level in the test fold): %s",
                    fold_i, conditionMessage(pred_probs)))
    return(NULL)
  }
  
  if (is.null(dim(pred_probs))) {
    pred_class <- rep(levels_all[1], nrow(test_df))
  } else {
    pred_class <- levels_all[apply(pred_probs, 1, which.max)]
  }
  
  # ---- Evaluation uses test_df$.wt -- the ORIGINAL, UNADJUSTED survey
  #      weight. Reported Macro-F1 / confusion matrices therefore still
  #      represent the true population, even though training used a
  #      class-adjusted weight.
  class_metrics <- weighted_class_metrics(
    y_true     = test_df[[y_var]],
    y_pred     = pred_class,
    w          = test_df$.wt,
    levels_all = levels_all
  )
  
  list(
    fold                = fold_i,
    n_train             = nrow(train_df),
    n_test              = nrow(test_df),
    selected_vars       = selected_clean,
    lambda_1se          = lasso_fit$lambda.1se,
    class_multiplier    = class_w$multiplier_raw,  # per-class multiplier used THIS fold
    macro_f1            = mean(class_metrics$f1),
    class_metrics       = class_metrics,
    y_true              = as.character(test_df[[y_var]]),
    y_pred              = pred_class,
    w                   = test_df$.wt  # original weights, for pooled confusion matrix
  )
}

# ----------------------------------------------------------------
# 4. Full nested pipeline -- national model, no macroregion split
#    force_train_ids: passed straight through to create_cluster_folds()
#    -- see its comments above. Use this for clusters holding the only
#    occurrences of an extremely rare category (e.g. race == "9" with
#    n = 1-8), so they always contribute to training but are never
#    relied upon for evaluation.
# ----------------------------------------------------------------
run_nested_cv <- function(design_obj, regression_variables, cluster_var,
                          y_var = "tenure_condition",
                          k = 10, seed = 123, inner_nfolds = 10,
                          levels_all_override = NULL,
                          force_train_ids = NULL) {
  
  df <- design_obj$variables
  weight_var <- ".sampling_weight"
  df[[weight_var]] <- as.vector(weights(design_obj, "sampling"))  # ORIGINAL weights
  
  levels_all <- if (!is.null(levels_all_override)) {
    levels_all_override
  } else {
    levels(factor(df[[y_var]]))
  }
  
  level_map <- force_factor_levels(df, regression_variables)
  df <- apply_factor_levels(df, level_map)
  
  row_fold <- create_cluster_folds(design_obj, cluster_var, k = k, seed = seed,
                                   force_train_ids = force_train_ids)
  
  fold_results <- vector("list", k)
  for (i in seq_len(k)) {
    message(sprintf("Running NESTED fold %d/%d (train-only class reweight + LASSO + refit)...", i, k))
    fold_results[[i]] <- run_fold_nested(
      fold_i = i, row_fold = row_fold, df = df,
      regression_variables = regression_variables,
      weight_var = weight_var, y_var = y_var, levels_all = levels_all,
      seed = seed, inner_nfolds = inner_nfolds
    )
  }
  
  fold_results <- Filter(Negate(is.null), fold_results)
  if (length(fold_results) < k) {
    warning(sprintf("%d of %d folds failed and were dropped -- see warnings above.",
                    k - length(fold_results), k))
  }
  if (length(fold_results) == 0) {
    stop("All folds failed -- see warnings above for the underlying error(s).")
  }
  
  macro_f1_by_fold <- sapply(fold_results, function(x) x$macro_f1)
  
  all_class_metrics <- do.call(rbind, lapply(fold_results, function(x) {
    cbind(fold = x$fold, x$class_metrics)
  }))
  class_table <- aggregate(cbind(precision, recall, f1) ~ class,
                           data = all_class_metrics, FUN = mean)
  class_table_sd <- aggregate(cbind(precision, recall, f1) ~ class,
                              data = all_class_metrics, FUN = sd)
  names(class_table_sd)[-1] <- paste0(names(class_table_sd)[-1], "_sd")
  class_table <- merge(class_table, class_table_sd, by = "class")
  class_table <- class_table[match(levels_all, class_table$class), ]
  rownames(class_table) <- NULL
  
  all_selected <- unlist(lapply(fold_results, function(x) x$selected_vars))
  selection_freq <- as.data.frame(table(all_selected))
  names(selection_freq) <- c("variable", "times_selected")
  selection_freq$out_of_k <- length(fold_results)
  selection_freq <- selection_freq[order(-selection_freq$times_selected), ]
  rownames(selection_freq) <- NULL
  
  # ---- per-fold class multipliers, so the write-up can report how
  #      much the class rebalancing varied depending on which 90% of the
  #      sample was used to compute it (transparency check, same spirit
  #      as selection_freq above).
  class_multiplier_table <- do.call(rbind, lapply(fold_results, function(x) {
    data.frame(fold = x$fold, class = names(x$class_multiplier),
               multiplier = as.vector(x$class_multiplier))
  }))
  
  pooled_y_true <- unlist(lapply(fold_results, function(x) x$y_true))
  pooled_y_pred <- unlist(lapply(fold_results, function(x) x$y_pred))
  pooled_w      <- unlist(lapply(fold_results, function(x) x$w))  # original weights
  
  confusion_matrix     <- build_weighted_confusion_matrix(
    pooled_y_true, pooled_y_pred, pooled_w, levels_all
  )
  confusion_matrix_pct <- row_normalize_confusion_matrix(confusion_matrix)
  
  list(
    mean_macro_f1           = mean(macro_f1_by_fold),
    sd_macro_f1             = sd(macro_f1_by_fold),
    per_fold                = macro_f1_by_fold,
    class_table             = class_table,
    selection_freq          = selection_freq,
    class_multiplier_table  = class_multiplier_table,
    confusion_matrix        = confusion_matrix,      # built from ORIGINAL weights
    confusion_matrix_pct    = confusion_matrix_pct,  # row-normalized, % of true population
    fold_details            = fold_results
  )
}


## 2.2. INTERPRETATION -------------------------------------------------------------------------------------

create_matrix_national <- function(design_obj) {
  
  # Define preditors
  regression_variables <- c("age", "race", "household_size", "family_structure", "household_income", "household_income_pcapita", "education_years", "wealth_index", "head_dependency", "worker_status", "single_mom", "metropolitan_area", "macroregion")
  
  # Use the dataframe
  pnadc_df <- design_obj$variables
  
  Y <- pnadc_df$tenure_condition # dependent variables
  X <- pnadc_df[, regression_variables] # predictors
  pnadc_weights <- weights(design_obj, "sampling") # weights
  
  stopifnot(length(pnadc_weights) == nrow(X))
  
  # Perform one hot encoding
  
  X_matrix <- model.matrix(~ . - 1, data = X)
  
  return(list(
    X_matrix = X_matrix,
    Y = Y,
    weights = pnadc_weights
  ))
}

run_lasso_national <- function(matrix_national) {
  set.seed(123)
  
  lasso <- cv.glmnet(x = matrix_national$X_matrix,
                     y = matrix_national$Y,
                     family = "multinomial",
                     weights = matrix_national$weights)
  
  return(lasso)
}

create_matrix_regional <- function(subset_df) {
  
  # Define preditors
  regression_variables <- c("age", "race", "household_size", "family_structure", "household_income", "household_income_pcapita", "education_years", "wealth_index", "head_dependency", "worker_status", "single_mom", "metropolitan_area")
  
  Y <- subset_df$tenure_condition # dependent variables
  X <- subset_df[, regression_variables] # predictors
  subset_weights <- weights(subset_df, "sampling") # weights
  
  # check for columns with only 1 unique value
  valid_columns <- sapply(X, function(col) length(unique(na.omit(col))) > 1)
    dropped_cols <- names(valid_columns)[!valid_columns]
  if (length(dropped_cols) > 0) {
    cat("Dropping constant variable(s):", paste(dropped_cols, collapse = ", "), "\n")
  }
  X <- X[, valid_columns, drop = FALSE]
  
  # Perform one hot encoding
  X_matrix <- model.matrix(~ . - 1, data = X)
  
  return(list(
    X_matrix = X_matrix,
    Y = Y,
    weights = subset_weights
  ))
}

run_regional_lasso <- function(survey_object) {
  set.seed(123)
  
  # Extract the flat data frame
  survey_df <- survey_object$variables
  
  # Define the regions
  regions <- c("norte", "nordeste", "sudeste", "sul", "centro-oeste")
  
  # Create a list to store the full cv.glmnet model objects
  lasso_models <- list()
  
  for (reg in regions) {
    cat(sprintf("Processing region: %s...\n", reg))
    
    # Isolate the specific macroregion
    temp_df <- subset(survey_df, macroregion == reg)
    
    # Prepare the matrices and vectors
    lasso_data <- create_matrix_regional(temp_df)
    
    # Run the LASSO
    cv_lasso <- cv.glmnet(
      x = lasso_data$X_matrix,
      y = lasso_data$Y,
      family = "multinomial",
      weights = lasso_data$weights
    )
    
    # saving the model
    lasso_models[[reg]] <- cv_lasso
    
    # RAM Cleanup
    rm(temp_df, lasso_data, cv_lasso)
    gc()
  }
  
  cat("All regional LASSO models adjusted successfully!\n")
  return(lasso_models)
}