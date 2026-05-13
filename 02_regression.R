# 02_regression.R
# CRE (Correlated Random Effects / Mundlak) regressions
# Runs 7 models with different DVs, outputs combined table
# Output: 02_output/02_regression.txt

setwd("c:/Users/maxim/OneDrive/Documents/University - Year 3/POLS3029/paper/POLS3029")

library(sandwich)
library(lmtest)

# Load master panel
master <- read.csv("02_output/master_panel.csv", stringsAsFactors = FALSE)

cat("Master panel loaded:", nrow(master), "rows\n")

# Time-varying covariates for Mundlak terms (main spec excludes HL)
time_varying <- c("F", "R", "logG", "D", "logP", "LD")

# ============================================================================
# Compute Mundlak means (country-level means of time-varying X's)
# ============================================================================

cat("Computing Mundlak means...\n")

mundlak_means <- aggregate(master[, time_varying, drop = FALSE],
                           by = list(ccode = master$ccode),
                           FUN = function(x) mean(x, na.rm = TRUE))
names(mundlak_means)[-1] <- paste0("mean_", names(mundlak_means)[-1])

# Merge back into master
master <- merge(master, mundlak_means, by = "ccode", all.x = TRUE)

# ============================================================================
# Prepare year dummies
# ============================================================================

master$year_f <- as.factor(master$year)
master$ccode_f <- as.factor(master$ccode)

# ============================================================================
# Function to fit CRE model and extract results
# ============================================================================

fit_cre_model <- function(dv_name, data) {
  # Remove rows with missing DV
  data_clean <- data[!is.na(data[[dv_name]]), ]
  
  n_countries <- length(unique(data_clean$ccode))
  n_obs <- nrow(data_clean)
  
  # Formula: DV ~ time-varying X's + time-invariant X's + Mundlak means + year FE
  # Note: ccode FE is implicit in the model specification; we use lm + lmtest for clustering
  
  formula_str <- sprintf(
    "%s ~ F + R + logG + D + logP + LD + Fed + B + F:Fed + 
            mean_F + mean_R + mean_logG + mean_D + mean_logP + mean_LD + 
            ccode_f + year_f",
    dv_name
  )
  
  # Fit model
  tryCatch({
    model <- lm(formula(formula_str), data = data_clean)
    
    # Extract coefficients and cluster-robust SE (country level)
    vcov_cluster <- vcovCL(model, cluster = data_clean$ccode)
    se_cluster <- sqrt(diag(vcov_cluster))
    coef_model <- coef(model)
    
    # Extract main effects (not FE's)
    main_effects <- c("F", "FedxF", "R", "logG", "D", "logP", "LD", "Fed", "B")
    
    result <- data.frame(
      Variable = main_effects,
      Coefficient = NA_real_,
      SE = NA_real_,
      t_stat = NA_real_,
      p_value = NA_real_,
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(main_effects)) {
      var <- main_effects[i]
      var_name <- var
      if (var == "FedxF") {
        if ("F:Fed" %in% names(coef_model)) var_name <- "F:Fed"
        else if ("Fed:F" %in% names(coef_model)) var_name <- "Fed:F"
      }

      if (var_name %in% names(coef_model)) {
        result$Coefficient[i] <- coef_model[var_name]
        result$SE[i] <- se_cluster[var_name]
        result$t_stat[i] <- result$Coefficient[i] / result$SE[i]
        result$p_value[i] <- 2 * (1 - pt(abs(result$t_stat[i]), df = n_obs - length(coef_model)))
      }
    }
    
    # Extract Mundlak mean coefficients
    mundlak_effects <- c("mean_F", "mean_R", "mean_logG", "mean_D", "mean_logP", "mean_LD")
    mundlak_result <- data.frame(
      Variable = mundlak_effects,
      Coefficient = NA_real_,
      SE = NA_real_,
      t_stat = NA_real_,
      p_value = NA_real_,
      stringsAsFactors = FALSE
    )

    for (i in seq_along(mundlak_effects)) {
      var <- mundlak_effects[i]
      if (var %in% names(coef_model)) {
        mundlak_result$Coefficient[i] <- coef_model[var]
        mundlak_result$SE[i] <- se_cluster[var]
        mundlak_result$t_stat[i] <- mundlak_result$Coefficient[i] / mundlak_result$SE[i]
        mundlak_result$p_value[i] <- 2 * (1 - pt(abs(mundlak_result$t_stat[i]), df = n_obs - length(coef_model)))
      }
    }

    # Hausman-style Wald test for CRE terms: H0 all Mundlak means = 0
    m_idx <- which(names(coef_model) %in% mundlak_effects)
    m_beta <- coef_model[m_idx]
    m_vcov <- vcov_cluster[m_idx, m_idx, drop = FALSE]
    wald_stat <- as.numeric(t(m_beta) %*% solve(m_vcov) %*% m_beta)
    wald_df <- length(m_idx)
    wald_p <- pchisq(wald_stat, df = wald_df, lower.tail = FALSE)

    # Summary statistics
    r2 <- summary(model)$r.squared
    adj_r2 <- summary(model)$adj.r.squared
    
    list(
      dv = dv_name,
      n_countries = n_countries,
      n_obs = n_obs,
      r2 = r2,
      adj_r2 = adj_r2,
      coefs = result,
      mundlak_coefs = mundlak_result,
      hausman_wald = data.frame(stat = wald_stat, df = wald_df, p = wald_p),
      model = model
    )
  }, error = function(e) {
    cat(sprintf("ERROR in %s: %s\n", dv_name, e$message))
    NULL
  })
}

# ============================================================================
# Fit all 7 models
# ============================================================================

cat("Fitting CRE models...\n")

dvs <- c("SE_GE", "STR_GTR", "NT_SE", "NT_SR", "RAI", "SelfR", "SharedR")
models_list <- list()

for (dv in dvs) {
  cat(sprintf("  Fitting %s...\n", dv))
  models_list[[dv]] <- fit_cre_model(dv, master)
}

# ============================================================================
# Create combined output table
# ============================================================================

cat("Creating combined regression table...\n")

# Create a wide table with one column per DV
main_effects_all <- c("F", "FedxF", "R", "logG", "D", "logP", "LD", "Fed", "B")

output_table <- data.frame(
  Variable = main_effects_all,
  stringsAsFactors = FALSE
)

# Add columns for each DV (coef and SE)
for (dv in dvs) {
  if (!is.null(models_list[[dv]])) {
    coefs <- models_list[[dv]]$coefs
    for (i in seq_along(main_effects_all)) {
      var <- main_effects_all[i]
      idx <- which(coefs$Variable == var)
      if (length(idx) > 0) {
        coef_val <- coefs$Coefficient[idx]
        se_val <- coefs$SE[idx]
        pval <- coefs$p_value[idx]
        
        sig_mark <- ""
        if (!is.na(pval)) {
          if (pval < 0.001) sig_mark <- "***"
          else if (pval < 0.01) sig_mark <- "**"
          else if (pval < 0.05) sig_mark <- "*"
          else if (pval < 0.10) sig_mark <- "."
        }
        
        output_table[[paste0(dv, "_coef")]][i] <- 
          sprintf("%.4f%s", ifelse(is.na(coef_val), NA, coef_val), sig_mark)
        output_table[[paste0(dv, "_se")]][i] <- 
          sprintf("(%.4f)", ifelse(is.na(se_val), NA, se_val))
      }
    }
  }
}

# Add summary rows
summary_rows <- data.frame(
  Variable = c("N", "Countries", "R-squared", "Adj R-squared"),
  stringsAsFactors = FALSE
)

for (dv in dvs) {
  if (!is.null(models_list[[dv]])) {
    summary_rows[[paste0(dv, "_coef")]] <- c(
      models_list[[dv]]$n_obs,
      models_list[[dv]]$n_countries,
      sprintf("%.4f", models_list[[dv]]$r2),
      sprintf("%.4f", models_list[[dv]]$adj_r2)
    )
    summary_rows[[paste0(dv, "_se")]] <- ""
  }
}

# Combine
output_full <- rbind(output_table, summary_rows)

# ============================================================================
# Write to text file in a readable format
# ============================================================================

output_file <- "02_output/02_regression.txt"

sink(output_file)

cat("================================================================================\n")
cat("CRE (Mundlak) REGRESSION RESULTS\n")
cat("Dependent Variables: SE_GE, STR_GTR, NT_SE, NT_SR, RAI, SelfR, SharedR\n")
cat("Panel: 1990-2014\n")
cat("Estimation: Linear models with country and year fixed effects\n")
cat("Standard Errors: Clustered at country level\n")
cat("Mundlak Terms: Country means of included time-varying covariates shown below\n")
cat("================================================================================\n\n")

cat("VARIABLE DEFINITIONS:\n")
cat("F = Fatalities (conflict)\n")
cat("FedxF = Interaction term Fed x F\n")
cat("R = Revisionist state (dummy)\n")
cat("logG = Log GDP per capita\n")
cat("D = Debt-to-GDP ratio\n")
cat("logP = Log population\n")
cat("LD = Liberal democracy score\n")
cat("Fed = Federal system (dummy)\n")
cat("B = Bicameral legislature (dummy)\n\n")

cat("Significance codes: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10\n\n")

cat("REGRESSION TABLE:\n")
cat(paste(rep("-", 120), collapse = ""), "\n")

# Print header
header <- "Variable"
for (dv in dvs) {
  header <- paste0(header, sprintf("%20s", dv))
}
cat(header, "\n")

cat(paste(rep("-", 120), collapse = ""), "\n")

# Print rows
for (i in seq_len(nrow(output_full))) {
  row_str <- sprintf("%-10s", output_full$Variable[i])
  for (dv in dvs) {
    coef_col <- paste0(dv, "_coef")
    se_col <- paste0(dv, "_se")
    
    if (coef_col %in% names(output_full)) {
      coef_str <- as.character(output_full[[coef_col]][i])
      row_str <- paste0(row_str, sprintf("%20s", coef_str))
    }
  }
  cat(row_str, "\n")
  
  # Print SE row if not summary
  if (!output_full$Variable[i] %in% c("N", "Countries", "R-squared", "Adj R-squared")) {
    se_row_str <- sprintf("%-10s", "")
    for (dv in dvs) {
      se_col <- paste0(dv, "_se")
      if (se_col %in% names(output_full)) {
        se_str <- as.character(output_full[[se_col]][i])
        se_row_str <- paste0(se_row_str, sprintf("%20s", se_str))
      }
    }
    cat(se_row_str, "\n")
  }
}

cat(paste(rep("-", 120), collapse = ""), "\n")
cat("\nESTIMATION COMPLETE.\n")

cat("\n")
cat("================================================================================\n")
cat("MUNDLAK MEAN COEFFICIENTS (COUNTRY MEANS)\n")
cat("================================================================================\n\n")

mundlak_effects_all <- c("mean_F", "mean_R", "mean_logG", "mean_D", "mean_logP", "mean_LD")

cat(paste(rep("-", 120), collapse = ""), "\n")
header2 <- "Variable"
for (dv in dvs) {
  header2 <- paste0(header2, sprintf("%20s", dv))
}
cat(header2, "\n")
cat(paste(rep("-", 120), collapse = ""), "\n")

for (var in mundlak_effects_all) {
  row_str <- sprintf("%-10s", var)
  se_row_str <- sprintf("%-10s", "")
  for (dv in dvs) {
    mtab <- models_list[[dv]]$mundlak_coefs
    idx <- which(mtab$Variable == var)
    coef_val <- mtab$Coefficient[idx]
    se_val <- mtab$SE[idx]
    pval <- mtab$p_value[idx]

    sig_mark <- ""
    if (!is.na(pval)) {
      if (pval < 0.001) sig_mark <- "***"
      else if (pval < 0.01) sig_mark <- "**"
      else if (pval < 0.05) sig_mark <- "*"
      else if (pval < 0.10) sig_mark <- "."
    }

    row_str <- paste0(row_str, sprintf("%20s", sprintf("%.4f%s", coef_val, sig_mark)))
    se_row_str <- paste0(se_row_str, sprintf("%20s", sprintf("(%.4f)", se_val)))
  }
  cat(row_str, "\n")
  cat(se_row_str, "\n")
}
cat(paste(rep("-", 120), collapse = ""), "\n")

cat("\n")
cat("================================================================================\n")
cat("HAUSMAN-TYPE WALD TEST FOR CRE TERMS\n")
cat("H0: mean_F = mean_R = mean_logG = mean_D = mean_logP = mean_LD = 0\n")
cat("(Rejection supports CRE over standard RE assumptions)\n")
cat("================================================================================\n\n")

cat(sprintf("%-12s %15s %8s %15s\n", "Model", "Chi-square", "df", "p-value"))
cat(paste(rep("-", 60), collapse = ""), "\n")
for (dv in dvs) {
  h <- models_list[[dv]]$hausman_wald
  p_mark <- ifelse(h$p < 0.001, "***", ifelse(h$p < 0.01, "**", ifelse(h$p < 0.05, "*", ifelse(h$p < 0.10, ".", ""))))
  cat(sprintf("%-12s %15.3f %8d %15.5f%s\n", dv, h$stat, h$df, h$p, p_mark))
}
cat(paste(rep("-", 60), collapse = ""), "\n")

sink()

cat(sprintf("Regression table written to: %s\n", output_file))
