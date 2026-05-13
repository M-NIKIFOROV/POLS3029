# 02_regression_lagged.R
# CRE (Mundlak) regressions with lagged time-varying covariates
# Lags: 1, 2, 3 years
# Output: 02_output/02_regression_lagged.txt

setwd("c:/Users/maxim/OneDrive/Documents/University - Year 3/POLS3029/paper/POLS3029")

library(sandwich)
library(lmtest)

master <- read.csv("02_output/master_panel.csv", stringsAsFactors = FALSE)
master <- master[order(master$ccode, master$year), ]
rownames(master) <- NULL

cat("Master panel loaded:", nrow(master), "rows\n")

dvs <- c("SE_GE", "STR_GTR", "NT_SE", "NT_SR", "RAI", "SelfR", "SharedR")
time_varying <- c("HL", "F", "R", "logG", "D", "logP", "LD")
main_effects <- c("HL", "F", "R", "logG", "D", "logP", "LD", "Fed", "B")

lag_by_group <- function(x, g, k) {
  ave(x, g, FUN = function(v) {
    n <- length(v)
    if (n <= k) return(rep(NA_real_, n))
    c(rep(NA_real_, k), v[1:(n - k)])
  })
}

compute_mundlak_means <- function(data, vars) {
  means <- aggregate(data[, vars, drop = FALSE],
                     by = list(ccode = data$ccode),
                     FUN = function(x) {
                       m <- mean(x, na.rm = TRUE)
                       if (is.nan(m)) NA_real_ else m
                     })
  names(means)[-1] <- paste0("mean_", vars)
  means
}

fit_cre_model_lag <- function(dv_name, data, lag_k) {
  lag_vars <- paste0(time_varying, "_L", lag_k)
  mean_vars <- paste0("mean_", lag_vars)

  formula_str <- paste0(
    dv_name,
    " ~ ",
    paste(c(lag_vars, "Fed", "B", mean_vars, "ccode_f", "year_f"), collapse = " + ")
  )

  model <- tryCatch(
    lm(as.formula(formula_str), data = data),
    error = function(e) NULL
  )

  if (is.null(model)) return(NULL)

  mf <- model.frame(model)
  n_obs <- nrow(mf)
  if (n_obs == 0) return(NULL)

  n_countries <- length(unique(as.character(mf$ccode_f)))

  vcov_cluster <- vcovCL(model, cluster = as.character(mf$ccode_f))
  se_cluster <- sqrt(diag(vcov_cluster))
  coef_model <- coef(model)

  coef_lookup <- c(paste0(time_varying, "_L", lag_k), "Fed", "B")
  names(coef_lookup) <- main_effects

  result <- data.frame(
    Variable = main_effects,
    Coefficient = NA_real_,
    SE = NA_real_,
    t_stat = NA_real_,
    p_value = NA_real_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(main_effects)) {
    term <- coef_lookup[i]
    if (term %in% names(coef_model) && term %in% names(se_cluster)) {
      b <- coef_model[term]
      s <- se_cluster[term]
      t <- b / s
      p <- 2 * (1 - pt(abs(t), df = n_obs - length(coef_model)))

      result$Coefficient[i] <- b
      result$SE[i] <- s
      result$t_stat[i] <- t
      result$p_value[i] <- p
    }
  }

  list(
    dv = dv_name,
    n_obs = n_obs,
    n_countries = n_countries,
    r2 = summary(model)$r.squared,
    adj_r2 = summary(model)$adj.r.squared,
    coefs = result
  )
}

format_model_table <- function(models_list) {
  output_table <- data.frame(Variable = main_effects, stringsAsFactors = FALSE)

  for (dv in dvs) {
    output_table[[paste0(dv, "_coef")]] <- NA_character_
    output_table[[paste0(dv, "_se")]] <- NA_character_

    if (!is.null(models_list[[dv]])) {
      coefs <- models_list[[dv]]$coefs
      for (i in seq_along(main_effects)) {
        var <- main_effects[i]
        idx <- which(coefs$Variable == var)
        if (length(idx) == 1 && !is.na(coefs$Coefficient[idx])) {
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

          output_table[[paste0(dv, "_coef")]][i] <- sprintf("%.4f%s", coef_val, sig_mark)
          output_table[[paste0(dv, "_se")]][i] <- sprintf("(%.4f)", se_val)
        }
      }
    }
  }

  summary_rows <- data.frame(
    Variable = c("N", "Countries", "R-squared", "Adj R-squared"),
    stringsAsFactors = FALSE
  )

  for (dv in dvs) {
    summary_rows[[paste0(dv, "_coef")]] <- ""
    summary_rows[[paste0(dv, "_se")]] <- ""
    if (!is.null(models_list[[dv]])) {
      summary_rows[[paste0(dv, "_coef")]] <- c(
        as.character(models_list[[dv]]$n_obs),
        as.character(models_list[[dv]]$n_countries),
        sprintf("%.4f", models_list[[dv]]$r2),
        sprintf("%.4f", models_list[[dv]]$adj_r2)
      )
    }
  }

  rbind(output_table, summary_rows)
}

print_table <- function(output_full) {
  cat("REGRESSION TABLE:\n")
  cat(paste(rep("-", 120), collapse = ""), "\n")

  header <- "Variable"
  for (dv in dvs) header <- paste0(header, sprintf("%20s", dv))
  cat(header, "\n")
  cat(paste(rep("-", 120), collapse = ""), "\n")

  for (i in seq_len(nrow(output_full))) {
    row_str <- sprintf("%-10s", output_full$Variable[i])
    for (dv in dvs) {
      coef_col <- paste0(dv, "_coef")
      coef_str <- as.character(output_full[[coef_col]][i])
      row_str <- paste0(row_str, sprintf("%20s", coef_str))
    }
    cat(row_str, "\n")

    if (!output_full$Variable[i] %in% c("N", "Countries", "R-squared", "Adj R-squared")) {
      se_row <- sprintf("%-10s", "")
      for (dv in dvs) {
        se_col <- paste0(dv, "_se")
        se_str <- as.character(output_full[[se_col]][i])
        se_row <- paste0(se_row, sprintf("%20s", se_str))
      }
      cat(se_row, "\n")
    }
  }

  cat(paste(rep("-", 120), collapse = ""), "\n")
}

run_lag_models <- function(lag_k, base_data) {
  data <- base_data

  lag_vars <- paste0(time_varying, "_L", lag_k)
  for (i in seq_along(time_varying)) {
    data[[lag_vars[i]]] <- lag_by_group(data[[time_varying[i]]], data$ccode, lag_k)
  }

  means <- compute_mundlak_means(data, lag_vars)
  data <- merge(data, means, by = "ccode", all.x = TRUE)

  data$year_f <- as.factor(data$year)
  data$ccode_f <- as.factor(data$ccode)

  models <- list()
  for (dv in dvs) {
    models[[dv]] <- fit_cre_model_lag(dv, data, lag_k)
  }

  models
}

output_file <- "02_output/02_regression_lagged.txt"
sink(output_file)

cat("================================================================================\n")
cat("CRE (Mundlak) REGRESSION RESULTS - LAGGED SPECIFICATIONS\n")
cat("Dependent Variables: SE_GE, STR_GTR, NT_SE, NT_SR, RAI, SelfR, SharedR\n")
cat("Panel: 1990-2014\n")
cat("Estimation: Linear models with country and year fixed effects\n")
cat("Standard Errors: Clustered at country level\n")
cat("Mundlak Terms: Country means of lagged time-varying covariates included (not shown)\n")
cat("Significance codes: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10\n")
cat("================================================================================\n\n")

for (lag_k in 1:3) {
  cat(sprintf("\n============================== LAG %d YEAR(S) ==============================\n\n", lag_k))
  models <- run_lag_models(lag_k, master)
  table_out <- format_model_table(models)
  print_table(table_out)
}

cat("\nESTIMATION COMPLETE.\n")

sink()

cat(sprintf("Lagged regression tables written to: %s\n", output_file))
