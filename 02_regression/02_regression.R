# 02_regression/02_regression.R
# Run 2x2 baseline CRE model matrix from model_config.R
# Output: 02_regression/02_output/02_regression.txt

setwd("c:/Users/maxim/OneDrive/Documents/University - Year 3/POLS3029/paper/POLS3029")

library(sandwich)
library(lmtest)

source("model_config.R")

# ----------------------------------------------------------------------------
# Load panel data
# ----------------------------------------------------------------------------

panel_path <- "clean_data/model_panel_clean.csv"
if (!file.exists(panel_path)) {
  stop("Missing clean_data/model_panel_clean.csv. Run 02_regression/01_build_panel.R first.")
}

panel <- read.csv(panel_path, stringsAsFactors = FALSE)

# Prepare FE factors
panel$ccode_f <- as.factor(panel$ccode)
panel$year_f <- as.factor(panel$year)

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

sig_mark <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  if (p < 0.10) return(".")
  ""
}

fit_one_model <- function(data, conflict_key, centralisation_key, cfg = MODEL_CONFIG) {
  f <- build_formula_by_keys(conflict_key, centralisation_key, cfg)

  vars_needed <- unique(all.vars(f))
  vars_needed <- vars_needed[vars_needed %in% names(data)]

  dat <- data[complete.cases(data[, vars_needed, drop = FALSE]), ]
  if (nrow(dat) == 0) {
    stop(sprintf("No complete cases for model %s x %s", conflict_key, centralisation_key))
  }

  m <- lm(f, data = dat)
  vc <- vcovCL(m, cluster = dat$ccode)
  ct <- coeftest(m, vcov. = vc)

  # Track terms expected by the formula but omitted from estimation output
  # (typically due to singularity / no in-sample variation).
  mm_terms <- colnames(model.matrix(m))
  omitted_terms <- setdiff(mm_terms, rownames(ct))

  # Keep full CRE coefficients but drop FE dummies for readability
  keep <- !grepl("^ccode_f|^year_f", rownames(ct))
  ct <- ct[keep, , drop = FALSE]

  coefs <- data.frame(
    term = rownames(ct),
    estimate = as.numeric(ct[, 1]),
    std_error = as.numeric(ct[, 2]),
    t_value = as.numeric(ct[, 3]),
    p_value = as.numeric(ct[, 4]),
    stringsAsFactors = FALSE
  )
  rownames(coefs) <- NULL

  list(
    conflict_key = conflict_key,
    conflict_label = cfg$conflict[[conflict_key]]$label,
    centralisation_key = centralisation_key,
    centralisation_label = cfg$centralisation[[centralisation_key]]$label,
    formula = deparse(f),
    n = nrow(dat),
    countries = length(unique(dat$ccode)),
    r2 = summary(m)$r.squared,
    adj_r2 = summary(m)$adj.r.squared,
    omitted_terms = omitted_terms,
    coefs = coefs
  )
}

# ----------------------------------------------------------------------------
# Run model matrix
# ----------------------------------------------------------------------------

results <- list()
for (i in seq_along(MODEL_CONFIG$model_matrix)) {
  spec <- MODEL_CONFIG$model_matrix[[i]]
  key <- paste(spec$conflict, spec$centralisation, sep = "__")
  results[[key]] <- fit_one_model(
    data = panel,
    conflict_key = spec$conflict,
    centralisation_key = spec$centralisation,
    cfg = MODEL_CONFIG
  )
}

# ----------------------------------------------------------------------------
# Write output
# ----------------------------------------------------------------------------

dir.create("02_regression/02_output", recursive = TRUE, showWarnings = FALSE)
out_file <- "02_regression/02_output/02_regression.txt"

sink(out_file)

cat("================================================================================\n")
cat("CRE 2x2 MODEL MATRIX RESULTS\n")
cat(sprintf("Conflict indicators: %s\n", paste(unique(vapply(MODEL_CONFIG$model_matrix, function(x) x$conflict, character(1))), collapse = ", ")))
cat(sprintf("Centralisation outcomes: %s\n", paste(unique(vapply(MODEL_CONFIG$model_matrix, function(x) x$centralisation, character(1))), collapse = ", ")))
cat("Specification: Y_it = Conflict_it + Conflict_it x Fed_i + logG_it + logP_it + country FE + year FE\n")
cat("SE: Country-clustered (vcovCL)\n")
cat("Reporting: country/year FE coefficients omitted for readability\n")
cat("================================================================================\n\n")

model_names <- names(results)
for (i in seq_along(model_names)) {
  r <- results[[model_names[i]]]

  cat(sprintf("MODEL %d\n", i))
  cat("--------------------------------------------------------------------------------\n")
  cat(sprintf("Conflict: %s (%s)\n", r$conflict_key, r$conflict_label))
  cat(sprintf("Outcome : %s (%s)\n", r$centralisation_key, r$centralisation_label))
  cat(sprintf("Formula : %s\n", paste(r$formula, collapse = " ")))
  cat(sprintf("N=%d | Countries=%d | R2=%.4f | Adj.R2=%.4f\n", r$n, r$countries, r$r2, r$adj_r2))
  cat("\n")

  cat(sprintf("%-35s %14s %14s %12s %12s\n", "Term", "Coef", "SE", "t", "p"))
  cat(paste(rep("-", 95), collapse = ""), "\n", sep = "")

  for (j in seq_len(nrow(r$coefs))) {
    rr <- r$coefs[j, ]
    est <- sprintf("%.6f%s", rr$estimate, sig_mark(rr$p_value))
    se <- sprintf("(%.6f)", rr$std_error)
    tval <- sprintf("%.3f", rr$t_value)
    pval <- sprintf("%.5f", rr$p_value)
    cat(sprintf("%-35s %14s %14s %12s %12s\n", rr$term, est, se, tval, pval))
  }

  dropped_non_fe <- r$omitted_terms[!grepl("^ccode_f|^year_f", r$omitted_terms)]
  if (length(dropped_non_fe) > 0) {
    cat("\nDropped (not estimable in-sample): ", paste(dropped_non_fe, collapse = ", "), "\n", sep = "")
  }

  cat("\n")
}

cat("Significance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10\n")
cat("\nESTIMATION COMPLETE.\n")

sink()

cat(sprintf("Results written to: %s\n", out_file))
