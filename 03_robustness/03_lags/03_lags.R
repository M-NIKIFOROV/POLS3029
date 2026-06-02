# Run 1-year, 2-year, and 3-year lag robustness models from model_config.R
# Output: 03_robustness/03_lags/03_lags_output/03_lags.txt

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
panel <- panel[order(panel$ccode, panel$year), ]

# Prepare lagged source variables by country
lag_lengths <- 1:3
conflict_sources <- unique(vapply(MODEL_CONFIG$conflict, function(x) x$source_variable, character(1)))

for (src in conflict_sources) {
  for (lag_k in lag_lengths) {
    lag_name <- sprintf("%s_l%d", src, lag_k)
    panel[[lag_name]] <- ave(
      panel[[src]],
      panel$ccode,
      FUN = function(x) c(rep(NA, lag_k), head(x, -lag_k))
    )
  }
}

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

build_lagged_conflict_term <- function(conflict_key, lag_k, cfg = MODEL_CONFIG) {
  spec <- cfg$conflict[[conflict_key]]
  src <- spec$source_variable
  lagged_src <- sprintf("%s_l%d", src, lag_k)
  gsub(sprintf("\\b%s\\b", src), lagged_src, spec$transform)
}

build_lagged_formula_by_keys <- function(conflict_key, centralisation_key, lag_k, cfg = MODEL_CONFIG) {
  conflict_term <- build_lagged_conflict_term(conflict_key, lag_k, cfg)
  outcome_term <- get_outcome_term_by_key(centralisation_key, cfg)
  fed <- cfg$federal_indicator

  rhs <- c(
    conflict_term,
    sprintf("(%s):%s", conflict_term, fed),
    cfg$controls,
    cfg$fixed_effects
  )

  formula_txt <- sprintf("%s ~ %s", outcome_term, paste(rhs, collapse = " + "))
  as.formula(formula_txt)
}

fit_one_lag_model <- function(data, conflict_key, centralisation_key, lag_k, cfg = MODEL_CONFIG) {
  f <- build_lagged_formula_by_keys(conflict_key, centralisation_key, lag_k, cfg)

  vars_needed <- unique(all.vars(f))
  vars_needed <- vars_needed[vars_needed %in% names(data)]

  dat <- data[complete.cases(data[, vars_needed, drop = FALSE]), ]
  if (nrow(dat) == 0) {
    stop(sprintf("No complete cases for lag %d model %s x %s", lag_k, conflict_key, centralisation_key))
  }

  m <- lm(f, data = dat)
  vc <- vcovCL(m, cluster = dat$ccode)
  ct <- coeftest(m, vcov. = vc)

  mm_terms <- colnames(model.matrix(m))
  omitted_terms <- setdiff(mm_terms, rownames(ct))

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
    lag = lag_k,
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
# Run lag model matrix
# ----------------------------------------------------------------------------

results <- list()
for (lag_k in lag_lengths) {
  for (i in seq_along(MODEL_CONFIG$model_matrix)) {
    spec <- MODEL_CONFIG$model_matrix[[i]]
    key <- paste0("l", lag_k, "__", spec$conflict, "__", spec$centralisation)
    results[[key]] <- fit_one_lag_model(
      data = panel,
      conflict_key = spec$conflict,
      centralisation_key = spec$centralisation,
      lag_k = lag_k,
      cfg = MODEL_CONFIG
    )
  }
}

# ----------------------------------------------------------------------------
# Write output
# ----------------------------------------------------------------------------

dir.create("03_robustness/03_lags/03_lags_output", recursive = TRUE, showWarnings = FALSE)
out_file <- "03_robustness/03_lags/03_lags_output/03_lags.txt"

sink(out_file)

cat("================================================================================\n")
cat("LAG ROBUSTNESS MODEL RESULTS\n")
cat(sprintf("Conflict indicators: %s\n", paste(unique(vapply(MODEL_CONFIG$model_matrix, function(x) x$conflict, character(1))), collapse = ", ")))
cat(sprintf("Centralisation outcomes: %s\n", paste(unique(vapply(MODEL_CONFIG$model_matrix, function(x) x$centralisation, character(1))), collapse = ", ")))
cat("Lags estimated: 1, 2, 3 years\n")
cat("Specification: Y_it = Conflict_i,t-k + Conflict_i,t-k x Fed_i + logG_it + logP_it + country FE + year FE\n")
cat("SE: Country-clustered (vcovCL)\n")
cat("Reporting: country/year FE coefficients omitted for readability\n")
cat("================================================================================\n\n")

model_names <- names(results)
for (i in seq_along(model_names)) {
  r <- results[[model_names[i]]]

  cat(sprintf("MODEL %d\n", i))
  cat("--------------------------------------------------------------------------------\n")
  cat(sprintf("Lag     : %d year(s)\n", r$lag))
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