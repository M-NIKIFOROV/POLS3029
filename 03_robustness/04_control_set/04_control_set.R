# Run control-set robustness models from model_config.R
# Output: 03_robustness/04_control_set/04_control_set_output/04_control_set.txt

setwd("c:/Users/maxim/OneDrive/Documents/University - Year 3/POLS3029/paper/POLS3029")

library(sandwich)
library(lmtest)

source("model_config.R")

# ----------------------------------------------------------------------------
# Load panel data
# ----------------------------------------------------------------------------

panel_path <- "clean_data/model_panel_clean.csv"
if (!file.exists(panel_path)) {
  stop("Missing clean_data/model_panel_clean.csv. Run 01_cleaning/01_build_panel.R first.")
}

panel <- read.csv(panel_path, stringsAsFactors = FALSE)
panel$year_f <- as.factor(panel$year)

# Control-set design: with/without democracy, with/without GDPpc (logG).
control_sets <- list(
  full_logG_dem = c("logG", "logP", "dem"),
  no_dem = c("logG", "logP"),
  no_logG = c("logP", "dem"),
  no_logG_no_dem = c("logP")
)

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

add_control_means <- function(dat, controls) {
  mean_vars <- character(0)
  for (ctrl in controls) {
    mean_col <- paste0(ctrl, "_mean")
    dat[[mean_col]] <- ave(dat[[ctrl]], dat$ccode, FUN = function(x) mean(x, na.rm = TRUE))
    mean_vars <- c(mean_vars, mean_col)
  }
  list(dat = dat, mean_vars = mean_vars)
}

fit_one_model <- function(data, conflict_key, centralisation_key, controls, cfg = MODEL_CONFIG) {
  conflict_expr <- get_conflict_term_by_key(conflict_key, cfg)
  outcome_expr <- get_outcome_term_by_key(centralisation_key, cfg)
  conflict_src <- cfg$conflict[[conflict_key]]$source_variable
  outcome_src <- cfg$centralisation[[centralisation_key]]$source_variable

  vars_needed <- c(conflict_src, outcome_src, controls, cfg$federal_indicator, "year_f", "ccode")
  vars_needed <- vars_needed[vars_needed %in% names(data)]

  dat <- data[complete.cases(data[, vars_needed, drop = FALSE]), ]
  if (nrow(dat) == 0) {
    stop(sprintf("No complete cases for model %s x %s", conflict_key, centralisation_key))
  }

  dat$y_val <- with(dat, eval(parse(text = outcome_expr)))
  dat$conflict_w <- with(dat, eval(parse(text = conflict_expr)))

  dat$conflict_mean <- ave(dat$conflict_w, dat$ccode, FUN = function(x) mean(x, na.rm = TRUE))
  dat$conflict_mean_Fed <- dat$conflict_mean * dat[[cfg$federal_indicator]]

  mean_build <- add_control_means(dat, controls)
  dat <- mean_build$dat
  control_mean_terms <- mean_build$mean_vars

  rhs <- c(
    "conflict_w",
    sprintf("conflict_w:%s", cfg$federal_indicator),
    controls,
    "conflict_mean",
    "conflict_mean_Fed",
    control_mean_terms,
    "year_f"
  )

  f <- as.formula(sprintf("y_val ~ %s", paste(rhs, collapse = " + ")))

  m <- lm(f, data = dat)
  vc <- vcovCL(m, cluster = dat$ccode)
  ct <- coeftest(m, vcov. = vc)

  mm_terms <- colnames(model.matrix(m))
  omitted_terms <- setdiff(mm_terms, rownames(ct))

  keep <- !grepl("^year_f", rownames(ct))
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
    controls = controls,
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
# Run model matrix across control sets
# ----------------------------------------------------------------------------

results <- list()
for (set_name in names(control_sets)) {
  controls_here <- control_sets[[set_name]]
  for (i in seq_along(MODEL_CONFIG$model_matrix)) {
    spec <- MODEL_CONFIG$model_matrix[[i]]
    key <- paste(set_name, spec$conflict, spec$centralisation, sep = "__")
    results[[key]] <- fit_one_model(
      data = panel,
      conflict_key = spec$conflict,
      centralisation_key = spec$centralisation,
      controls = controls_here,
      cfg = MODEL_CONFIG
    )
  }
}

# ----------------------------------------------------------------------------
# Write output
# ----------------------------------------------------------------------------

dir.create("03_robustness/04_control_set/04_control_set_output", recursive = TRUE, showWarnings = FALSE)
out_file <- "03_robustness/04_control_set/04_control_set_output/04_control_set.txt"

sink(out_file)

cat("================================================================================\n")
cat("CONTROL-SET ROBUSTNESS RESULTS (CRE-MUNDLAK 2x2)\n")
cat("Control sets: full_logG_dem, no_dem, no_logG, no_logG_no_dem\n")
cat("Specification: Y_it = Conflict_it + Conflict_it x Fed_i + controls_it + country means (Mundlak) + year FE\n")
cat("SE: Country-clustered (vcovCL)\n")
cat("Reporting: year FE coefficients omitted for readability\n")
cat("================================================================================\n\n")

model_names <- names(results)
for (i in seq_along(model_names)) {
  r <- results[[model_names[i]]]

  model_id <- strsplit(model_names[i], "__", fixed = TRUE)[[1]]
  set_name <- model_id[1]

  cat(sprintf("MODEL %d\n", i))
  cat("--------------------------------------------------------------------------------\n")
  cat(sprintf("Control set: %s | controls = %s\n", set_name, paste(r$controls, collapse = ", ")))
  cat(sprintf("Conflict   : %s (%s)\n", r$conflict_key, r$conflict_label))
  cat(sprintf("Outcome    : %s (%s)\n", r$centralisation_key, r$centralisation_label))
  cat(sprintf("Formula    : %s\n", paste(r$formula, collapse = " ")))
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

  dropped_non_fe <- r$omitted_terms[!grepl("^year_f", r$omitted_terms)]
  if (length(dropped_non_fe) > 0) {
    cat("\nDropped (not estimable in-sample): ", paste(dropped_non_fe, collapse = ", "), "\n", sep = "")
  }

  cat("\n")
}

cat("Significance: *** p<0.001, ** p<0.01, * p<0.05, . p<0.10\n")
cat("\nESTIMATION COMPLETE.\n")

sink()

cat(sprintf("Results written to: %s\n", out_file))
