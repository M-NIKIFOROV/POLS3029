# 03_robustness/04_hausman_mundlak/04_hausman_mundlak.R
# Hausman-Mundlak diagnostics for baseline CRE-Mundlak model matrix
# Output: 03_robustness/04_hausman_mundlak/04_hausman_mundlak_output/04_hausman_mundlak.txt

setwd("c:/Users/maxim/OneDrive/Documents/University - Year 3/POLS3029/paper/POLS3029")

library(sandwich)
library(lmtest)
library(car)

source("model_config.R")

panel_path <- "clean_data/model_panel_clean.csv"
if (!file.exists(panel_path)) {
  stop("Missing clean_data/model_panel_clean.csv. Run 02_regression/01_build_panel.R first.")
}

panel <- read.csv(panel_path, stringsAsFactors = FALSE)
panel$year_f <- as.factor(panel$year)

add_control_means <- function(dat, controls) {
  mean_vars <- character(0)
  for (ctrl in controls) {
    mean_col <- paste0(ctrl, "_mean")
    dat[[mean_col]] <- ave(dat[[ctrl]], dat$ccode, FUN = function(x) mean(x, na.rm = TRUE))
    mean_vars <- c(mean_vars, mean_col)
  }
  list(dat = dat, mean_vars = mean_vars)
}

fit_and_test_mundlak <- function(data, conflict_key, centralisation_key, cfg = MODEL_CONFIG) {
  conflict_expr <- get_conflict_term_by_key(conflict_key, cfg)
  outcome_expr <- get_outcome_term_by_key(centralisation_key, cfg)
  conflict_src <- cfg$conflict[[conflict_key]]$source_variable
  outcome_src <- cfg$centralisation[[centralisation_key]]$source_variable
  fed <- cfg$federal_indicator

  vars_needed <- c(conflict_src, outcome_src, cfg$controls, fed, "year_f", "ccode")
  vars_needed <- vars_needed[vars_needed %in% names(data)]

  dat <- data[complete.cases(data[, vars_needed, drop = FALSE]), ]
  if (nrow(dat) == 0) {
    stop(sprintf("No complete cases for model %s x %s", conflict_key, centralisation_key))
  }

  dat$y_val <- with(dat, eval(parse(text = outcome_expr)))
  dat$conflict_w <- with(dat, eval(parse(text = conflict_expr)))

  dat$conflict_mean <- ave(dat$conflict_w, dat$ccode, FUN = function(x) mean(x, na.rm = TRUE))
  dat$conflict_mean_Fed <- dat$conflict_mean * dat[[fed]]

  mean_build <- add_control_means(dat, cfg$controls)
  dat <- mean_build$dat
  control_mean_terms <- mean_build$mean_vars

  rhs <- c(
    "conflict_w",
    sprintf("conflict_w:%s", fed),
    cfg$controls,
    "conflict_mean",
    "conflict_mean_Fed",
    control_mean_terms,
    "year_f"
  )

  f <- as.formula(sprintf("y_val ~ %s", paste(rhs, collapse = " + ")))

  m <- lm(f, data = dat)
  V <- vcovCL(m, cluster = dat$ccode)

  mundlak_terms <- c("conflict_mean", "conflict_mean_Fed", control_mean_terms)
  test_restrictions <- paste0(mundlak_terms, " = 0")

  test <- linearHypothesis(
    m,
    test_restrictions,
    vcov. = V,
    test = "Chisq"
  )

  list(
    conflict_key = conflict_key,
    conflict_label = cfg$conflict[[conflict_key]]$label,
    centralisation_key = centralisation_key,
    centralisation_label = cfg$centralisation[[centralisation_key]]$label,
    n = nrow(dat),
    countries = length(unique(dat$ccode)),
    formula = deparse(f),
    test = test
  )
}

results <- list()
for (i in seq_along(MODEL_CONFIG$model_matrix)) {
  spec <- MODEL_CONFIG$model_matrix[[i]]
  key <- paste(spec$conflict, spec$centralisation, sep = "__")
  results[[key]] <- fit_and_test_mundlak(panel, spec$conflict, spec$centralisation, MODEL_CONFIG)
}

dir.create("03_robustness/04_hausman_mundlak/04_hausman_mundlak_output", recursive = TRUE, showWarnings = FALSE)
out_file <- "03_robustness/04_hausman_mundlak/04_hausman_mundlak_output/04_hausman_mundlak.txt"

sink(out_file)

cat("================================================================================\n")
cat("HAUSMAN-MUNDLAK DIAGNOSTIC RESULTS\n")
cat("Test: Joint Wald test of Mundlak means = 0\n")
cat("H0: conflict_mean = conflict_mean_Fed = all control means = 0\n")
cat("SE: Country-clustered (vcovCL)\n")
cat("================================================================================\n\n")

for (i in seq_along(results)) {
  r <- results[[i]]
  ttab <- r$test
  chi <- as.numeric(ttab[2, "Chisq"])
  df <- as.numeric(ttab[2, "Df"])
  p <- as.numeric(ttab[2, "Pr(>Chisq)"])

  cat(sprintf("MODEL %d\n", i))
  cat("--------------------------------------------------------------------------------\n")
  cat(sprintf("Conflict: %s (%s)\n", r$conflict_key, r$conflict_label))
  cat(sprintf("Outcome : %s (%s)\n", r$centralisation_key, r$centralisation_label))
  cat(sprintf("Formula : %s\n", paste(r$formula, collapse = " ")))
  cat(sprintf("N=%d | Countries=%d\n", r$n, r$countries))
  cat(sprintf("Wald ChiSq=%.4f | df=%.0f | p=%.5f\n", chi, df, p))
  cat("Decision at 5%: ")
  if (is.na(p)) {
    cat("Unavailable\n\n")
  } else if (p < 0.05) {
    cat("Reject H0 (correlated effects likely present)\n\n")
  } else {
    cat("Fail to reject H0 (RE assumption more plausible)\n\n")
  }
}

cat("DIAGNOSTICS COMPLETE.\n")

sink()

cat(sprintf("Results written to: %s\n", out_file))
