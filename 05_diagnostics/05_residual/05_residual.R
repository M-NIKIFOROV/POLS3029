# Compute residual dependence diagnostics for baseline CRE-Mundlak model matrix
# Tests: Pesaran CD (cross-sectional dependence), Breusch-Godfrey serial correlation
# Output: 05_diagnostics/05_residual/05_residual_output/05_residual.txt

setwd("c:/Users/maxim/OneDrive/Documents/University - Year 3/POLS3029/paper/POLS3029")

library(plm)
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

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  sprintf("%.5f", p)
}

decision_5pct <- function(p) {
  if (is.na(p)) return("Unavailable")
  if (p < 0.05) return("Reject H0 at 5%")
  "Fail to reject H0 at 5%"
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

fit_and_test_residuals <- function(data, conflict_key, centralisation_key, cfg = MODEL_CONFIG) {
  conflict_expr <- get_conflict_term_by_key(conflict_key, cfg)
  outcome_expr <- get_outcome_term_by_key(centralisation_key, cfg)
  conflict_src <- cfg$conflict[[conflict_key]]$source_variable
  outcome_src <- cfg$centralisation[[centralisation_key]]$source_variable

  vars_needed <- c(conflict_src, outcome_src, cfg$controls, cfg$federal_indicator, "year_f", "year", "ccode")
  vars_needed <- vars_needed[vars_needed %in% names(data)]

  dat <- data[complete.cases(data[, vars_needed, drop = FALSE]), ]
  if (nrow(dat) == 0) {
    stop(sprintf("No complete cases for model %s x %s", conflict_key, centralisation_key))
  }

  dat <- dat[order(dat$ccode, dat$year), ]

  dat$y_val <- with(dat, eval(parse(text = outcome_expr)))
  dat$conflict_w <- with(dat, eval(parse(text = conflict_expr)))

  dat$conflict_mean <- ave(dat$conflict_w, dat$ccode, FUN = function(x) mean(x, na.rm = TRUE))
  dat$conflict_mean_Fed <- dat$conflict_mean * dat[[cfg$federal_indicator]]

  mean_build <- add_control_means(dat, cfg$controls)
  dat <- mean_build$dat
  control_mean_terms <- mean_build$mean_vars

  rhs <- c(
    "conflict_w",
    sprintf("conflict_w:%s", cfg$federal_indicator),
    cfg$controls,
    "conflict_mean",
    "conflict_mean_Fed",
    control_mean_terms,
    "year_f"
  )

  f <- as.formula(sprintf("y_val ~ %s", paste(rhs, collapse = " + ")))

  pm <- plm(f, data = dat, model = "pooling", index = c("ccode", "year"))

  cd <- tryCatch(
    pcdtest(pm, test = "cd"),
    error = function(e) NULL
  )

  bg <- tryCatch(
    pbgtest(pm),
    error = function(e) NULL
  )

  cd_stat <- if (is.null(cd)) NA_real_ else as.numeric(unname(cd$statistic))
  cd_p <- if (is.null(cd)) NA_real_ else as.numeric(cd$p.value)

  bg_stat <- if (is.null(bg)) NA_real_ else as.numeric(unname(bg$statistic))
  bg_p <- if (is.null(bg)) NA_real_ else as.numeric(bg$p.value)

  list(
    conflict_key = conflict_key,
    conflict_label = cfg$conflict[[conflict_key]]$label,
    centralisation_key = centralisation_key,
    centralisation_label = cfg$centralisation[[centralisation_key]]$label,
    formula = deparse(f),
    n = nrow(dat),
    countries = length(unique(dat$ccode)),
    years = length(unique(dat$year)),
    cd_stat = cd_stat,
    cd_p = cd_p,
    bg_stat = bg_stat,
    bg_p = bg_p
  )
}

# ----------------------------------------------------------------------------
# Run model matrix
# ----------------------------------------------------------------------------

results <- list()
for (i in seq_along(MODEL_CONFIG$model_matrix)) {
  spec <- MODEL_CONFIG$model_matrix[[i]]
  key <- paste(spec$conflict, spec$centralisation, sep = "__")
  results[[key]] <- fit_and_test_residuals(
    data = panel,
    conflict_key = spec$conflict,
    centralisation_key = spec$centralisation,
    cfg = MODEL_CONFIG
  )
}

# ----------------------------------------------------------------------------
# Write output
# ----------------------------------------------------------------------------

dir.create("05_diagnostics/05_residual/05_residual_output", recursive = TRUE, showWarnings = FALSE)
out_file <- "05_diagnostics/05_residual/05_residual_output/05_residual.txt"

sink(out_file)

cat("================================================================================\n")
cat("RESIDUAL DEPENDENCE DIAGNOSTICS (CRE-MUNDLAK 2x2)\n")
cat("Tests: Pesaran CD (cross-sectional dependence), Breusch-Godfrey pbgtest (serial correlation)\n")
cat("================================================================================\n\n")

for (i in seq_along(results)) {
  r <- results[[i]]

  cat(sprintf("MODEL %d\n", i))
  cat("--------------------------------------------------------------------------------\n")
  cat(sprintf("Conflict: %s (%s)\n", r$conflict_key, r$conflict_label))
  cat(sprintf("Outcome : %s (%s)\n", r$centralisation_key, r$centralisation_label))
  cat(sprintf("Formula : %s\n", paste(r$formula, collapse = " ")))
  cat(sprintf("N=%d | Countries=%d | Years=%d\n", r$n, r$countries, r$years))
  cat("\n")

  cat("Pesaran CD (H0: cross-sectional independence)\n")
  cat(sprintf("  Statistic = %s | p = %s | %s\n",
              ifelse(is.na(r$cd_stat), "NA", sprintf("%.4f", r$cd_stat)),
              fmt_p(r$cd_p),
              decision_5pct(r$cd_p)))

  cat("Breusch-Godfrey pbgtest (H0: no serial correlation)\n")
  cat(sprintf("  Statistic = %s | p = %s | %s\n",
              ifelse(is.na(r$bg_stat), "NA", sprintf("%.4f", r$bg_stat)),
              fmt_p(r$bg_p),
              decision_5pct(r$bg_p)))

  cat("\n")
}

cat("Interpretation note: rejection in either test supports robust SE choices such as DK.\n")
cat("\nDIAGNOSTICS COMPLETE.\n")

sink()

cat(sprintf("Results written to: %s\n", out_file))
