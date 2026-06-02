# Compute GVIF diagnostics for baseline CRE-Mundlak model matrix
# Output: 05_diagnostics/05_gvif/05_gvif_output/05_gvif.txt

setwd("c:/Users/maxim/OneDrive/Documents/University - Year 3/POLS3029/paper/POLS3029")

library(car)

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

add_control_means <- function(dat, controls) {
  mean_vars <- character(0)
  for (ctrl in controls) {
    mean_col <- paste0(ctrl, "_mean")
    dat[[mean_col]] <- ave(dat[[ctrl]], dat$ccode, FUN = function(x) mean(x, na.rm = TRUE))
    mean_vars <- c(mean_vars, mean_col)
  }
  list(dat = dat, mean_vars = mean_vars)
}

fit_one_model_for_gvif <- function(data, conflict_key, centralisation_key, cfg = MODEL_CONFIG) {
  conflict_expr <- get_conflict_term_by_key(conflict_key, cfg)
  outcome_expr <- get_outcome_term_by_key(centralisation_key, cfg)
  conflict_src <- cfg$conflict[[conflict_key]]$source_variable
  outcome_src <- cfg$centralisation[[centralisation_key]]$source_variable

  vars_needed <- c(conflict_src, outcome_src, cfg$controls, cfg$federal_indicator, "year_f", "ccode")
  vars_needed <- vars_needed[vars_needed %in% names(data)]

  dat <- data[complete.cases(data[, vars_needed, drop = FALSE]), ]
  if (nrow(dat) == 0) {
    stop(sprintf("No complete cases for model %s x %s", conflict_key, centralisation_key))
  }

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
  m <- lm(f, data = dat)

  v <- suppressWarnings(vif(m))

  if (is.vector(v)) {
    vif_tab <- data.frame(
      term = names(v),
      gvif = as.numeric(v),
      df = 1,
      gvif_adj = as.numeric(v),
      stringsAsFactors = FALSE
    )
  } else {
    vif_tab <- data.frame(
      term = rownames(v),
      gvif = as.numeric(v[, "GVIF"]),
      df = as.numeric(v[, "Df"]),
      gvif_adj = as.numeric(v[, "GVIF^(1/(2*Df))"]),
      stringsAsFactors = FALSE
    )
  }
  rownames(vif_tab) <- NULL

  list(
    conflict_key = conflict_key,
    conflict_label = cfg$conflict[[conflict_key]]$label,
    centralisation_key = centralisation_key,
    centralisation_label = cfg$centralisation[[centralisation_key]]$label,
    formula = deparse(f),
    n = nrow(dat),
    countries = length(unique(dat$ccode)),
    vif_tab = vif_tab
  )
}

# ----------------------------------------------------------------------------
# Run model matrix
# ----------------------------------------------------------------------------

results <- list()
for (i in seq_along(MODEL_CONFIG$model_matrix)) {
  spec <- MODEL_CONFIG$model_matrix[[i]]
  key <- paste(spec$conflict, spec$centralisation, sep = "__")
  results[[key]] <- fit_one_model_for_gvif(
    data = panel,
    conflict_key = spec$conflict,
    centralisation_key = spec$centralisation,
    cfg = MODEL_CONFIG
  )
}

# ----------------------------------------------------------------------------
# Write output
# ----------------------------------------------------------------------------

dir.create("05_diagnostics/05_gvif/05_gvif_output", recursive = TRUE, showWarnings = FALSE)
out_file <- "05_diagnostics/05_gvif/05_gvif_output/05_gvif.txt"

sink(out_file)

cat("================================================================================\n")
cat("GVIF DIAGNOSTIC RESULTS (CRE-MUNDLAK 2x2)\n")
cat("Metric reported: GVIF, Df, and adjusted GVIF = GVIF^(1/(2*Df))\n")
cat("Interpretation focus should be on adjusted GVIF values\n")
cat("================================================================================\n\n")

for (i in seq_along(results)) {
  r <- results[[i]]

  cat(sprintf("MODEL %d\n", i))
  cat("--------------------------------------------------------------------------------\n")
  cat(sprintf("Conflict: %s (%s)\n", r$conflict_key, r$conflict_label))
  cat(sprintf("Outcome : %s (%s)\n", r$centralisation_key, r$centralisation_label))
  cat(sprintf("Formula : %s\n", paste(r$formula, collapse = " ")))
  cat(sprintf("N=%d | Countries=%d\n", r$n, r$countries))
  cat("\n")

  cat(sprintf("%-35s %14s %10s %14s\n", "Term", "GVIF", "Df", "GVIF_adj"))
  cat(paste(rep("-", 80), collapse = ""), "\n", sep = "")

  for (j in seq_len(nrow(r$vif_tab))) {
    rr <- r$vif_tab[j, ]
    cat(sprintf("%-35s %14.4f %10.0f %14.4f\n", rr$term, rr$gvif, rr$df, rr$gvif_adj))
  }

  key_terms <- r$vif_tab[r$vif_tab$term %in% c("conflict_w", "conflict_w:Fed"), , drop = FALSE]
  if (nrow(key_terms) > 0) {
    cat("\nKey terms (adjusted GVIF):\n")
    for (k in seq_len(nrow(key_terms))) {
      cat(sprintf("  %s = %.4f\n", key_terms$term[k], key_terms$gvif_adj[k]))
    }
  }

  cat("\n")
}

cat("GUIDE: adjusted GVIF around <5 often acceptable; >10 often concerning.\n")
cat("\nDIAGNOSTICS COMPLETE.\n")

sink()

cat(sprintf("Results written to: %s\n", out_file))
