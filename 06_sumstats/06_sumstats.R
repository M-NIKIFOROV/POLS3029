# Build summary statistics tables for panel variables
# Output: 06_sumstats/06_sumstats_output/06_sumstats.txt

setwd("c:/Users/maxim/OneDrive/Documents/University - Year 3/POLS3029/paper/POLS3029")

source("model_config.R")

panel_path <- "clean_data/model_panel_clean.csv"
if (!file.exists(panel_path)) {
  stop("Missing clean_data/model_panel_clean.csv. Run 01_cleaning/01_build_panel.R first.")
}

panel <- read.csv(panel_path, stringsAsFactors = FALSE)

vars_main <- c(
  unique(vapply(MODEL_CONFIG$conflict, function(x) x$source_variable, character(1))),
  unique(vapply(MODEL_CONFIG$centralisation, function(x) x$source_variable, character(1))),
  MODEL_CONFIG$controls,
  MODEL_CONFIG$federal_indicator
)
vars_main <- vars_main[vars_main %in% names(panel)]

num_stats <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(c(N = 0, Mean = NA, SD = NA, Min = NA, P25 = NA, Median = NA, P75 = NA, Max = NA))
  }
  c(
    N = length(x),
    Mean = mean(x),
    SD = sd(x),
    Min = min(x),
    P25 = as.numeric(quantile(x, 0.25, type = 7)),
    Median = median(x),
    P75 = as.numeric(quantile(x, 0.75, type = 7)),
    Max = max(x)
  )
}

build_summary_table <- function(dat, vars) {
  rows <- lapply(vars, function(v) {
    st <- num_stats(dat[[v]])
    data.frame(
      Variable = v,
      N = as.integer(st["N"]),
      Mean = as.numeric(st["Mean"]),
      SD = as.numeric(st["SD"]),
      Min = as.numeric(st["Min"]),
      P25 = as.numeric(st["P25"]),
      Median = as.numeric(st["Median"]),
      P75 = as.numeric(st["P75"]),
      Max = as.numeric(st["Max"]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

print_table <- function(df, digits = 3) {
  out <- df
  num_cols <- names(out)[sapply(out, is.numeric)]
  for (nm in num_cols) {
    if (nm == "N") next
    out[[nm]] <- ifelse(is.na(out[[nm]]), "NA", format(round(out[[nm]], digits), nsmall = digits, trim = TRUE))
  }
  print(out, row.names = FALSE, right = TRUE)
}

fed_var <- MODEL_CONFIG$federal_indicator
if (!(fed_var %in% names(panel))) {
  stop(sprintf("Federal indicator '%s' not found in panel.", fed_var))
}

overall_tbl <- build_summary_table(panel, vars_main)

by_fed_tbl <- do.call(
  rbind,
  lapply(sort(unique(panel[[fed_var]])), function(g) {
    sub <- panel[panel[[fed_var]] == g, , drop = FALSE]
    t <- build_summary_table(sub, setdiff(vars_main, fed_var))
    t$FedGroup <- g
    t
  })
)
by_fed_tbl <- by_fed_tbl[, c("FedGroup", "Variable", "N", "Mean", "SD", "Min", "P25", "Median", "P75", "Max")]

fed_only <- panel[panel[[fed_var]] == 1, , drop = FALSE]

country_col <- "ccode"
if (!(country_col %in% names(fed_only))) {
  stop("Expected country identifier 'ccode' not found.")
}

country_vars <- setdiff(vars_main, fed_var)
country_vars <- country_vars[sapply(country_vars, function(v) is.numeric(fed_only[[v]]))]

fed_country_tbl <- do.call(
  rbind,
  lapply(split(fed_only, fed_only[[country_col]]), function(df) {
    data.frame(
      ccode = df[[country_col]][1],
      Years = length(unique(df$year)),
      Obs = nrow(df),
      F_mean = mean(df$F, na.rm = TRUE),
      HL_mean = mean(df$HL, na.rm = TRUE),
      SELF_mean = mean(df$SELF, na.rm = TRUE),
      vfi_mean = mean(df$vfi, na.rm = TRUE),
      dem_mean = mean(df$dem, na.rm = TRUE),
      logG_mean = mean(df$logG, na.rm = TRUE),
      logP_mean = mean(df$logP, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
)

fed_country_tbl <- fed_country_tbl[order(fed_country_tbl$ccode), ]

out_dir <- "06_sumstats/06_sumstats_output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, "06_sumstats.txt")

sink(out_file)

cat("================================================================================\n")
cat("SUMMARY STATISTICS\n")
cat("Data source: clean_data/model_panel_clean.csv\n")
cat(sprintf("Panel size: %d rows | %d countries | %d years\n", nrow(panel), length(unique(panel$ccode)), length(unique(panel$year))))
cat(sprintf("Federal indicator used: %s\n", fed_var))
cat("================================================================================\n\n")

cat("TABLE A. OVERALL SUMMARY STATISTICS (ALL COUNTRY-YEARS)\n")
cat("--------------------------------------------------------------------------------\n")
print_table(overall_tbl)
cat("\n\n")

cat("TABLE B. SUMMARY STATISTICS BY FEDERAL STATUS (Fed = 0 / 1)\n")
cat("--------------------------------------------------------------------------------\n")
print_table(by_fed_tbl)
cat("\n\n")

cat("TABLE C. FEDERAL COUNTRIES ONLY: COUNTRY-LEVEL MEANS\n")
cat("--------------------------------------------------------------------------------\n")
print_table(fed_country_tbl)
cat("\n")

cat("SUMMARY TABLES COMPLETE.\n")

sink()

cat(sprintf("Results written to: %s\n", out_file))
