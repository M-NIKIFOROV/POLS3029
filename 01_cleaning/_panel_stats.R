setwd("c:/Users/maxim/OneDrive/Documents/University - Year 3/POLS3029/paper/POLS3029")

yr_min <- 1990
yr_max <- 2014

rai <- read.csv("clean_data/rai_clean.csv", stringsAsFactors = FALSE)
rai$year <- as.integer(rai$year)
rai_countries <- unique(rai[rai$year >= yr_min & rai$year <= yr_max, "ccode"])
cat("RAI countries in 1990-2014 window:", length(rai_countries), "\n")

panel_stats <- function(file, dv_name) {
  d <- read.csv(file, stringsAsFactors = FALSE)

  # Identify ccode column — prefer column named "ccode", else first char col
  if ("ccode" %in% names(d)) {
    ccode_col <- "ccode"
  } else {
    char_cols <- names(d)[sapply(d, is.character)]
    ccode_col <- char_cols[1]
  }
  ccodes <- d[[ccode_col]]

  # Year columns within window (handles both "1990" and "X1990" prefixes)
  col_years <- suppressWarnings(as.integer(sub("^X", "", names(d))))
  yr_cols <- names(d)[!is.na(col_years) & col_years >= yr_min & col_years <= yr_max]

  # Restrict to RAI countries
  in_rai <- ccodes %in% rai_countries
  d_rai <- d[in_rai, ]
  ccodes_rai <- ccodes[in_rai]

  obs_matrix <- d_rai[, yr_cols, drop = FALSE]

  # Countries with at least 1 non-NA year
  has_data <- apply(obs_matrix, 1, function(r) any(!is.na(r)))
  n_countries <- sum(has_data)

  if (n_countries == 0) { cat("  No countries matched.\n"); return(invisible(NULL)) }
  obs_matrix_valid <- obs_matrix[has_data, ]
  n_obs <- sum(!is.na(obs_matrix_valid))

  yr_with_data <- sum(apply(obs_matrix_valid, 2, function(c) any(!is.na(c))))
  obs_per_country <- apply(obs_matrix_valid, 1, function(r) sum(!is.na(r)))
  avg_yrs <- round(mean(obs_per_country), 1)

  cat(sprintf("\n=== %s ===\n", dv_name))
  cat(sprintf("  Countries (in RAI, >=1 obs in 1990-2014): %d\n", n_countries))
  cat(sprintf("  Year span covered: 1990-2014 (%d cols)\n", length(yr_cols)))
  cat(sprintf("  Years with at least 1 country: %d\n", yr_with_data))
  cat(sprintf("  Total N (non-NA obs): %d\n", n_obs))
  cat(sprintf("  Obs per country: avg=%.1f, min=%d, max=%d\n",
              avg_yrs, min(obs_per_country), max(obs_per_country)))

  sparse <- sort(ccodes_rai[has_data][obs_per_country < 5])
  if (length(sparse) > 0)
    cat(sprintf("  Sparse (<5 yrs): %s\n", paste(sparse, collapse = ", ")))

  invisible(NULL)
}

# RAI is long-format — report separately for each of its three measures
rai_window <- rai[rai$year >= yr_min & rai$year <= yr_max, ]

rai_stats <- function(var) {
  sub <- rai_window[!is.na(rai_window[[var]]), ]
  n_countries <- length(unique(sub$ccode))
  n_obs       <- nrow(sub)
  yr_with_data <- length(unique(sub$year))
  obs_per_c   <- tapply(sub[[var]], sub$ccode, length)
  cat(sprintf("\n=== %s (RAI) ===\n", var))
  cat(sprintf("  Countries: %d\n", n_countries))
  cat(sprintf("  Year span covered: 1990-2014 (25 cols)\n"))
  cat(sprintf("  Years with at least 1 country: %d\n", yr_with_data))
  cat(sprintf("  Total N (non-NA obs): %d\n", n_obs))
  cat(sprintf("  Obs per country: avg=%.1f, min=%d, max=%d\n",
              mean(obs_per_c), min(obs_per_c), max(obs_per_c)))
}

rai_stats("n_RAI")
rai_stats("n_selfrule")
rai_stats("n_sharedrule")

panel_stats("clean_data/subexp_clean.csv",     "Subnational expenditure share (subexp)")
panel_stats("clean_data/subtaxrev_clean.csv",  "Subnational tax revenue share (subtaxrev)")
panel_stats("clean_data/nettran_os_clean.csv", "Net transfers own-source (nettran_os)")
panel_stats("clean_data/nettran_or_clean.csv", "Net transfers own-revenue (nettran_or)")

cat("\nDone.\n")
