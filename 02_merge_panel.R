# 02_merge_panel.R
# Master dataset merge for CRE regressions
# Combines all cleaned datasets into a long-format panel
# Output: 02_output/master_panel.csv

setwd("c:/Users/maxim/OneDrive/Documents/University - Year 3/POLS3029/paper/POLS3029")

# ============================================================================
# Helper: Convert wide format to long
# ============================================================================
wide_to_long <- function(df, id_col = "ccode", country_col = "country") {
  year_cols <- grep("^X?[0-9]{4}$", names(df), value = TRUE)
  years <- as.integer(sub("^X", "", year_cols))
  
  # Extract the data from year columns (should be numeric)
  data_cols <- df[, year_cols, drop = FALSE]
  
  long_list <- list()
  long_list$ccode <- rep(df[[id_col]], length(year_cols))
  long_list$country <- rep(df[[country_col]], length(year_cols))
  long_list$year <- rep(years, each = nrow(df))
  
  # Extract the actual values
  values <- as.numeric(as.matrix(data_cols))
  long_list$value <- values
  
  long <- as.data.frame(long_list, stringsAsFactors = FALSE)
  
  long <- long[!is.na(long$year), ]
  long <- long[order(long$ccode, long$year), ]
  rownames(long) <- NULL
  long
}

# ============================================================================
# Load and convert all datasets to long format
# ============================================================================

cat("Loading and converting datasets to long format...\n")

# DVs: fiscal measures
subexp <- read.csv("clean_data/subexp_clean.csv", stringsAsFactors = FALSE)
subexp_long <- wide_to_long(subexp, id_col = "ccode", country_col = "country")
names(subexp_long)[names(subexp_long) == "value"] <- "SE_GE"

subtaxrev <- read.csv("clean_data/subtaxrev_clean.csv", stringsAsFactors = FALSE)
subtaxrev_long <- wide_to_long(subtaxrev, id_col = "ccode", country_col = "country")
names(subtaxrev_long)[names(subtaxrev_long) == "value"] <- "STR_GTR"

nettran_os <- read.csv("clean_data/nettran_os_clean.csv", stringsAsFactors = FALSE)
nettran_os_long <- wide_to_long(nettran_os, id_col = "ccode", country_col = "country")
names(nettran_os_long)[names(nettran_os_long) == "value"] <- "NT_SE"

nettran_or <- read.csv("clean_data/nettran_or_clean.csv", stringsAsFactors = FALSE)
nettran_or_long <- wide_to_long(nettran_or, id_col = "ccode", country_col = "country")
names(nettran_or_long)[names(nettran_or_long) == "value"] <- "NT_SR"

# IV: RAI and subcomponents
rai <- read.csv("clean_data/rai_clean.csv", stringsAsFactors = FALSE)
rai$year <- as.integer(rai$year)
rai <- rai[, c("ccode", "year", "n_RAI", "n_selfrule", "n_sharedrule")]
names(rai) <- c("ccode", "year", "RAI", "SelfR", "SharedR")

# Covariates
conflict <- read.csv("clean_data/conflict_clean.csv", stringsAsFactors = FALSE)
conflict$year <- as.integer(conflict$year)
names(conflict)[names(conflict) == "hostlev"] <- "HL"
names(conflict)[names(conflict) == "fatality"] <- "F"
names(conflict)[names(conflict) == "revstate"] <- "R"
conflict <- conflict[, c("ccode", "year", "HL", "F", "R")]

democracy <- read.csv("clean_data/democracy_clean.csv", stringsAsFactors = FALSE)
democracy$year <- as.integer(democracy$year)
names(democracy)[names(democracy) == "v2x_libdem"] <- "LD"
democracy <- democracy[, c("ccode", "year", "LD")]

gdppc <- read.csv("clean_data/gdppc_clean.csv", stringsAsFactors = FALSE)
gdppc_long <- wide_to_long(gdppc, id_col = "ccode", country_col = "country")
names(gdppc_long)[names(gdppc_long) == "value"] <- "G"
# Filter out World Bank regional aggregates
gdppc_long <- gdppc_long[!gdppc_long$ccode %in% c("afe", "afw", "eas", "lcn", "mena", "ssa"), ]

population <- read.csv("clean_data/population_clean.csv", stringsAsFactors = FALSE)
population_long <- wide_to_long(population, id_col = "ccode", country_col = "country")
names(population_long)[names(population_long) == "value"] <- "P"

debt_gdp <- read.csv("clean_data/debt-gdp_clean.csv", stringsAsFactors = FALSE)
debt_gdp_long <- wide_to_long(debt_gdp, id_col = "ccode", country_col = "country")
names(debt_gdp_long)[names(debt_gdp_long) == "value"] <- "D"

# ============================================================================
# Create static variables: Bicameral (B) and Federal (Fed)
# ============================================================================

bicameral_countries <- c("aus", "aut", "bel", "bih", "bra", "chl", "col", "can",
                          "cze", "deu", "esp", "fra", "gbr", "idn", "ita", "jpn",
                          "khm", "mex", "mmr", "mng", "nld", "npl", "pry", "rus",
                          "tha", "usa")

federal_countries <- c("aus", "aut", "bel", "can", "deu", "ind", "mex", "nga",
                       "pak", "che", "usa", "arg", "bra")

static_vars <- data.frame(
  ccode = unique(subexp$ccode),
  stringsAsFactors = FALSE
)
static_vars$B <- as.integer(static_vars$ccode %in% bicameral_countries)
static_vars$Fed <- as.integer(static_vars$ccode %in% federal_countries)

# ============================================================================
# Master merge: all datasets on ccode, year
# ============================================================================

cat("Merging all datasets...\n")

# Start with RAI as base (96 countries, most complete)
master <- rai

# Merge in fiscal DVs
master <- merge(master, subexp_long[, c("ccode", "year", "SE_GE")],
                by = c("ccode", "year"), all.x = TRUE)
master <- merge(master, subtaxrev_long[, c("ccode", "year", "STR_GTR")],
                by = c("ccode", "year"), all.x = TRUE)
master <- merge(master, nettran_os_long[, c("ccode", "year", "NT_SE")],
                by = c("ccode", "year"), all.x = TRUE)
master <- merge(master, nettran_or_long[, c("ccode", "year", "NT_SR")],
                by = c("ccode", "year"), all.x = TRUE)

# Merge in covariates
master <- merge(master, conflict[, c("ccode", "year", "HL", "F", "R")],
                by = c("ccode", "year"), all.x = TRUE)
master <- merge(master, democracy[, c("ccode", "year", "LD")],
                by = c("ccode", "year"), all.x = TRUE)
master <- merge(master, gdppc_long[, c("ccode", "year", "G")],
                by = c("ccode", "year"), all.x = TRUE)
master <- merge(master, population_long[, c("ccode", "year", "P")],
                by = c("ccode", "year"), all.x = TRUE)
master <- merge(master, debt_gdp_long[, c("ccode", "year", "D")],
                by = c("ccode", "year"), all.x = TRUE)

# Merge in static variables
master <- merge(master, static_vars, by = "ccode", all.x = TRUE)

# Sort by ccode and year
master <- master[order(master$ccode, master$year), ]
rownames(master) <- NULL

# Create log-transformed covariates
master$logG <- log(master$G)
master$logP <- log(master$P)

# Keep only 1990-2014 window
master <- master[master$year >= 1990 & master$year <= 2014, ]

# Save
write.csv(master, "02_output/master_panel.csv", row.names = FALSE)
cat("master_panel.csv written:", nrow(master), "rows,", 
    length(unique(master$ccode)), "countries\n")
