# 02_regression/01_build_panel.R
# Build model panel directly from clean_data inputs
# Output: clean_data/model_panel_clean.csv

setwd("c:/Users/maxim/OneDrive/Documents/University - Year 3/POLS3029/paper/POLS3029")

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

wide_to_long <- function(df, id_col = "ccode", country_col = "country") {
  year_cols <- grep("^X?[0-9]{4}$", names(df), value = TRUE)
  years <- as.integer(sub("^X", "", year_cols))

  long <- data.frame(
    ccode = rep(tolower(df[[id_col]]), length(year_cols)),
    country = rep(tolower(df[[country_col]]), length(year_cols)),
    year = rep(years, each = nrow(df)),
    value = as.numeric(as.matrix(df[, year_cols, drop = FALSE])),
    stringsAsFactors = FALSE
  )

  long <- long[!is.na(long$year), ]
  long <- long[order(long$ccode, long$year), ]
  rownames(long) <- NULL
  long
}

# ----------------------------------------------------------------------------
# Load and shape inputs
# ----------------------------------------------------------------------------

conflict <- read.csv("clean_data/conflict_clean.csv", stringsAsFactors = FALSE)
conflict$ccode <- tolower(conflict$ccode)
conflict$year <- as.integer(conflict$year)
names(conflict)[names(conflict) == "hostlev"] <- "HL"
names(conflict)[names(conflict) == "fatality"] <- "F"
conflict <- conflict[, c("ccode", "year", "HL", "F")]

rai <- read.csv("clean_data/rai_clean.csv", stringsAsFactors = FALSE)
rai$ccode <- tolower(rai$ccode)
rai$year <- as.integer(rai$year)
rai <- rai[, c("ccode", "year", "n_selfrule")]
names(rai)[names(rai) == "n_selfrule"] <- "SELF"

gdppc <- read.csv("clean_data/gdppc_clean.csv", stringsAsFactors = FALSE, check.names = FALSE)
gdppc_long <- wide_to_long(gdppc, id_col = "ccode", country_col = "country")
# Drop known World Bank region aggregates
gdppc_long <- gdppc_long[!gdppc_long$ccode %in% c("afe", "afw", "eas", "lcn", "mena", "ssa"), ]
G <- gdppc_long[, c("ccode", "year", "value")]
names(G)[names(G) == "value"] <- "G"

population <- read.csv("clean_data/population_clean.csv", stringsAsFactors = FALSE, check.names = FALSE)
population_long <- wide_to_long(population, id_col = "ccode", country_col = "country")
P <- population_long[, c("ccode", "year", "value")]
names(P)[names(P) == "value"] <- "P"

vfi <- read.csv("clean_data/vfi_clean.csv", stringsAsFactors = FALSE)
vfi$ccode <- tolower(vfi$ccode)
vfi$year <- as.integer(vfi$year)
vfi <- vfi[, c("ccode", "year", "vfi")]

# Static federalism indicator used in models
federal_countries <- c("aus", "aut", "bel", "can", "deu", "ind", "mex", "nga",
                       "pak", "che", "usa", "arg", "bra")

# ----------------------------------------------------------------------------
# Build panel
# ----------------------------------------------------------------------------

# Start from conflict universe, then merge outcomes and controls.
panel <- conflict

panel <- merge(panel, rai, by = c("ccode", "year"), all.x = TRUE)
panel <- merge(panel, G, by = c("ccode", "year"), all.x = TRUE)
panel <- merge(panel, P, by = c("ccode", "year"), all.x = TRUE)
panel <- merge(panel, vfi, by = c("ccode", "year"), all.x = TRUE)

# Keep analysis period consistent with prior scripts
panel <- panel[panel$year >= 1990 & panel$year <= 2014, ]

# Derived controls used by baseline specification
panel$logG <- ifelse(panel$G > 0, log(panel$G), NA_real_)
panel$logP <- ifelse(panel$P > 0, log(panel$P), NA_real_)

# Time-invariant federalism indicator
panel$Fed <- as.integer(panel$ccode %in% federal_countries)

panel <- panel[order(panel$ccode, panel$year), ]
rownames(panel) <- NULL

write.csv(panel, "clean_data/model_panel_clean.csv", row.names = FALSE)

cat("model_panel_clean.csv written:", nrow(panel), "rows,",
    length(unique(panel$ccode)), "countries\n")
