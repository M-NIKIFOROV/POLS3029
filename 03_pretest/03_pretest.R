# 03_pretest.R
# CRE regressions: debt-gdp and expenditure as DVs, VFI as main IV

library(plm)
library(sandwich)
library(lmtest)

# Load datasets
area        <- read.csv("../clean_data/area_clean.csv",        check.names=FALSE, stringsAsFactors=FALSE)
balance     <- read.csv("../clean_data/balance_clean.csv",     check.names=FALSE, stringsAsFactors=FALSE)
expenditure <- read.csv("../clean_data/expenditure_clean.csv", check.names=FALSE, stringsAsFactors=FALSE)
debt_gdp    <- read.csv("../clean_data/debt-gdp_clean.csv",    check.names=FALSE, stringsAsFactors=FALSE)
gdp         <- read.csv("../clean_data/gdp_clean.csv",         check.names=FALSE, stringsAsFactors=FALSE)
gdppc       <- read.csv("../clean_data/gdppc_clean.csv",       check.names=FALSE, stringsAsFactors=FALSE)
population  <- read.csv("../clean_data/population_clean.csv",  check.names=FALSE, stringsAsFactors=FALSE)
democracy   <- read.csv("../clean_data/democracy_clean.csv",   check.names=FALSE, stringsAsFactors=FALSE)
vfi         <- read.csv("../clean_data/vfi_clean.csv",         check.names=FALSE, stringsAsFactors=FALSE)

# Helper: reshape wide to long
wide_to_long <- function(d, id_cols, value_name) {
  yr_cols <- grep("^[0-9]{4}$", names(d), value=TRUE)
  long <- data.frame(
    country_code = rep(d[[id_cols[1]]], times=length(yr_cols)),
    year = rep(as.integer(yr_cols), each=nrow(d)),
    value = as.numeric(unlist(d[, yr_cols], use.names=FALSE)),
    stringsAsFactors = FALSE
  )
  names(long)[3] <- value_name
  long[!is.na(long[[value_name]]), ]
}

# Reshape wide datasets to long
area_long        <- wide_to_long(area, "country_code", "area")
balance_long     <- wide_to_long(balance, "country_code", "balance")
expenditure_long <- wide_to_long(expenditure, "country_code", "expenditure")
debt_gdp_long    <- wide_to_long(debt_gdp, "country_code", "debt_gdp")
gdp_long         <- wide_to_long(gdp, "country_code", "gdp")
gdppc_long       <- wide_to_long(gdppc, "country_code", "gdppc")
population_long  <- wide_to_long(population, "country_code", "population")

# VFI: select central government sector
vfi_cg <- vfi[tolower(vfi$sector) == "central government", ]
yr_cols_vfi <- grep("^[0-9]{4}$", names(vfi_cg), value=TRUE)
vfi_long <- data.frame(
  country_code = rep(vfi_cg$country_code, times=length(yr_cols_vfi)),
  year = rep(as.integer(yr_cols_vfi), each=nrow(vfi_cg)),
  vfi = as.numeric(unlist(vfi_cg[, yr_cols_vfi], use.names=FALSE)),
  stringsAsFactors = FALSE
)
vfi_long <- vfi_long[!is.na(vfi_long$vfi), ]

# Democracy: select main index v2x_polyarchy
democracy_sub <- democracy[, c("country_code", "year", "v2x_polyarchy")]
names(democracy_sub)[3] <- "democracy"

# Merge all
df <- Reduce(function(x, y) merge(x, y, by=c("country_code", "year"), all=FALSE),
  list(vfi_long, debt_gdp_long, expenditure_long, gdp_long, gdppc_long, 
       population_long, area_long, democracy_sub, balance_long))

# Keep only years 1989-2020 (VFI range)
df <- df[df$year >= 1989 & df$year <= 2020, ]

# Create logged variables
df$log_gdp        <- log(df$gdp)
df$log_gdppc      <- log(df$gdppc)
df$log_population <- log(df$population)
df$log_area       <- log(df$area)

# Federalism dummy: 11 countries
federal_countries <- c("aus", "aut", "bel", "bra", "can", "deu", "mex", "rus", "zaf", "che", "usa")
df$federalism <- as.integer(df$country_code %in% federal_countries)

# Interaction
df$vfi_federalism <- df$vfi * df$federalism

# Sort
df <- df[order(df$country_code, df$year), ]
rownames(df) <- NULL

# Remove NAs
df <- na.omit(df)

# CRE (Mundlak): add country means of all time-varying variables
time_varying <- c("vfi", "log_gdp", "log_gdppc", "log_population", "log_area", "democracy", "balance")
for (v in time_varying) {
  df[[paste0(v, "_mean")]] <- ave(df[[v]], df$country_code, FUN = mean)
}
# Interaction mean
df$vfi_federalism_mean <- ave(df$vfi_federalism, df$country_code, FUN = mean)

# CRE via pooled OLS with country means (Mundlak approach)
# Federalism is time-invariant so it's estimated directly
m1_cre <- lm(debt_gdp ~ vfi + federalism + vfi_federalism +
               log_gdp + log_gdppc + log_population + log_area + democracy + balance +
               vfi_mean + log_gdp_mean + log_gdppc_mean + log_population_mean +
               log_area_mean + democracy_mean + balance_mean + vfi_federalism_mean,
             data = df)

m2_cre <- lm(expenditure ~ vfi + federalism + vfi_federalism +
               log_gdp + log_gdppc + log_population + log_area + democracy + balance +
               vfi_mean + log_gdp_mean + log_gdppc_mean + log_population_mean +
               log_area_mean + democracy_mean + balance_mean + vfi_federalism_mean,
             data = df)

# Cluster-robust SEs at country level
vcov1 <- vcovCL(m1_cre, cluster = ~country_code)
vcov2 <- vcovCL(m2_cre, cluster = ~country_code)
ct1   <- coeftest(m1_cre, vcov = vcov1)
ct2   <- coeftest(m2_cre, vcov = vcov2)

# Build comparison table
var_labels <- c(
  vfi            = "VFI",
  federalism     = "Federalism",
  vfi_federalism = "VFI x Federalism",
  log_gdp        = "log(GDP)",
  log_gdppc      = "log(GDP per capita)",
  log_population = "log(Population)",
  log_area       = "log(Area)",
  democracy      = "Democracy (polyarchy)",
  balance        = "Fiscal Balance"
)

fmt_coef <- function(ct, var) {
  if (!var %in% rownames(ct)) return(c("", ""))
  b <- ct[var, "Estimate"]
  se <- ct[var, "Std. Error"]
  p  <- ct[var, "Pr(>|t|)"]
  stars <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
  c(sprintf("%.4f%s", b, stars), sprintf("(%.4f)", se))
}

out <- capture.output({
  cat(paste0(strrep("=", 80), "\n"))
  cat("CRE (MUNDLAK) REGRESSION RESULTS: Pretesting\n")
  cat(paste0(strrep("=", 80), "\n"))
  cat(sprintf("Sample: %d observations, %d countries, years 1989-2020\n",
              nrow(df), length(unique(df$country_code))))
  cat(paste0(strrep("-", 80), "\n"))
  cat(sprintf("%-25s  %20s  %20s\n", "Variable", "Model 1 (Debt/GDP)", "Model 2 (Expenditure)"))
  cat(paste0(strrep("-", 80), "\n"))

  for (var in names(var_labels)) {
    r1 <- fmt_coef(ct1, var)
    r2 <- fmt_coef(ct2, var)
    cat(sprintf("%-25s  %20s  %20s\n", var_labels[var], r1[1], r2[1]))
    cat(sprintf("%-25s  %20s  %20s\n", "",              r1[2], r2[2]))
  }

  cat(paste0(strrep("-", 80), "\n"))

  r2_1 <- summary(m1_cre)$r.squared
  r2_2 <- summary(m2_cre)$r.squared
  n1   <- length(residuals(m1_cre))
  n2   <- length(residuals(m2_cre))
  grp1 <- length(unique(df$country_code))
  grp2 <- length(unique(df$country_code))

  cat(sprintf("%-25s  %20d  %20d\n", "Observations",   n1,  n2))
  cat(sprintf("%-25s  %20d  %20d\n", "Countries",      grp1, grp2))
  cat(sprintf("%-25s  %20.4f  %20.4f\n", "R-squared (within)", r2_1, r2_2))
  cat(paste0(strrep("=", 80), "\n"))
  cat("* p<0.1  ** p<0.05  *** p<0.01\n")
  cat("Standard errors in parentheses, clustered at country level.\n")
  cat("CRE (Mundlak) estimator: pooled OLS with country means of time-varying regressors.\n")
  cat("Logged variables: GDP, GDP per capita, population, area.\n")
  cat("Federalism = 1 for AUS, AUT, BEL, BRA, CAN, DEU, MEX, RUS, ZAF, CHE, USA.\n")
  cat("Democracy measured by V-Dem electoral democracy index (v2x_polyarchy).\n")
  cat(paste0(strrep("=", 80), "\n"))
})

# Write to file
writeLines(out, "../04_pretest_output/04_pretest_output.txt")
cat("Results written to ../04_pretest_output/04_pretest_output.txt\n")
