# 07_clean_debt_gdp.R
# Clean IMF debt-to-GDP data
# Input:  raw_data/debt-gdp(IMF).csv
# Output: clean_data/debt-gdp_clean.csv (wide: country_name, country_code, year cols)

raw <- read.csv(
  "raw_data/debt-gdp(IMF).csv",
  stringsAsFactors = FALSE,
  check.names       = FALSE
)

raw <- raw[raw[["OBS_MEASURE"]] == "OBS_VALUE" &
           grepl("^Debt instruments", raw[["INDICATOR"]], ignore.case = TRUE), ]

year_cols <- grep("^[0-9]{4}$", names(raw), value = TRUE)

country_name <- tolower(raw[["COUNTRY"]])
country_code <- tolower(sub("\\..*$", "", raw[["SERIES_CODE"]]))

wide <- data.frame(
  country_name = country_name,
  country_code = country_code,
  raw[, year_cols],
  stringsAsFactors = FALSE,
  check.names = FALSE
)

names(wide)[-(1:2)] <- as.integer(year_cols)

wide <- wide[order(wide$country_name), ]
rownames(wide) <- NULL

write.csv(wide, "clean_data/debt-gdp_clean.csv", row.names = FALSE)
cat("debt-gdp_clean.csv written:", nrow(wide), "rows\n")

# --- GDP panel ---
raw_gdp <- read.csv(
  "raw_data/debt-gdp(IMF).csv",
  stringsAsFactors = FALSE,
  check.names       = FALSE
)

raw_gdp <- raw_gdp[raw_gdp[["OBS_MEASURE"]] == "OBS_VALUE" &
                   grepl("^Gross domestic product", raw_gdp[["INDICATOR"]], ignore.case = TRUE), ]

year_cols_gdp <- grep("^[0-9]{4}$", names(raw_gdp), value = TRUE)

wide_gdp <- data.frame(
  country_name = tolower(raw_gdp[["COUNTRY"]]),
  country_code = tolower(sub("\\..*$", "", raw_gdp[["SERIES_CODE"]])),
  raw_gdp[, year_cols_gdp],
  stringsAsFactors = FALSE,
  check.names = FALSE
)

names(wide_gdp)[-(1:2)] <- as.integer(year_cols_gdp)

wide_gdp <- wide_gdp[order(wide_gdp$country_name), ]
rownames(wide_gdp) <- NULL

write.csv(wide_gdp, "clean_data/gdp_clean.csv", row.names = FALSE)
cat("gdp_clean.csv written:", nrow(wide_gdp), "rows\n")
