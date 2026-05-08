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

country <- tolower(raw[["COUNTRY"]])
ccode <- tolower(sub("\\..*$", "", raw[["SERIES_CODE"]]))

wide <- data.frame(
  country = country,
  ccode = ccode,
  raw[, year_cols],
  stringsAsFactors = FALSE,
  check.names = FALSE
)

names(wide)[-(1:2)] <- as.integer(year_cols)

wide <- wide[order(wide$country), ]
rownames(wide) <- NULL

write.csv(wide, "clean_data/debt-gdp_clean.csv", row.names = FALSE)
cat("debt-gdp_clean.csv written:", nrow(wide), "rows\n")
