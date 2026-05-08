# 08_clean_vfi.R
# Clean IMF vertical fiscal imbalance (VFI) data
# Input:  raw_data/vfi(FD).csv
# Output: clean_data/vfi_clean.csv (wide: country_name, country_code, sector, year cols)

raw <- read.csv(
  "raw_data/vfi(FD).csv",
  stringsAsFactors = FALSE,
  check.names       = FALSE
)

raw <- raw[raw[["OBS_MEASURE"]] == "OBS_VALUE", ]

year_cols <- grep("^[0-9]{4}$", names(raw), value = TRUE)

wide <- data.frame(
  country = tolower(raw[["COUNTRY"]]),
  ccode = tolower(sub("\\..*$", "", raw[["SERIES_CODE"]])),

  sector       = tolower(raw[["SECTOR"]]),
  raw[, year_cols],
  stringsAsFactors = FALSE,
  check.names = FALSE
)

names(wide)[-(1:3)] <- as.integer(year_cols)

wide <- wide[order(wide$country, wide$sector), ]
rownames(wide) <- NULL

write.csv(wide, "clean_data/vfi_clean.csv", row.names = FALSE)
cat("vfi_clean.csv written:", nrow(wide), "rows\n")
