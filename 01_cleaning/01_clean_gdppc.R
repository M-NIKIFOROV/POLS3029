# 01_clean_gdppc.R
# Clean World Bank GDP per capita (constant 2015 US$) data
# Input:  raw_data/gdppc(worldbank).csv
# Output: clean_data/gdppc_clean.csv (wide: country, ccode, year cols)

raw <- read.csv(
  "raw_data/gdppc(worldbank).csv",
  skip            = 3,
  check.names     = FALSE,
  stringsAsFactors = FALSE
)

year_cols <- grep("^[0-9]{4}$", names(raw), value = TRUE)

raw <- raw[raw[["Indicator Code"]] == "NY.GDP.PCAP.KD", ]

wide <- raw[, c("Country Name", "Country Code", year_cols)]
names(wide)[1:2] <- c("country", "ccode")

wide$country <- tolower(wide$country)
wide$ccode   <- tolower(wide$ccode)

for (col in year_cols) {
  wide[[col]] <- as.numeric(wide[[col]])
}

wide <- wide[order(wide$ccode), ]
rownames(wide) <- NULL

write.csv(wide, "clean_data/gdppc_clean.csv", row.names = FALSE)
cat("gdppc_clean.csv written:", nrow(wide), "rows, years", year_cols[1], "-", tail(year_cols, 1), "\n")
