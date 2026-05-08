# 05_clean_democracy.R
# Clean V-Dem democracy data
# Input:  raw_data/democracy(V-DEM).csv  (long: country x year, many variables)
# Output: clean_data/democracy_clean.csv (long panel: country_name, country_code,
#                                         year, five high-level V-Dem indices)

raw <- read.csv(
  "raw_data/democracy(V-DEM).csv",
  stringsAsFactors = FALSE,
  check.names       = FALSE
)

# Keep identifying columns + five high-level composite indices
keep_cols <- c(
  "country_name",
  "country_text_id",
  "year",
  "v2x_polyarchy",   # Electoral Democracy Index
  "v2x_libdem",      # Liberal Democracy Index
  "v2x_partipdem",   # Participatory Democracy Index
  "v2x_delibdem",    # Deliberative Democracy Index
  "v2x_egaldem"      # Egalitarian Democracy Index
)

out <- raw[, keep_cols]

# Rename columns and lowercase string columns
names(out)[names(out) == "country_text_id"] <- "ccode"
names(out)[names(out) == "country_name"]    <- "country"
out$country <- tolower(out$country)
out$ccode   <- tolower(out$ccode)

# Ensure year is integer
out$year <- as.integer(out$year)

# Sort by country then year
out <- out[order(out$country, out$year), ]
rownames(out) <- NULL

write.csv(out, "clean_data/democracy_clean.csv", row.names = FALSE)
cat("democracy_clean.csv written:", nrow(out), "rows\n")
