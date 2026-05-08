# 09_clean_rai.R
# Clean RAI (Regional Authority Index) data
# Input:  raw_data/rai(UNCCH).csv
# Output: clean_data/rai_clean.csv (long: country_code, year, n_selfrule, n_sharedrule, n_RAI)

raw <- read.csv(
  "raw_data/rai(UNCCH).csv",
  stringsAsFactors = FALSE,
  check.names       = FALSE
)

out <- raw[, c("abbr_country", "year", "n_selfrule", "n_sharedrule", "n_RAI")]
names(out)[names(out) == "abbr_country"] <- "ccode"
out$ccode <- tolower(out$ccode)
out$year <- as.integer(out$year)

out <- out[order(out$ccode, out$year), ]
rownames(out) <- NULL

write.csv(out, "clean_data/rai_clean.csv", row.names = FALSE)
cat("rai_clean.csv written:", nrow(out), "rows,",
    length(unique(out$ccode)), "countries\n")
