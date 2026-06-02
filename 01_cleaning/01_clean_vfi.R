# 01_clean_vfi.R
# Clean IMF Vertical Fiscal Imbalance data
# Input:  raw_data/vfi(FD-IMF).csv
# Output: clean_data/vfi_clean.csv

setwd("c:/Users/maxim/OneDrive/Documents/University - Year 3/POLS3029/paper/POLS3029")

raw <- read.csv("raw_data/vfi(FD-IMF).csv", stringsAsFactors = FALSE)

vfi <- raw[
  raw$INDICATOR == "IMF_FISCALDECENTRALIZATION_VFI" &
    raw$COMP_BREAKDOWN_1_LABEL == "Sector: Subnational Government",
  c("REF_AREA", "REF_AREA_LABEL", "TIME_PERIOD", "OBS_VALUE")
]

names(vfi) <- c("ccode", "country", "year", "vfi")

vfi$ccode <- tolower(vfi$ccode)
vfi$country <- tolower(vfi$country)
vfi$year <- as.integer(vfi$year)

vfi <- vfi[order(vfi$ccode, vfi$year), ]
rownames(vfi) <- NULL

write.csv(vfi, "clean_data/vfi_clean.csv", row.names = FALSE)

cat("vfi_clean.csv written:", nrow(vfi), "rows\n")