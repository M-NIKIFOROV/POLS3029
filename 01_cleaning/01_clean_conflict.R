# 06_clean_conflict.R
# Clean COW MIDB 5.0 data
# Input:  raw_data/midb(COW).csv
# Output: clean_data/conflict_clean.csv (long panel: ccode, year, revstate, fatality, hostlev)
#         averaged across simultaneous conflicts in the same country-year

raw <- read.csv(
  "raw_data/midb(COW).csv",
  stringsAsFactors = FALSE,
  check.names       = FALSE
)

out <- raw[, c("stabb", "styear", "endyear", "revstate", "fatality", "hostlev")]
out$stabb <- tolower(out$stabb)

# Remove rows coded -9 (missing/unknown) in any of the three variables
out <- out[out$revstate != -9 & out$fatality != -9 & out$hostlev != -9, ]

# Expand each conflict spell into one row per year
rows <- vector("list", nrow(out))
for (i in seq_len(nrow(out))) {
  yrs <- seq(out$styear[i], out$endyear[i])
  rows[[i]] <- data.frame(
    ccode    = out$stabb[i],
    year     = yrs,
    revstate = out$revstate[i],
    fatality = out$fatality[i],
    hostlev  = out$hostlev[i],
    stringsAsFactors = FALSE
  )
}
expanded <- do.call(rbind, rows)

# Average scores within country-year across simultaneous conflicts
conflict_cy <- aggregate(cbind(revstate, fatality, hostlev) ~ ccode + year,
                         data = expanded, FUN = mean)

# Zero-fill: build full grid of every country x every year in the COW universe
all_ccodes <- sort(unique(out$stabb))
all_years  <- seq(min(out$styear, na.rm = TRUE), max(out$endyear, na.rm = TRUE))
full_grid  <- expand.grid(ccode = all_ccodes, year = all_years,
                          stringsAsFactors = FALSE)

panel <- merge(full_grid, conflict_cy, by = c("ccode", "year"), all.x = TRUE)
panel$revstate[is.na(panel$revstate)] <- 0
panel$fatality[is.na(panel$fatality)] <- 0
panel$hostlev[is.na(panel$hostlev)]   <- 0

panel <- panel[order(panel$ccode, panel$year), ]
rownames(panel) <- NULL

write.csv(panel, "clean_data/conflict_clean.csv", row.names = FALSE)
cat("conflict_clean.csv written:", nrow(panel), "rows,",
    length(unique(panel$ccode)), "countries\n")
