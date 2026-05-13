# 01_clean_conflict.R
# Clean COW MIDB 5.0 data
# Input:  raw_data/midb(COW).csv
#         raw_data/cow_iso_crosswalk.csv
# Output: clean_data/conflict_clean.csv (long panel: ccode, year, revstate, fatality, hostlev)
#         averaged across simultaneous conflicts in the same country-year

raw <- read.csv(
  "raw_data/midb(COW).csv",
  stringsAsFactors = FALSE,
  check.names       = FALSE
)

out <- raw[, c("ccode", "styear", "endyear", "revstate", "fatality", "hostlev")]
out$ccode <- suppressWarnings(as.integer(out$ccode))

# Remove rows coded -9 (missing/unknown) in any of the three variables
out <- out[!is.na(out$ccode) & out$revstate != -9 & out$fatality != -9 & out$hostlev != -9, ]

# Expand each conflict spell into one row per year
rows <- vector("list", nrow(out))
for (i in seq_len(nrow(out))) {
  yrs <- seq(out$styear[i], out$endyear[i])
  rows[[i]] <- data.frame(
    ccode_cow = out$ccode[i],
    year     = yrs,
    revstate = out$revstate[i],
    fatality = out$fatality[i],
    hostlev  = out$hostlev[i],
    stringsAsFactors = FALSE
  )
}
expanded <- do.call(rbind, rows)

# Average scores within country-year across simultaneous conflicts
conflict_cy <- aggregate(cbind(revstate, fatality, hostlev) ~ ccode_cow + year,
                         data = expanded, FUN = mean)

# Map COW numeric codes to ISO-3 codes
crosswalk <- read.csv(
  "raw_data/cow_iso_crosswalk.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
crosswalk$ccode_cow <- suppressWarnings(as.integer(crosswalk$ccode_cow))
crosswalk$ccode_iso <- tolower(trimws(crosswalk$ccode_iso))
crosswalk$ccode_iso[crosswalk$ccode_iso == ""] <- NA

mapped <- merge(conflict_cy, crosswalk[, c("ccode_cow", "ccode_iso")],
                by = "ccode_cow", all.x = TRUE)
mapped <- mapped[!is.na(mapped$ccode_iso),
                 c("ccode_iso", "year", "revstate", "fatality", "hostlev")]

conflict_iso <- aggregate(cbind(revstate, fatality, hostlev) ~ ccode_iso + year,
                          data = mapped, FUN = mean)

# Zero-fill: build full grid of every mapped ISO country x every year in sample
all_ccodes <- sort(unique(conflict_iso$ccode_iso))
all_years  <- seq(min(conflict_iso$year, na.rm = TRUE), max(conflict_iso$year, na.rm = TRUE))
full_grid  <- expand.grid(ccode_iso = all_ccodes, year = all_years,
                          stringsAsFactors = FALSE)

panel <- merge(full_grid, conflict_iso, by = c("ccode_iso", "year"), all.x = TRUE)
panel$revstate[is.na(panel$revstate)] <- 0
panel$fatality[is.na(panel$fatality)] <- 0
panel$hostlev[is.na(panel$hostlev)]   <- 0

names(panel)[names(panel) == "ccode_iso"] <- "ccode"

panel <- panel[order(panel$ccode, panel$year), ]
rownames(panel) <- NULL

write.csv(panel, "clean_data/conflict_clean.csv", row.names = FALSE)
cat("conflict_clean.csv written:", nrow(panel), "rows,",
    length(unique(panel$ccode)), "countries\n")
