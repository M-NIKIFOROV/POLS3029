# 06_clean_conflict.R
# Clean COW conflict (MID) data
# Input:  raw_data/conflict(COW).csv
# Output: clean_data/conflict_clean.csv (wide: ccode, year cols 1816-2014,
#                                         hostlev per year; 0 = no conflict,
#                                         max hostlev when conflicts overlap)

raw <- read.csv(
  "raw_data/conflict(COW).csv",
  stringsAsFactors = FALSE,
  check.names       = FALSE
)

out <- raw[, c("stabb", "styear", "endyear", "hostlev")]
names(out)[names(out) == "stabb"] <- "ccode"
out$ccode <- tolower(out$ccode)

# Expand each conflict spell into one row per year
rows <- vector("list", nrow(out))
for (i in seq_len(nrow(out))) {
  yrs <- seq(out$styear[i], out$endyear[i])
  rows[[i]] <- data.frame(
    ccode   = out$ccode[i],
    year    = yrs,
    hostlev = out$hostlev[i],
    stringsAsFactors = FALSE
  )
}
expanded <- do.call(rbind, rows)

# For country-years with multiple conflicts, keep the max hostlev
panel_long <- aggregate(hostlev ~ ccode + year, data = expanded, FUN = max)

# Build full grid of every country x every year (1816-2014), fill 0 for no conflict
all_ccodes <- sort(unique(panel_long$ccode))
all_years  <- seq(1816L, 2014L)
full_grid  <- expand.grid(ccode = all_ccodes, year = all_years, stringsAsFactors = FALSE)

panel_long_full <- merge(full_grid, panel_long, by = c("ccode", "year"), all.x = TRUE)
panel_long_full$hostlev[is.na(panel_long_full$hostlev)] <- 0L

# Pivot to wide: one row per country, year cols
wide <- reshape(
  panel_long_full,
  idvar     = "ccode",
  timevar   = "year",
  direction = "wide"
)
names(wide) <- sub("^hostlev\\.", "", names(wide))
year_names <- grep("^[0-9]{4}$", names(wide), value = TRUE)
wide <- wide[, c("ccode", as.character(sort(as.integer(year_names))))]
wide <- wide[order(wide$ccode), ]
rownames(wide) <- NULL

write.csv(wide, "clean_data/conflict_clean.csv", row.names = FALSE)
cat("conflict_clean.csv written:", nrow(wide), "rows\n")
