# 11_clean_ideology.R
# Clean ParlGov ideology data
# Constructs country-year vote-share weighted mean left-right score
# from election results, carried forward between elections
# Input:  raw_data/ideology(parlgov).csv
# Output: clean_data/ideology_clean.csv (long: country_code, year, ideology)

raw <- read.csv(
  "raw_data/ideology(parlgov).csv",
  stringsAsFactors = FALSE,
  check.names       = FALSE
)

# Keep parliament elections only, drop rows with missing left_right or vote_share
raw <- raw[raw$election_type == "parliament" & 
           !is.na(raw$left_right) & 
           !is.na(raw$vote_share), ]

raw$election_year <- as.integer(substr(raw$election_date, 1, 4))
raw$ccode  <- tolower(raw$country_name_short)

# Compute vote-share weighted mean ideology per country-election
agg <- aggregate(
  cbind(wt_lr = left_right * vote_share, wt = vote_share) ~ ccode + election_year,
  data = raw,
  FUN  = sum
)
agg$ideology <- agg$wt_lr / agg$wt
agg <- agg[, c("ccode", "election_year", "ideology")]
agg <- agg[order(agg$ccode, agg$election_year), ]

# Carry forward election-year ideology to all years until next election
year_range <- seq(min(agg$election_year), 2020L)
countries  <- sort(unique(agg$ccode))

panel_rows <- vector("list", length(countries))
for (i in seq_along(countries)) {
  cc   <- countries[i]
  sub  <- agg[agg$ccode == cc, ]
  df   <- data.frame(ccode = cc, year = year_range, ideology = NA_real_,
                     stringsAsFactors = FALSE)
  # Fill election years, then carry forward
  df$ideology[df$year %in% sub$election_year] <-
    sub$ideology[match(df$year[df$year %in% sub$election_year], sub$election_year)]
  # Forward-fill
  last <- NA_real_
  for (j in seq_len(nrow(df))) {
    if (!is.na(df$ideology[j])) last <- df$ideology[j]
    else df$ideology[j] <- last
  }
  panel_rows[[i]] <- df[!is.na(df$ideology), ]
}

panel <- do.call(rbind, panel_rows)
panel <- panel[order(panel$ccode, panel$year), ]
rownames(panel) <- NULL

write.csv(panel, "clean_data/ideology_clean.csv", row.names = FALSE)
cat("ideology_clean.csv written:", nrow(panel), "rows,",
    length(unique(panel$ccode)), "countries\n")
