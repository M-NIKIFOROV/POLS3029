input_path <- file.path("raw_data", "balance-revenue-spending(FM).csv")

output_revenue_path <- file.path("clean_data", "revenue_clean.csv")
output_balance_path <- file.path("clean_data", "balance_clean.csv")
output_expenditure_path <- file.path("clean_data", "expenditure_clean.csv")

if (!file.exists(input_path)) {
  input_path <- file.path("..", input_path)
  output_revenue_path <- file.path("..", output_revenue_path)
  output_balance_path <- file.path("..", output_balance_path)
  output_expenditure_path <- file.path("..", output_expenditure_path)
}

if (!file.exists(input_path)) {
  stop("Could not find raw_data/balance-revenue-spending(FM).csv from the current working directory.")
}

raw <- read.csv(
  input_path,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

year_cols <- grep("^[0-9]{4}$", names(raw), value = TRUE)

raw <- raw[raw[["OBS_MEASURE"]] == "OBS_VALUE", ]

indicator_type <- rep(NA_character_, nrow(raw))
indicator_type[grepl("^Revenue", raw[["INDICATOR"]], ignore.case = TRUE)] <- "revenue"
indicator_type[grepl("^Expenditure", raw[["INDICATOR"]], ignore.case = TRUE)] <- "expenditure"
indicator_type[grepl("^Net lending", raw[["INDICATOR"]], ignore.case = TRUE)] <- "balance"

raw$indicator_type <- indicator_type
raw <- raw[!is.na(raw$indicator_type), ]

long <- data.frame(
  country_name = rep(tolower(raw[["COUNTRY"]]), times = length(year_cols)),
  country_code = rep(tolower(sub("\\..*$", "", raw[["SERIES_CODE"]])), times = length(year_cols)),
  indicator_type = rep(raw$indicator_type, times = length(year_cols)),
  year = rep(as.integer(year_cols), each = nrow(raw)),
  value = as.numeric(unlist(raw[year_cols], use.names = FALSE)),
  stringsAsFactors = FALSE
)

long <- long[!is.na(long$value), ]
long <- long[order(long$country_code, long$year, long$indicator_type), ]

build_wide <- function(data_long, indicator_name) {
  subset_long <- data_long[data_long$indicator_type == indicator_name, c("country_name", "country_code", "year", "value")]

  wide <- reshape(
    subset_long,
    idvar = c("country_name", "country_code"),
    timevar = "year",
    direction = "wide"
  )

  names(wide) <- sub("^value\\.", "", names(wide))
  year_names <- grep("^[0-9]{4}$", names(wide), value = TRUE)
  ordered_names <- c("country_name", "country_code", as.character(sort(as.integer(year_names))))
  wide <- wide[ordered_names]
  wide[order(wide$country_code), ]
}

wide_revenue <- build_wide(long, "revenue")
wide_balance <- build_wide(long, "balance")
wide_expenditure <- build_wide(long, "expenditure")

write.csv(wide_revenue, output_revenue_path, row.names = FALSE, quote = TRUE)
write.csv(wide_balance, output_balance_path, row.names = FALSE, quote = TRUE)
write.csv(wide_expenditure, output_expenditure_path, row.names = FALSE, quote = TRUE)

message("Saved cleaned wide revenue dataset to: ", normalizePath(output_revenue_path, winslash = "/", mustWork = FALSE))
message("Saved cleaned wide balance dataset to: ", normalizePath(output_balance_path, winslash = "/", mustWork = FALSE))
message("Saved cleaned wide expenditure dataset to: ", normalizePath(output_expenditure_path, winslash = "/", mustWork = FALSE))
