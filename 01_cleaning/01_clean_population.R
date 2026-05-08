input_path <- file.path("raw_data", "population(IMF).csv")

raw_filename <- basename(input_path)
base_name <- sub("\\(.*\\)", "", raw_filename)
base_name <- sub("\\.csv$", "", base_name, ignore.case = TRUE)
base_name <- gsub("[[:space:]]+", "_", trimws(base_name))

output_path <- file.path("clean_data", paste0(base_name, "_clean.csv"))

if (!file.exists(input_path)) {
  input_path <- file.path("..", input_path)
  output_path <- file.path("..", output_path)
}

if (!file.exists(input_path)) {
  stop("Could not find raw_data/population(IMF).csv from the current working directory.")
}

raw <- read.csv(
  input_path,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

year_cols <- grep("^[0-9]{4}$", names(raw), value = TRUE)

raw <- raw[raw[["OBS_MEASURE"]] == "OBS_VALUE", ]

wide <- raw[c("COUNTRY", "SERIES_CODE", year_cols)]
names(wide)[1:2] <- c("country", "ccode")

wide$country <- tolower(wide$country)
wide$ccode <- tolower(sub("\\..*$", "", wide$ccode))

for (col in year_cols) {
  wide[[col]] <- as.numeric(wide[[col]])
}

wide <- wide[order(wide$ccode), ]

write.csv(wide, output_path, row.names = FALSE, quote = TRUE)

message("Saved cleaned wide dataset to: ", normalizePath(output_path, winslash = "/", mustWork = FALSE))
