input_path <- file.path("raw_data", "area(worldbank).csv")

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
  stop("Could not find raw_data/area(worldbank).csv from the current working directory.")
}

area_raw <- read.csv(
  input_path,
  skip = 3,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

year_cols <- grep("^[0-9]{4}$", names(area_raw), value = TRUE)

area_raw <- area_raw[area_raw[["Indicator Code"]] == "AG.LND.TOTL.K2", ]

area_wide <- area_raw[c("Country Name", "Country Code", year_cols)]
names(area_wide)[1:2] <- c("country_name", "country_code")

area_wide$country_name <- tolower(area_wide$country_name)
area_wide$country_code <- tolower(area_wide$country_code)

for (col in year_cols) {
  area_wide[[col]] <- as.numeric(area_wide[[col]])
}

area_wide <- area_wide[order(area_wide$country_code), ]

write.csv(area_wide, output_path, row.names = FALSE, quote = TRUE)

message("Saved cleaned wide panel dataset to: ", normalizePath(output_path, winslash = "/", mustWork = FALSE))