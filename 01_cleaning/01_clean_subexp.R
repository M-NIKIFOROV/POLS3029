# 10_clean_subexp.R
# Clean IMF expenditure decentralisation and tax revenue decentralisation
# Input:  raw_data/subexp-genexp(FD-IMF).csv
#         raw_data/subtaxrev-gentaxrev.csv
#         raw_data/nettran-subexp,nettran-subrev.csv  (two indicators: net transfers / own spending, / own revenue)
# Output: clean_data/subexp_clean.csv       (wide)
#         clean_data/subtaxrev_clean.csv     (wide)
#         clean_data/nettran_os_clean.csv    (wide: net transfers / own spending)
#         clean_data/nettran_or_clean.csv    (wide: net transfers / own revenue)

imf_wide <- function(path, indicator_pattern, outname) {
  raw <- read.csv(path, stringsAsFactors=FALSE, check.names=FALSE)
  raw <- raw[raw[["OBS_MEASURE"]] == "OBS_VALUE" &
             grepl(indicator_pattern, raw[["INDICATOR"]], ignore.case=TRUE), ]
  yr_cols <- grep("^[0-9]{4}$", names(raw), value=TRUE)
  wide <- data.frame(
    country = tolower(raw[["COUNTRY"]]),
    ccode = tolower(sub("\\..*$", "", raw[["SERIES_CODE"]])),

    raw[, yr_cols],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(wide)[-(1:2)] <- as.integer(yr_cols)
  wide <- wide[order(wide$country), ]
  rownames(wide) <- NULL
  write.csv(wide, outname, row.names=FALSE)
  cat(basename(outname), "written:", nrow(wide), "rows\n")
}

imf_wide("raw_data/subexp-genexp(FD-IMF).csv",
         "Expenditure decentralization",
         "clean_data/subexp_clean.csv")

imf_wide("raw_data/subtaxrev-gentaxrev.csv",
         "Tax revenue decentralization",
         "clean_data/subtaxrev_clean.csv")

imf_wide("raw_data/nettran-subexp,nettran-subrev.csv",
         "Ratio of net transfers to own spending",
         "clean_data/nettran_os_clean.csv")

imf_wide("raw_data/nettran-subexp,nettran-subrev.csv",
         "Ratio of net transfers to own revenue",
         "clean_data/nettran_or_clean.csv")
