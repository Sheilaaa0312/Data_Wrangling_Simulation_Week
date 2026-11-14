# IBM 5910 - Group 2 - Full RAW-source cleaning workflow
# Run in RStudio from the folder containing this script and the three raw files.
# install.packages(c("readxl", "dplyr", "stringr", "lubridate", "readr"))
library(readxl); library(dplyr); library(stringr); library(lubridate); library(readr)

michigan_file <- "Clery Crime and Fire Log - January 1 2016 to September 1 2025 - Requested 10-6-25.xlsx"
ohio_file <- "Daily Crime Log 01.01.2016 -09.01.2025.xls"
tcu_file <- "TCU Records(260421191447_daily-crime-and-fi).csv"

categories <- c("Alcohol offense", "Sexual offense", "Robbery offense", "Burglary offense", "Assault offense", "Other")
standard_cols <- c("date_reported", "date_occurred", "time_reported", "time_occurred", "state", "university", "crime_type", "gambling_legal", "crime_location")
classify <- function(x) {
 x <- str_to_lower(coalesce(as.character(x), ""))
 case_when(
  str_detect(x, "rape|sexual|sex offense|sex assault|fondling|indecent|sodomy|stalking.*sex") ~ "Sexual offense",
  str_detect(x, "robbery|carjack") ~ "Robbery offense",
  str_detect(x, "burglary|breaking and entering|break-in") ~ "Burglary offense",
  str_detect(x, "assault|battery|menacing|domestic violence|strangulation") ~ "Assault offense",
  str_detect(x, "alcohol|intoxicat|liquor|underage|open container|drunk|dui|ovi|dwi") ~ "Alcohol offense",
  TRUE ~ "Other")
}
fmt_date <- function(x) ifelse(is.na(x), NA_character_, format(as.Date(x), "%Y-%m-%d"))
fmt_time <- function(x) ifelse(is.na(x), NA_character_, format(as.POSIXct(x), "%H:%M"))
# Try several timestamp representations. Excel datetimes are numeric days since 1899-12-30.
parse_stamp <- function(x) {
 if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz="UTC"))
 if (inherits(x, "Date")) return(as.POSIXct(x, tz="UTC"))
 z <- as.character(x); z[z %in% c("", "NA", "Unknown", "N/A")] <- NA
 num <- suppressWarnings(as.numeric(z))
 result <- suppressWarnings(parse_date_time(z, orders=c("mdy IMp", "mdy HM", "mdy HMS", "mdy", "ymd HMS", "ymd HM", "ymd"), quiet=TRUE))
 result <- as.POSIXct(result, tz="UTC")
 ix <- !is.na(num) & num > 10000
 result[ix] <- as.POSIXct((num[ix] - 25569)*86400, origin="1970-01-01", tz="UTC")
 result
}
first_or_na <- function(x) { x <- str_trim(as.character(x)); x[x==""] <- NA_character_; x }
combine_location <- function(...) {
 x <- cbind(...); apply(x, 1, function(row) { row <- unique(na.omit(first_or_na(row))); if(length(row)) paste(row, collapse="; ") else NA_character_ })
}

# Michigan source: actually labels Michigan State University, NOT University of Michigan.
mi <- read_excel(michigan_file, col_types="text")
stopifnot(all(c("CleryOffenseDescription", "OccurredOn", "ReportedOn", "LocationName", "Location", "CrossStreet") %in% names(mi)))
mi_occur <- parse_stamp(mi$OccurredOn); mi_report <- parse_stamp(mi$ReportedOn)
mi_out <- tibble(date_reported=fmt_date(mi_report), date_occurred=fmt_date(mi_occur),
 time_reported=fmt_time(mi_report), time_occurred=fmt_time(mi_occur),
 state="MI", university="Michigan State University", crime_type=classify(mi$CleryOffenseDescription),
 gambling_legal="2021-01-22", crime_location=combine_location(mi$LocationName,mi$Location,mi$CrossStreet))

# Ohio State: timestamp columns are Excel dates; read_excel may yield datetime or text.
oh <- read_excel(ohio_file, col_types="text")
stopifnot(all(c("Reported_dt", "Occurrence_start_dt", "Offenses", "Loc_CSA", "Loc_Ext", "Loc_Patrol") %in% names(oh)))
oh_report <- parse_stamp(oh$Reported_dt); oh_occur <- parse_stamp(oh$Occurrence_start_dt)
oh_out <- tibble(date_reported=fmt_date(oh_report), date_occurred=fmt_date(oh_occur),
 time_reported=fmt_time(oh_report), time_occurred=fmt_time(oh_occur),
 state="OH", university="Ohio State University", crime_type=classify(oh$Offenses),
 gambling_legal="2023-01-01", crime_location=combine_location(oh$Loc_CSA,oh$Loc_Ext,oh$Loc_Patrol))

# TCU has date ranges in some source rows. Only parse unambiguous single dates/times.
tcu <- read_csv(tcu_file, col_types=cols(.default=col_character()), show_col_types=FALSE)
stopifnot(all(c("Type of Offense", "Date Reported", "Time Reported", "Date Occurred", "Time Occurred", "Location") %in% names(tcu)))
parse_single_date <- function(x) {
 x <- first_or_na(x)
 # Multiple date tokens mean ambiguous range; do not silently select a date.
 x[str_count(coalesce(x,""), "\\d{1,2}/\\d{1,2}/\\d{2,4}") != 1] <- NA_character_
 fmt_date(parse_stamp(x))
}
parse_single_time <- function(x) {
 x <- first_or_na(x)
 x[!str_detect(coalesce(x,""), "^\\d{1,2}:\\d{2}$")] <- NA_character_
 ifelse(is.na(x),NA_character_,sprintf("%02d:%02d",suppressWarnings(as.integer(str_extract(x,"^\\d+"))),suppressWarnings(as.integer(str_extract(x,"(?<=:)\\d{2}")))))
}
tcu_out <- tibble(date_reported=parse_single_date(tcu$`Date Reported`),
 date_occurred=parse_single_date(tcu$`Date Occurred`),
 time_reported=parse_single_time(tcu$`Time Reported`), time_occurred=parse_single_time(tcu$`Time Occurred`),
 state="TX", university="Texas Christian University", crime_type=classify(tcu$`Type of Offense`),
 gambling_legal=NA_character_, crime_location=first_or_na(tcu$Location))

# Preserve all rows. Future years are flagged, not guessed. Review audit against raw files.
outputs <- list(michigan=mi_out, ohio_state=oh_out, tcu=tcu_out)
audit <- list()
for (school in names(outputs)) {
 d <- outputs[[school]] %>% select(all_of(standard_cols))
 bad <- which(!is.na(d$date_occurred) & suppressWarnings(as.integer(substr(d$date_occurred,1,4))) > 2026)
 if(length(bad)) {
  audit[[school]] <- tibble(school=school, source_data_row=bad+1L, date_reported=d$date_reported[bad],
   suspect_date_occurred=d$date_occurred[bad], crime_location=d$crime_location[bad],
   resolution="Date set to NA pending verification against original log")
  d$date_occurred[bad] <- NA_character_
 }
 stopifnot(identical(names(d), standard_cols), all(d$crime_type %in% categories))
 outputs[[school]] <- d
 write_csv(d, paste0("raw_rebuilt_",school,".csv"), na="")
}
write_csv(bind_rows(outputs), "raw_rebuilt_three_universities_combined.csv", na="")
if(length(audit)) write_csv(bind_rows(audit), "raw_source_date_anomaly_audit.csv")
print(sapply(outputs,nrow))
cat("IMPORTANT: This is an independent raw-source reconstruction. Compare its row-level values and categories with the previously delivered cleaned CSVs before replacing the handoff files.\n")
