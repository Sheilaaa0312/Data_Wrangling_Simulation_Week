# Simulation Week - Group 2
# Combine the six cleaned universities into the required submission CSV

library(tidyverse)

# 1. Import the corrected cleaned datasets. Read as text to preserve HH:MM.
pacific <- read_csv("data/cleaned/pacific_cleaned.csv",
                    col_types = cols(.default = col_character()))
ucf <- read_csv("data/cleaned/ucf_cleaned.csv",
                col_types = cols(.default = col_character()))
stetson <- read_csv("data/cleaned/stetson_cleaned.csv",
                    col_types = cols(.default = col_character()))
peiyun <- read_csv("data/cleaned/group2_three_universities_corrected.csv",
                   col_types = cols(.default = col_character()))

# 2. UCF original source includes one incident dated a year AFTER its report.
# The correct occurrence date is unknown, so preserve the incident and
# report date but do not invent an occurred date or time.
ucf <- ucf |>
  mutate(
    invalid_occurred = date_reported == "2023-10-28" &
      date_occurred == "2024-10-28",
    date_occurred = case_when(
      invalid_occurred ~ NA_character_, TRUE ~ date_occurred
    ),
    time_occurred = case_when(
      invalid_occurred ~ NA_character_, TRUE ~ time_occurred
    )
  ) |>
  select(-invalid_occurred)

# 3. Append all six universities without removing legitimate incidents.
group2_final <- bind_rows(pacific, ucf, stetson, peiyun) |>
  mutate(
    date_reported = as.Date(date_reported),
    date_occurred = as.Date(date_occurred),
    gambling_legal = as.Date(gambling_legal)
  )

# 4. Validate before submission.
stopifnot(nrow(group2_final) == 55218)
stopifnot(ncol(group2_final) == 9)
stopifnot(length(unique(group2_final$university)) == 6)
dim(group2_final)
group2_final |> count(university)
group2_final |> count(crime_type, sort = TRUE)
group2_final |>
  summarize(
    missing_reported = sum(is.na(date_reported)),
    missing_occurred = sum(is.na(date_occurred)),
    missing_time_reported = sum(is.na(time_reported)),
    missing_time_occurred = sum(is.na(time_occurred)),
    missing_location = sum(is.na(crime_location))
  )

# 5. Rename columns exactly as the professor requested.
submission <- group2_final |>
  select(
    `date reported` = date_reported,
    `date occurred` = date_occurred,
    `time reported` = time_reported,
    `time occurred` = time_occurred,
    state,
    university,
    `crime type` = crime_type,
    `gambling legal` = gambling_legal,
    `crime location` = crime_location
  )

# 6. Export exactly the required filename
write_csv(submission, "group_number_2.csv")
