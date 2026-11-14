# Simulation Week - Group 2
# Michigan State, Ohio State, and TCU
# Pei Yun Chuang

library(tidyverse)


# 1. Import the three cleaned datasets

michigan <- read_csv(
  "group2_michigan_clean.csv",
  col_types = cols(.default = col_character())
)

ohio <- read_csv(
  "group2_ohio_state_clean.csv",
  col_types = cols(.default = col_character())
)

tcu <- read_csv(
  "group2_tcu_clean.csv",
  col_types = cols(.default = col_character())
)


# 2. Combine the three datasets

peiyun <- bind_rows(
  michigan,
  ohio,
  tcu
)


# 3. Standardize column names

peiyun <- peiyun |>
  select(
    date_reported = `date reported`,
    date_occurred = `date occurred`,
    time_reported = `time reported`,
    time_occurred = `time occurred`,
    state,
    university,
    crime_type = `crime type`,
    gambling_legal = `gambling legal`,
    crime_location = `crime location`
  )


# 4. Standardize crime categories

peiyun <- peiyun |>
  mutate(
    crime_type = case_when(
      str_to_lower(crime_type) == "alcohol offense" ~ "Alcohol offense",
      str_to_lower(crime_type) == "sexual offense" ~ "Sexual offense",
      str_to_lower(crime_type) == "robbery offense" ~ "Robbery offense",
      str_to_lower(crime_type) == "burglary offense" ~ "Burglary offense",
      str_to_lower(crime_type) == "assault offense" ~ "Assault offense",
      str_to_lower(crime_type) == "other" ~ "Other",
      TRUE ~ NA_character_
    )
  )


# 5. Save the records with invalid future occurrence dates

date_anomalies <- peiyun |>
  filter(
    !is.na(date_occurred),
    date_occurred > "2026-11-09"
  )

write_csv(
  date_anomalies,
  "date_anomaly_audit.csv"
)


# 6. Set invalid future occurrence dates to NA

peiyun <- peiyun |>
  mutate(
    date_occurred = case_when(
      !is.na(date_occurred) &
        date_occurred > "2026-11-09" ~ NA_character_,
      TRUE ~ date_occurred
    ),
    date_reported = as.Date(date_reported),
    date_occurred = as.Date(date_occurred),
    gambling_legal = as.Date(gambling_legal)
  )


# 7. Check the results

dim(peiyun)
colnames(peiyun)

peiyun |>
  count(university)

peiyun |>
  count(crime_type, sort = TRUE)

date_anomalies |>
  count(university)

peiyun |>
  summarize(
    missing_reported = sum(is.na(date_reported)),
    missing_occurred = sum(is.na(date_occurred)),
    missing_time_reported = sum(is.na(time_reported)),
    missing_time_occurred = sum(is.na(time_occurred)),
    missing_location = sum(is.na(crime_location))
  )


# 8. Export the revised combined dataset

write_csv(
  peiyun,
  "group2_three_universities_combined_revised.csv"
)

