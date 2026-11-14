# Simulation Week - Group 2
# Stetson University
# Xueying Luo

library(tidyverse)
library(readxl)


# 1. Import and combine the nine datasets

stetson_all <- bind_rows(
  read_excel(
    "data/raw/Stetson University/Stetson University 2018 Crime Log.xlsx",
    sheet = "Results"
  ) |> select(1:9),
  
  read_excel(
    "data/raw/Stetson University/Stetson University 2019 Crime Log.xlsx",
    sheet = "Results"
  ) |> select(1:9),
  
  read_excel(
    "data/raw/Stetson University/Stetson University 2020 Crime Log.xlsx",
    sheet = "Results"
  ) |> select(1:9),
  
  read_excel(
    "data/raw/Stetson University/Stetson University 2021 Crime Log.xlsx",
    sheet = "Results"
  ) |> select(1:9),
  
  read_excel(
    "data/raw/Stetson University/Stetson University 2022 Crime Log.xlsx",
    sheet = "Results"
  ) |> select(1:9),
  
  read_excel(
    "data/raw/Stetson University/Stetson University 2023 Crime Log.xlsx",
    sheet = "Results"
  ) |> select(1:9),
  
  read_excel(
    "data/raw/Stetson University/Stetson University 2024 Crime Log.xlsx",
    sheet = "Results"
  ) |> select(1:9),
  
  read_excel(
    "data/raw/Stetson University/Stetson University 2025 Crime Log.xlsx",
    sheet = "Results"
  ) |> select(1:9),
  
  read_excel(
    "data/raw/Stetson University/Stetson University 2026 Crime Log.xlsx",
    sheet = "Results"
  ) |> select(1:9)
)


# 2. Select and rename useful columns

stetson_clean <- stetson_all |>
  select(
    case_number = `CASE NUMBER`,
    occurred_raw = `INCIDENT OCCURRED DATE`,
    occurred_time_raw = `INCIDENT OCCURRED START TIME`,
    occurred_end_raw = `INCIDENT OCCURRED END TIME`,
    reported_raw = `REPORTED DATE`,
    reported_time_raw = `REPORTED TIME`,
    crime_description = `INCIDENT TYPE`,
    crime_location = `INCIDENT LOCATION`,
    location_detail = DISPOSITION
  )


# 3. Remove exact duplicate records

stetson_clean <- stetson_clean |>
  distinct()


# 4. Clean dates and times

stetson_clean <- stetson_clean |>
  mutate(
    date_reported = as.Date(reported_raw),
    date_occurred = as.Date(occurred_raw),
    
    time_reported = paste0(
      str_sub(reported_time_raw, 1, 2),
      ":",
      str_sub(reported_time_raw, 3, 4)
    ),
    
    time_occurred = paste0(
      str_sub(occurred_time_raw, 1, 2),
      ":",
      str_sub(occurred_time_raw, 3, 4)
    )
  )


# 5. Standardize crime categories

stetson_clean <- stetson_clean |>
  mutate(
    crime_type = case_when(
      str_detect(
        str_to_lower(crime_description),
        "sex offense|sexual|rape|fondling"
      ) ~ "Sexual offense",
      
      str_detect(
        str_to_lower(crime_description),
        "robbery"
      ) ~ "Robbery offense",
      
      str_detect(
        str_to_lower(crime_description),
        "burglary"
      ) ~ "Burglary offense",
      
      str_detect(
        str_to_lower(crime_description),
        "assault|battery|dating violence|domestic violence|fight"
      ) ~ "Assault offense",
      
      str_detect(
        str_to_lower(crime_description),
        "alcohol|liquor"
      ) ~ "Alcohol offense",
      
      TRUE ~ "Other"
    )
  )


# 6. Add university, state, and legalization date

stetson_clean <- stetson_clean |>
  mutate(
    university = "Stetson University",
    state = "FL",
    gambling_legal = as.Date("2021-05-25")
  )


# 7. Select the nine required columns

stetson_final <- stetson_clean |>
  select(
    date_reported,
    date_occurred,
    time_reported,
    time_occurred,
    state,
    university,
    crime_type,
    gambling_legal,
    crime_location
  )


# 8. Check the cleaned dataset

dim(stetson_final)
colnames(stetson_final)

stetson_final |>
  count(crime_type, sort = TRUE)

stetson_final |>
  summarize(
    missing_reported = sum(is.na(date_reported)),
    missing_occurred = sum(is.na(date_occurred)),
    missing_time_reported = sum(is.na(time_reported)),
    missing_time_occurred = sum(is.na(time_occurred)),
    missing_location = sum(is.na(crime_location))
  )


# 9. Export the cleaned dataset

write_csv(
  stetson_final,
  "data/cleaned/stetson_cleaned.csv"
)
