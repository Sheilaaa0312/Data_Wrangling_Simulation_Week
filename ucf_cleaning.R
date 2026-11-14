# Simulation Week - Group 2
# University of Central Florida
# Xueying Luo

library(tidyverse)
library(lubridate)


# 1. Import the dataset

ucf <- read_csv(
  "data/raw/University of Central Florida/Clery Log 2016- present(Sheet).csv",
  col_names = FALSE,
  col_types = "cccccccccccccc"
)


# 2. Remove header rows and select useful columns

ucf_clean <- ucf |>
  filter(!is.na(X4), X1 != "Case #") |>
  select(
    case_number = X1,
    crime_description = X4,
    reported_raw = X7,
    occurred_raw = X8,
    occurred_end_raw = X9,
    crime_location = X10
  )


# 3. Clean dates and times

ucf_clean <- ucf_clean |>
  mutate(
    date_reported = mdy(
      str_extract(reported_raw, "\\d{2}/\\d{2}/\\d{2}")
    ),
    date_occurred = mdy(
      str_extract(occurred_raw, "\\d{2}/\\d{2}/\\d{2}")
    ),
    time_reported = str_extract(
      reported_raw, "\\d{2}:\\d{2}"
    ),
    time_occurred = str_extract(
      occurred_raw, "\\d{2}:\\d{2}"
    )
  )


# 4. Remove exact duplicate records

ucf_clean <- ucf_clean |>
  distinct()


# 5. Standardize crime categories

ucf_clean <- ucf_clean |>
  mutate(
    crime_type = case_when(
      str_detect(
        str_to_lower(crime_description),
        "sexual|sex offense|rape|fondling|indecent exposure"
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
        "assault|battery|dating violence|domestic violence"
      ) ~ "Assault offense",
      
      str_detect(
        str_to_lower(crime_description),
        "liquor|alcohol"
      ) ~ "Alcohol offense",
      
      TRUE ~ "Other"
    )
  )


# 6. Add university, state, and legalization date

ucf_clean <- ucf_clean |>
  mutate(
    university = "University of Central Florida",
    state = "FL",
    gambling_legal = as.Date("2021-05-25")
  )


# 7. Select the nine required columns

ucf_final <- ucf_clean |>
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

dim(ucf_final)
colnames(ucf_final)

ucf_final |>
  count(crime_type, sort = TRUE)

ucf_final |>
  summarize(
    missing_reported = sum(is.na(date_reported)),
    missing_occurred = sum(is.na(date_occurred)),
    missing_time_reported = sum(is.na(time_reported)),
    missing_time_occurred = sum(is.na(time_occurred)),
    missing_location = sum(is.na(crime_location))
  )


# 9. Export the cleaned dataset

write_csv(
  ucf_final,
  "data/cleaned/ucf_cleaned.csv"
)



