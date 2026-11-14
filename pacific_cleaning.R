# Simulation Week - Group 2
# University of the Pacific
# Xueying Luo

library(tidyverse)
library(lubridate)


# 1. Import the datasets

pacific_old <- read_csv(
  "data/raw/University of the Pacific/UoP 16-24.csv",
  col_names = FALSE,
  show_col_types = FALSE
)

pacific_new <- read_csv(
  "data/raw/University of the Pacific/UoP 24 - 26.csv",
  show_col_types = FALSE
)


# 2. Select and rename columns

pacific_old_clean <- pacific_old |>
  select(
    reported_raw = X10,
    case_number = X12,
    crime_location = X14,
    occurred_raw = X16,
    crime_description = X20
  )

pacific_new_clean <- pacific_new |>
  select(
    reported_raw = Reported,
    case_number = `Case #`,
    crime_location = Location,
    occurred_raw = Occurred,
    crime_description = Nature
  )


# 3. Clean dates and times - older dataset

pacific_old_clean <- pacific_old_clean |>
  mutate(
    date_reported = mdy(
      str_extract(reported_raw, "\\d{2}/\\d{2}/\\d{2}")
    ),
    time_reported = str_extract(
      reported_raw, "\\d{1,2}:\\d{2}"
    ),
    date_occurred = mdy(
      str_extract(occurred_raw, "\\d{2}/\\d{2}/\\d{2}")
    ),
    time_occurred = str_extract(
      occurred_raw, "\\d{1,2}:\\d{2}"
    )
  )


# 4. Clean dates and times - newer dataset

pacific_new_clean <- pacific_new_clean |>
  mutate(
    date_reported = mdy(
      str_extract(reported_raw, "\\d{2}/\\d{2}/\\d{4}")
    ),
    time_reported = str_extract(
      reported_raw, "\\d{1,2}:\\d{2}"
    ),
    date_occurred = mdy(
      str_extract(occurred_raw, "\\d{2}/\\d{2}/\\d{4}")
    ),
    time_occurred = str_extract(
      occurred_raw, "\\d{1,2}:\\d{2}"
    )
  )


# 5. Standardize crime types - older dataset

pacific_old_clean <- pacific_old_clean |>
  mutate(
    crime_type = case_when(
      str_detect(
        str_to_lower(crime_description),
        "sex offense|sexual battery|rape|indecent exposure"
      ) ~ "Sexual offense",
      
      str_detect(
        str_to_lower(crime_description),
        "robbery -"
      ) ~ "Robbery offense",
      
      str_detect(
        str_to_lower(crime_description),
        "burglary -|burglary-auto -"
      ) ~ "Burglary offense",
      
      str_detect(
        str_to_lower(crime_description),
        "assault|battery|domestic violence"
      ) ~ "Assault offense",
      
      str_detect(
        str_to_lower(crime_description),
        "alcohol -"
      ) ~ "Alcohol offense",
      
      str_detect(
        str_to_lower(crime_description),
        "dui"
      ) &
        str_detect(
          str_to_lower(crime_description),
          "b\\.a\\.|bac"
        ) ~ "Alcohol offense",
      
      TRUE ~ "Other"
    )
  )


# 6. Standardize crime types - newer dataset

pacific_new_clean <- pacific_new_clean |>
  mutate(
    crime_type = case_when(
      str_detect(
        str_to_lower(crime_description),
        "sexual|rape|fondling|indecent exposure"
      ) ~ "Sexual offense",
      
      str_detect(
        str_to_lower(crime_description),
        "^robbery"
      ) ~ "Robbery offense",
      
      str_detect(
        str_to_lower(crime_description),
        "^burglary:|^burglary -"
      ) ~ "Burglary offense",
      
      str_detect(
        str_to_lower(crime_description),
        "assault|battery|corporal injury"
      ) ~ "Assault offense",
      
      str_detect(
        str_to_lower(crime_description),
        "dui alcohol|disorderly conduct:alcohol"
      ) ~ "Alcohol offense",
      
      TRUE ~ "Other"
    )
  )


# 7. Add university, state, and legalization date

pacific_old_clean <- pacific_old_clean |>
  mutate(
    university = "University of the Pacific",
    state = "CA",
    gambling_legal = as.Date(NA)
  )

pacific_new_clean <- pacific_new_clean |>
  mutate(
    university = "University of the Pacific",
    state = "CA",
    gambling_legal = as.Date(NA)
  )


# 8. Remove exact duplicate source records

pacific_old_clean <- pacific_old_clean |>
  distinct()

pacific_new_clean <- pacific_new_clean |>
  distinct()


# 9. Select the required columns

pacific_old_final <- pacific_old_clean |>
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

pacific_new_final <- pacific_new_clean |>
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


# 10. Combine the datasets

pacific_combined <- bind_rows(
  pacific_old_final,
  pacific_new_final
)


# 11. Standardize time formats to HH:MM

pacific_combined <- pacific_combined |>
  mutate(
    time_reported = case_when(
      str_detect(time_reported, "^\\d:") ~
        paste0("0", time_reported),
      TRUE ~ time_reported
    ),
    
    time_occurred = case_when(
      str_detect(time_occurred, "^\\d:") ~
        paste0("0", time_occurred),
      TRUE ~ time_occurred
    )
  )


# 12. Check the combined dataset

dim(pacific_combined)

pacific_combined |>
  count(crime_type, sort = TRUE)

pacific_combined |>
  summarize(
    missing_reported = sum(is.na(date_reported)),
    missing_occurred = sum(is.na(date_occurred)),
    missing_time_reported = sum(is.na(time_reported)),
    missing_time_occurred = sum(is.na(time_occurred)),
    missing_location = sum(is.na(crime_location)),
    missing_legalization = sum(is.na(gambling_legal))
  ) |>
  print(width = Inf)

# Check that all non-missing times use HH:MM

pacific_combined |>
  summarize(
    short_reported_times = sum(
      str_detect(time_reported, "^\\d:"),
      na.rm = TRUE
    ),
    short_occurred_times = sum(
      str_detect(time_occurred, "^\\d:"),
      na.rm = TRUE
    )
  )

class(pacific_combined$date_reported)
class(pacific_combined$date_occurred)
class(pacific_combined$gambling_legal)


# 13. Export cleaned data

dir.create("data/cleaned", showWarnings = FALSE)

write_csv(
  pacific_combined,
  "data/cleaned/pacific_cleaned.csv"
)