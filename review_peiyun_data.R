# Simulation Week - Group 2
# Review the cleaned Michigan State, Ohio State, and TCU datasets
# Source comparisons documented in data/audits/

library(tidyverse)
library(readxl)

# 1. Import Pei Yun's harmonized dataset and the Michigan State source log
peiyun <- read_csv(
  "data/cleaned/group2_three_universities_combined_revised.csv",
  col_types = cols(.default = col_character())
)

michigan_raw <- read_excel(
  "data/raw/Michigan State University/Clery Crime and Fire Log - January 1 2016 to September 1 2025 - Requested 10-6-25.xlsx"
)

michigan <- peiyun |> filter(university == "Michigan State University")
ohio <- peiyun |> filter(university == "Ohio State University")
tcu <- peiyun |> filter(university == "Texas Christian University")

# The cleaned Michigan State file is in the original source row order.
# Check this before matching each row with its original offense description.
stopifnot(nrow(michigan) == nrow(michigan_raw))

# 2. Correct Michigan State records containing stalking descriptions.
# An explicit sexual offense takes priority. Assault/dating violence is next.
# Stalking without one of these descriptions is Other in our six categories.
source_description <- str_to_lower(michigan_raw$CleryOffenseDescription)

michigan <- michigan |>
  mutate(
    crime_type = case_when(
      str_detect(source_description, "stalk") &
        str_detect(source_description, "rape|sexual|fondling|sex offense|indecent|sodomy|csc") ~ "Sexual offense",
      str_detect(source_description, "stalk") &
        str_detect(source_description, "assault|battery|dating violence|domestic violence") ~ "Assault offense",
      str_detect(source_description, "stalk") ~ "Other",
      TRUE ~ crime_type
    )
  )


# Review additional Michigan State crime classifications

michigan <- michigan |>
  mutate(
    crime_type = case_when(
      crime_type == "Sexual offense" &
        str_detect(source_description, "dating violence") &
        !str_detect(source_description, "rape|sexual assault|fondling|sex offense") ~
        "Assault offense",
      
      crime_type == "Sexual offense" &
        str_detect(source_description, "sexual orientation") &
        str_detect(source_description, "assault") &
        !str_detect(source_description, "rape|sexual assault|fondling|sex offense") ~
        "Assault offense",
      
      crime_type == "Sexual offense" &
        str_detect(source_description, "sexual orientation") &
        !str_detect(source_description, "rape|sexual assault|fondling|sex offense") ~
        "Other",
      
      TRUE ~ crime_type
    )
  )

# 3. Ohio State source dates: two 1900 placeholders and a 2025 date
# attached to a report filed in September 2023. True occurrence dates
# are not established; keep the records and use NA for occurred date/time.
ohio <- ohio |>
  mutate(
    invalid_occurred =
      (date_occurred == "1900-01-01" &
         date_reported %in% c("2023-12-19", "2023-11-28")) |
      (date_reported == "2023-09-14" & date_occurred == "2025-09-13"),
    date_occurred = case_when(
      invalid_occurred ~ NA_character_,
      TRUE ~ date_occurred
    ),
    time_occurred = case_when(
      invalid_occurred ~ NA_character_,
      TRUE ~ time_occurred
    )
  ) |>
  select(-invalid_occurred)

# 4. Keep every incident and recombine the three cleaned universities.
peiyun_corrected <- bind_rows(michigan, ohio, tcu)

stopifnot(nrow(peiyun_corrected) == 42014)
peiyun_corrected |> count(university)
peiyun_corrected |> count(crime_type, sort = TRUE)

write_csv(
  peiyun_corrected,
  "data/cleaned/group2_three_universities_corrected.csv"
)
