GROUP 2 — PEI YUN HANDOFF (November 2026)

USE FOR XUEYING:
- group2_three_universities_combined_revised.csv (42,014 rows, nine underscored columns)
- Individual *_clean_revised.csv files are for checking ONLY; do not append them again to the combined file.
- 01_revise_existing_cleaned_csvs.R reproduces the revised CSVs from the three included pre-cleaned CSVs.
- 02_rebuild_from_original_raw_logs.R is a separate raw-source cleaning script using readxl, dplyr, stringr, lubridate, readr. Put the original raw Excel/CSV files beside it before running. It is an independent reconstruction and may differ from the earlier cleaning decisions; validate outputs before replacing the delivered CSV.
- date_anomaly_audit.csv identifies six impossible future occurrence dates. They were blanked, NOT guessed. Raw-source correction still needs manual confirmation.

IMPORTANT: Michigan source identifies Michigan State University, while assignment names Michigan. Verify this with the instructor.
IMPORTANT: Texas sports betting legalization date is blank because it was not legalized in the provided study period.
IMPORTANT: The team requested underscores, although assignment PDF uses space-separated labels.

File count: Michigan 17,175; Ohio State 21,022; TCU 3,817; combined 42,014.
