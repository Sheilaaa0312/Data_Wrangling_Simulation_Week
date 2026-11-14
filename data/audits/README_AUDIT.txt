Simulation Week - Group 2 - Final harmonized data audit

Source files reviewed:
  - Michigan State original Excel crime log (17,175 records)
  - Ohio State original legacy .xls workbook (21,023 rows including an unusable/blank row)
  - Texas Christian original CSV
  - University of the Pacific older and newer raw CSVs
  - University of Central Florida raw crime log CSV
  - Stetson annual Excel logs

Output: group number 2.csv (55,218 incident records, nine required fields)

Corrections made to previously cleaned files:
1. Michigan State: 503 offenses mentioning stalking had been coded as Sexual offense.
   Compared each against the source descriptions using the preserved source row order;
   487 are now Other; 10 explicitly include assault/dating violence and are now
   Assault offense; six explicitly include a sexual offense and remain Sexual offense.
   Every evaluated record is listed in michigan_stalking_classification_audit.csv.
2. Ohio State: two source records with occurrence date 1900-01-01 and one record
   reported in 2023 with occurrence date in 2025 had ambiguous/invalid occurrence
   timestamps. Their occurrence dates and times are set to missing, with all rows kept.
3. UCF: source contains one report dated 2023-10-28 with occurrence dated
   2024-10-28. Its occurrence date/time are set to missing pending confirmation.
4. The six previously flagged future-year source dates in Ohio State/TCU already
   have missing occurrence dates in Pei Yun's cleaned file and remain missing.
5. No dates were changed simply because the crime was reported after it occurred.
6. California/Texas gambling legalization dates are missing intentionally.

Remaining limits:
- Other records with reported dates earlier than occurred dates may be source errors;
  they are not automatically changed without source-specific verification.
- Legalization milestones differ in meaning: FL 2021-05-25 is a legal compact
  milestone; MI 2021-01-22 and OH 2023-01-01 are online betting launches.
  Define this carefully before any pre/post causal interpretation in the report.
- The above fixes build upon Pei Yun's cleaned CSV, not a complete independently
  reimplemented raw-to-final pipeline for every school. Reproduce the final dataset
  by running review_peiyun_data.R, then combine_cleaned_data.R.
- CSV blank cells represent missing observations, not necessarily missing records.

The final CSV filename and column names follow the assignment handout.
