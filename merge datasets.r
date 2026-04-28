# =============================================================================
# Merge Script: Pancreatectomy Full Data + Endocrine/Exocrine Outcomes
# Study: Risk of endocrine and exocrine insufficiency after pancreatectomy
# Population in dm_data: patients WITHOUT pre-op DM, insulin, or DM meds
# Link key: dm_data$MRN <-> full_data$mayo_id
# =============================================================================

library(readxl)
library(readxl)
library(dplyr)
library(lubridate)

dm_data <- read_excel("C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/Diabetes_PD.xlsx")
full_data <- read_excel("C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/pancreatectomy_db.xlsx")

head(dm_data)
head(full_data)

str(dm_data)
str(full_data)

colnames(dm_data)
colnames(full_data)

# -----------------------------------------------------------------------------
# 2. CLEAN dm_data
#    - Remove trailing empty columns (...36 to ...40)
#    - Standardize key column name for join
# -----------------------------------------------------------------------------

dm_data_clean <- dm_data %>%
  # Drop auto-named empty columns from Excel
  select(-starts_with("...")) %>%
  # Rename MRN to mayo_id for consistent joining
  rename(mayo_id = MRN) %>%
  # Convert HbA1c_Y1 and eAG_Pre-OP / eAG_Y1 to numeric (stored as char)
  mutate(
    HbA1c_Y1      = suppressWarnings(as.numeric(HbA1c_Y1)),
    `eAG_Pre-OP`  = suppressWarnings(as.numeric(`eAG_Pre-OP`)),
    eAG_Y1        = suppressWarnings(as.numeric(eAG_Y1))
  )

cat("dm_data rows after cleaning:", nrow(dm_data_clean), "\n")
cat("Unique mayo_ids in dm_data: ", n_distinct(dm_data_clean$mayo_id), "\n\n")

# -----------------------------------------------------------------------------
# 3. FILTER full_data to PD (pancreaticoduodenectomy) patients only
#    Based on resection_type == "PPPD" or "Whipple" or similar
#    Adjust the filter values below to match your actual coding in full_data
# -----------------------------------------------------------------------------

# Inspect what resection types exist
cat("Resection types in full_data:\n")
print(table(full_data$resection_type, useNA = "ifany"))

# Filter to PD only — adjust values to match your data
pd_patients <- full_data %>%
  filter(grepl("PPPD|Whipple|PD|pancreaticoduodenectomy", 
               resection_type, ignore.case = TRUE))

cat("\nfull_data rows after filtering to PD only:", nrow(pd_patients), "\n")
cat("Unique mayo_ids in PD subset:", n_distinct(pd_patients$mayo_id), "\n\n")

# -----------------------------------------------------------------------------
# 4. CHECK OVERLAP BEFORE MERGING
# -----------------------------------------------------------------------------

ids_dm   <- unique(dm_data_clean$mayo_id)
ids_pd   <- unique(pd_patients$mayo_id)

n_matched   <- sum(ids_dm %in% ids_pd)
n_dm_only   <- sum(!ids_dm %in% ids_pd)   # in dm_data but not in full_data PD
n_pd_only   <- sum(!ids_pd %in% ids_dm)   # in full_data PD but not in dm_data (expected: had pre-op DM)

cat("=== ID overlap summary ===\n")
cat("IDs in dm_data:                  ", length(ids_dm), "\n")
cat("IDs in full_data (PD only):      ", length(ids_pd), "\n")
cat("IDs matched (will be merged):    ", n_matched, "\n")
cat("IDs in dm_data NOT in full_data: ", n_dm_only, " <-- investigate if > 0\n")
cat("IDs in full_data NOT in dm_data: ", n_pd_only, " (expected: pre-op DM patients)\n\n")

# Flag unmatched dm_data IDs for review
unmatched_dm_ids <- dm_data_clean %>%
  filter(!mayo_id %in% ids_pd) %>%
  select(mayo_id, DOS)

if (nrow(unmatched_dm_ids) > 0) {
  cat("WARNING: The following dm_data IDs did not match any full_data record:\n")
  print(unmatched_dm_ids)
  cat("\n")
}

# -----------------------------------------------------------------------------
# 5. MERGE — left join: keep all dm_data patients, bring in full_data variables
#    (right join / inner join options commented out below)
# -----------------------------------------------------------------------------

# LEFT JOIN: all dm_data patients retained; full_data vars added where available
merged_data <- dm_data_clean %>%
  left_join(pd_patients, by = "mayo_id", suffix = c("_dm", "_full"))

# # INNER JOIN (only patients present in BOTH datasets):
# merged_data <- dm_data_clean %>%
#   inner_join(pd_patients, by = "mayo_id", suffix = c("_dm", "_full"))

cat("Merged dataset rows:", nrow(merged_data), "\n")
cat("Merged dataset columns:", ncol(merged_data), "\n\n")

# -----------------------------------------------------------------------------
# 6. RESOLVE DUPLICATE DATE COLUMNS (DOS / dos)
#    dm_data has DOS, full_data has dos — keep dm_data's and rename clearly
# -----------------------------------------------------------------------------

merged_data <- merged_data %>%
  rename(
    DOS_pancreatectomy = DOS,   # from dm_data (surgery date)
    dos_full           = dos    # from full_data (same surgery date, cross-check)
  ) %>%
  # Optional: flag mismatches between the two surgery date fields
  mutate(
    date_mismatch_flag = case_when(
      is.na(DOS_pancreatectomy) | is.na(dos_full) ~ NA,
      as.Date(DOS_pancreatectomy) != as.Date(dos_full) ~ TRUE,
      TRUE ~ FALSE
    )
  )

n_mismatch <- sum(merged_data$date_mismatch_flag == TRUE, na.rm = TRUE)
if (n_mismatch > 0) {
  cat("WARNING:", n_mismatch, "patients have mismatched surgery dates between datasets.\n")
  cat("Review 'date_mismatch_flag' column.\n\n")
}

# -----------------------------------------------------------------------------
# 7. CREATE STUDY-RELEVANT DERIVED VARIABLES
# -----------------------------------------------------------------------------

merged_data <- merged_data %>%
  mutate(
    # --- Endocrine outcomes ---
    # New-onset DM after pancreatectomy (from dm_data; 0 = no DM, 1 = yes)
    new_onset_DM = DM,

    # Days from surgery to DM diagnosis
    days_to_DM = as.numeric(difftime(as.Date(DM_date),
                                     as.Date(DOS_pancreatectomy),
                                     units = "days")),

    # --- Exocrine outcomes ---
    # Pancreatic exocrine insufficiency flag (from dm_data)
    exo_insufficiency = Pancreatic_insufficiency,

    # Days from surgery to exocrine insufficiency diagnosis
    days_to_exo_insuff = as.numeric(difftime(
      suppressWarnings(as.Date(as.numeric(Date_of_Pancreatic_Ins),
                               origin = "1899-12-30")),
      as.Date(DOS_pancreatectomy),
      units = "days"
    )),

    # --- Symptom composite ---
    exo_symptoms_any = case_when(
      Abd_Pain == 1 | Diarrhea == 1 | steatorrhea == 1 ~ 1,
      TRUE ~ 0
    )
  )

# -----------------------------------------------------------------------------
# 8. SELECT AND ORDER KEY COLUMNS FOR FINAL EXPORT
#    Adjust this selection to include/exclude columns as needed
# -----------------------------------------------------------------------------

final_data <- merged_data %>%
  select(
    # Identifiers
    mayo_id,
    DOS_pancreatectomy,
    dos_full,
    date_mismatch_flag,

    # Demographics (from full_data)
    dob, age, sex, race, bmi,

    # Pre-op comorbidities (from full_data)
    hypertension, high_cholesterol_tg, hep_fib_cirr,
    hx_significant_etoh_use, tobacco,
    pancreatitis, pancreatitis_etiol,
    ppi, Steroids,
    ecog,

    # Surgery details (from full_data)
    resection_type, laparoscopic, op_time, ebl, surgeon, asa,
    gland_texture, panc_duct_mm, cbd_mm,
    vein_resxn, splenectomy,
    icu_days, los,
    popf, popf_grade, dge, dge_grade, pph, pph_grade,
    wound_inf, abd_abscess, cd_grade,

    # Pathology (from full_data)
    path_dx1_sub, path_dx1_specific,
    tumor_max, margin, lvi, pni,
    pt_stage_1, pn_stage_1,

    # Reason for pancreatectomy / pre-op dx (from dm_data)
    PD_Reason, Family_Hx_DM,

    # Pre-op glycemic status (from dm_data) — all should be non-DM by design
    `HbA1c_Pre-OP`, `eAG_Pre-OP`,

    # Post-op HbA1c trajectory
    HbA1c_Y1, HbA1c_Y2, HbA1c_Y3, HbA1c_Y4, HbA1c_Y5,
    HbA1c_Y6, HbA1c_Y7, HbA1c_Y8, HbA1c_Y9, HbA1c_Y10,

    # Post-op eAG trajectory
    eAG_Y1, eAG_Y2, eAG_Y3, eAG_Y4, eAG_Y5,

    # Endocrine outcomes
    new_onset_DM, DM_date, days_to_DM,
    Insulin, DM_Medication, DM_Medication_start_day,

    # Exocrine outcomes
    exo_insufficiency, Date_of_Pancreatic_Ins, days_to_exo_insuff,
    Abd_Pain, Diarrhea, PERT, `PERT enddate`, Cholestyramine,
    exo_symptoms_any,

    # Imaging follow-up
    `Post-op MR`, `Post-op CT`,

    # Survival / follow-up (from full_data)
    vital_fu_dt, vital_status_fu, surv_days, surv_mo,
    recurr, recurr_dt, recurr_type
  )

cat("Final export columns:", ncol(final_data), "\n")
cat("Final export rows:   ", nrow(final_data), "\n\n")

# -----------------------------------------------------------------------------
# 9. EXPORT TO CSV
# -----------------------------------------------------------------------------

output_path <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/datamerged_endo_exo_PD.csv"

write.csv(final_data, file = output_path, row.names = FALSE, na = "")

cat("✓ File exported successfully to:\n", output_path, "\n\n")

# -----------------------------------------------------------------------------
# 10. QUICK SUMMARY CHECKS
# -----------------------------------------------------------------------------

cat("=== Quick Summary ===\n")
cat("Total patients in merged dataset:", nrow(final_data), "\n")
cat("New-onset DM:", sum(final_data$new_onset_DM == 1, na.rm = TRUE), "\n")
cat("Exocrine insufficiency:", sum(final_data$exo_insufficiency == 1, na.rm = TRUE), "\n")
cat("On insulin post-op:", sum(final_data$Insulin == 1, na.rm = TRUE), "\n")
cat("On PERT post-op:", sum(final_data$PERT == 1, na.rm = TRUE), "\n")
cat("Median days to DM diagnosis:", 
    median(final_data$days_to_DM, na.rm = TRUE), "days\n")
cat("Median days to exocrine insuff:", 
    median(final_data$days_to_exo_insuff, na.rm = TRUE), "days\n")