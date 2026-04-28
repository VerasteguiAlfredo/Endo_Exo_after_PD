# =============================================================================
# Inspect & Clean: merged_endo_exo_PD.csv  (FIXED VERSION)
# =============================================================================

library(dplyr)
library(lubridate)
library(readr)
library(janitor)

# -----------------------------------------------------------------------------
# 1. LOAD
# -----------------------------------------------------------------------------

data_path   <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/datamerged_endo_exo_PD.csv"
output_path <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/datamerged_endo_exo_PD_clean.csv"
rds_path    <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/datamerged_endo_exo_PD_clean.rds"

raw <- read_csv(data_path, show_col_types = FALSE)

cat("Dimensions on load:", nrow(raw), "rows x", ncol(raw), "cols\n\n")

# -----------------------------------------------------------------------------
# 2. CLEAN COLUMN NAMES
# -----------------------------------------------------------------------------

df <- raw %>% clean_names()

cat("Column names after cleaning:\n")
print(colnames(df))
cat("\n")

# -----------------------------------------------------------------------------
# 3. HELPER: convert Excel serial OR date string to Date
# -----------------------------------------------------------------------------

excel_or_date <- function(x) {
  num <- suppressWarnings(as.numeric(x))
  result <- as.Date(rep(NA, length(x)))
  # Where value is a numeric Excel serial, convert from Excel origin
  result[!is.na(num)] <- as.Date(num[!is.na(num)], origin = "1899-12-30")
  # Where value is already a date string, parse directly
  result[is.na(num) & !is.na(x)] <- as.Date(x[is.na(num) & !is.na(x)])
  return(result)
}

# -----------------------------------------------------------------------------
# 4. FIX VARIABLE TYPES
# -----------------------------------------------------------------------------

df <- df %>%
  mutate(

    # --- Dates already parsed correctly by read_csv ---
    dos_pancreatectomy = as_date(dos_pancreatectomy),
    dos_full           = as_date(dos_full),
    dm_date            = as_date(dm_date),
    dob                = as_date(dob),
    vital_fu_dt        = as_date(vital_fu_dt),
    pert_enddate       = as_date(pert_enddate),

    # --- Dates stored as Excel serials (character) ---
    date_of_pancreatic_ins  = excel_or_date(date_of_pancreatic_ins),
    dm_medication_start_day = excel_or_date(dm_medication_start_day),
    post_op_mr              = excel_or_date(post_op_mr),
    post_op_ct              = excel_or_date(post_op_ct),
    recurr_dt               = excel_or_date(recurr_dt),

    # --- Factors ---
    sex               = factor(sex, levels = c("F", "M")),
    race              = factor(race),
    tobacco           = factor(tobacco, levels = c("never", "past", "current")),
    resection_type    = factor(resection_type),
    laparoscopic      = factor(laparoscopic),
    asa               = factor(asa, levels = c("I", "II", "III", "IV")),
    vital_status_fu   = factor(vital_status_fu),
    recurr_type       = factor(recurr_type),
    pd_reason         = factor(pd_reason),
    path_dx1_sub      = factor(path_dx1_sub),
    path_dx1_specific = factor(path_dx1_specific),
    margin            = factor(margin),
    popf_grade        = factor(popf_grade, levels = c("A", "B", "C")),
    dge_grade         = factor(dge_grade,  levels = c("A", "B", "C")),
    pph_grade         = factor(pph_grade,  levels = c("A", "B", "C")),
    cd_grade          = factor(cd_grade),
    pancreatitis      = factor(pancreatitis),
    gland_texture     = factor(gland_texture),
    pt_stage_1        = factor(pt_stage_1),
    pn_stage_1        = factor(pn_stage_1),

    # --- Binary -> integer (0/1) ---
    across(c(new_onset_dm, exo_insufficiency, insulin, pert,
             hypertension, high_cholesterol_tg, hep_fib_cirr,
             hx_significant_etoh_use, ppi, steroids,
             vein_resxn, splenectomy, popf, dge, pph,
             wound_inf, abd_abscess, recurr, lvi, pni,
             abd_pain, diarrhea, cholestyramine, family_hx_dm,
             exo_symptoms_any), as.integer),

    # --- Numeric ---
    across(c(age, bmi, op_time, ebl, los, icu_days,
             panc_duct_mm, cbd_mm, tumor_max,
             surv_days, surv_mo,
             hb_a1c_pre_op, hb_a1c_y1, hb_a1c_y2, hb_a1c_y3,
             hb_a1c_y4, hb_a1c_y5, hb_a1c_y6, hb_a1c_y7,
             hb_a1c_y8, hb_a1c_y9, hb_a1c_y10,
             e_ag_pre_op, e_ag_y1, e_ag_y2, e_ag_y3,
             e_ag_y4, e_ag_y5,
             days_to_dm, days_to_exo_insuff), as.numeric)
  )

# -----------------------------------------------------------------------------
# 5. VERIFY DATE CONVERSIONS
# -----------------------------------------------------------------------------

cat("=== Date conversion spot-check ===\n")
cat("date_of_pancreatic_ins range:",
    format(min(df$date_of_pancreatic_ins, na.rm = TRUE)), "to",
    format(max(df$date_of_pancreatic_ins, na.rm = TRUE)), "\n")
cat("post_op_mr range            :",
    format(min(df$post_op_mr, na.rm = TRUE)), "to",
    format(max(df$post_op_mr, na.rm = TRUE)), "\n")
cat("post_op_ct range            :",
    format(min(df$post_op_ct, na.rm = TRUE)), "to",
    format(max(df$post_op_ct, na.rm = TRUE)), "\n")
cat("recurr_dt range             :",
    format(min(df$recurr_dt, na.rm = TRUE)), "to",
    format(max(df$recurr_dt, na.rm = TRUE)), "\n\n")

# -----------------------------------------------------------------------------
# 6. DERIVED VARIABLES
# -----------------------------------------------------------------------------

df <- df %>%
  mutate(

    age_at_surgery = as.numeric(difftime(dos_pancreatectomy, dob,
                                         units = "days")) / 365.25,

    followup_months = as.numeric(difftime(vital_fu_dt, dos_pancreatectomy,
                                          units = "days")) / 30.44,

    # Recalculate time-to-events from clean dates
    days_to_dm         = as.numeric(difftime(dm_date, dos_pancreatectomy,
                                             units = "days")),
    days_to_exo_insuff = as.numeric(difftime(date_of_pancreatic_ins,
                                             dos_pancreatectomy,
                                             units = "days")),

    months_to_dm         = days_to_dm / 30.44,
    months_to_exo_insuff = days_to_exo_insuff / 30.44,

    # HbA1c summary variables
    hba1c_peak = pmax(hb_a1c_y1, hb_a1c_y2, hb_a1c_y3, hb_a1c_y4, hb_a1c_y5,
                      hb_a1c_y6, hb_a1c_y7, hb_a1c_y8, hb_a1c_y9, hb_a1c_y10,
                      na.rm = TRUE),

    hba1c_last = coalesce(hb_a1c_y10, hb_a1c_y9, hb_a1c_y8, hb_a1c_y7,
                          hb_a1c_y6,  hb_a1c_y5, hb_a1c_y4, hb_a1c_y3,
                          hb_a1c_y2,  hb_a1c_y1),

    hba1c_n_obs = rowSums(!is.na(
      across(c(hb_a1c_y1, hb_a1c_y2, hb_a1c_y3, hb_a1c_y4, hb_a1c_y5,
               hb_a1c_y6, hb_a1c_y7, hb_a1c_y8, hb_a1c_y9, hb_a1c_y10)))),

    hba1c_preop_cat = factor(case_when(
      hb_a1c_pre_op < 5.7                         ~ "Normal (<5.7)",
      hb_a1c_pre_op >= 5.7 & hb_a1c_pre_op < 6.5 ~ "Prediabetes (5.7-6.4)",
      hb_a1c_pre_op >= 6.5                        ~ "DM range (>=6.5)",
      TRUE ~ NA_character_),
      levels = c("Normal (<5.7)", "Prediabetes (5.7-6.4)", "DM range (>=6.5)"))
  )

# -----------------------------------------------------------------------------
# 7. SANITY CHECKS
# -----------------------------------------------------------------------------

cat("=== Sanity Checks ===\n")

neg_dm <- df %>% filter(!is.na(days_to_dm) & days_to_dm < 0)
cat("Patients with DM date BEFORE surgery:", nrow(neg_dm),
    "-- review 'days_to_dm' for data entry errors\n")
if (nrow(neg_dm) > 0) print(select(neg_dm, mayo_id, dos_pancreatectomy, dm_date, days_to_dm))

neg_exo <- df %>% filter(!is.na(days_to_exo_insuff) & days_to_exo_insuff < 0)
cat("Patients with exo insuff BEFORE surgery:", nrow(neg_exo),
    "-- review 'days_to_exo_insuff'\n")
if (nrow(neg_exo) > 0) print(select(neg_exo, mayo_id, dos_pancreatectomy,
                                     date_of_pancreatic_ins, days_to_exo_insuff))

cat("HbA1c pre-op out of range (<3 or >20):",
    sum(df$hb_a1c_pre_op < 3 | df$hb_a1c_pre_op > 20, na.rm = TRUE), "\n")
cat("Surgery date mismatches between datasets:",
    sum(df$date_mismatch_flag == TRUE, na.rm = TRUE), "\n")
cat("Duplicate mayo_ids:", sum(duplicated(df$mayo_id)), "\n\n")

# -----------------------------------------------------------------------------
# 8. MISSING VALUES SUMMARY
# -----------------------------------------------------------------------------

cat("=== Missing values per column (top 20) ===\n")
missing_summary <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  tidyr::pivot_longer(everything(),
                      names_to  = "variable",
                      values_to = "n_missing") %>%
  mutate(pct_missing = round(n_missing / nrow(df) * 100, 1)) %>%
  arrange(desc(n_missing))

print(missing_summary, n = 20)
cat("\n")

# -----------------------------------------------------------------------------
# 9. OUTCOME SUMMARY
# -----------------------------------------------------------------------------

cat("=== Final dataset ===\n")
cat("Rows:", nrow(df), "| Cols:", ncol(df), "\n\n")

cat("=== Endocrine/Exocrine outcome summary ===\n")
cat("New-onset DM:           ", sum(df$new_onset_dm == 1, na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$new_onset_dm == 1, na.rm = TRUE) * 100))
cat("Exocrine insufficiency: ", sum(df$exo_insufficiency == 1, na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$exo_insufficiency == 1, na.rm = TRUE) * 100))
cat("On insulin post-op:     ", sum(df$insulin == 1, na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$insulin == 1, na.rm = TRUE) * 100))
cat("On PERT post-op:        ", sum(df$pert == 1, na.rm = TRUE),
    sprintf("(%.1f%%)\n", mean(df$pert == 1, na.rm = TRUE) * 100))
cat("\nMedian days to DM (positive only):       ",
    median(df$days_to_dm[df$days_to_dm > 0], na.rm = TRUE), "\n")
cat("Median days to exo insuff (positive only):",
    median(df$days_to_exo_insuff[df$days_to_exo_insuff > 0], na.rm = TRUE), "\n")
cat("Median follow-up months:                  ",
    round(median(df$followup_months, na.rm = TRUE), 1), "\n\n")

# -----------------------------------------------------------------------------
# 10. EXPORT
# -----------------------------------------------------------------------------

write_csv(df, output_path, na = "")
saveRDS(df, rds_path)

cat("✓ CSV saved to:\n ", output_path, "\n")
cat("✓ RDS saved to:\n ", rds_path, "\n\n")
cat("TIP: Load the RDS in future scripts with:\n")
cat('  df <- readRDS("', rds_path, '")\n', sep = "")