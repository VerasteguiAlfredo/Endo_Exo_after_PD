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

data_path   <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/raw datasets/datamerged_endo_exo_PD.csv"
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

## Some data cleaning, Manual cleanup
excel_or_date <- function(x) {
  # --- Step 1: Manual corrections for known bad entries ---
  x <- case_when(
    x == "6/62022"  ~ "6/6/2022",
    x == "1/2/0214" ~ "1/2/2014",
    x == "Unknown"  ~ NA_character_,
    TRUE ~ x
  )
  
  # --- Step 2: Numeric Excel serials ---
  num <- suppressWarnings(as.numeric(x))
  result <- as.Date(rep(NA, length(x)))
  result[!is.na(num)] <- as.Date(num[!is.na(num)], origin = "1899-12-30")
  
  # --- Step 3: Character dates — try multiple formats including d/m/Y ---
  char_idx <- is.na(num) & !is.na(x)
  if (any(char_idx)) {
    result[char_idx] <- as.Date(
      suppressWarnings(
        lubridate::parse_date_time(
          x[char_idx],
          orders = c("Ymd", "mdY", "dmY", "Ymd HMS", "mdY HMS", "dmY HMS"),
          quiet  = TRUE
        )
      )
    )
  }
  return(result)
}

# -----------------------------------------------------------------------------
# 3. HELPER: convert Excel serial OR date string to Date
# -----------------------------------------------------------------------------

excel_or_date <- function(x) {
  num <- suppressWarnings(as.numeric(x))
  result <- as.Date(rep(NA, length(x)))
  
  # Excel serials
  result[!is.na(num)] <- as.Date(num[!is.na(num)], origin = "1899-12-30")
  
  # Character dates — try multiple formats
  char_idx <- is.na(num) & !is.na(x)
  if (any(char_idx)) {
    result[char_idx] <- as.Date(
      suppressWarnings(
        lubridate::parse_date_time(
          x[char_idx],
          orders = c("Ymd", "mdY", "dmY", "Ymd HMS", "mdY HMS"),
          quiet  = TRUE
        )
      )
    )
  }
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
             hb_a1c_pre_op, hb_a1c_y1, hb_a1c_y2, hb_a1c_y3,
             hb_a1c_y4, hb_a1c_y5, hb_a1c_y6, hb_a1c_y7,
             hb_a1c_y8, hb_a1c_y9, hb_a1c_y10,
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
# 6b. RECODE CATEGORICAL VARIABLES
# -----------------------------------------------------------------------------

df <- df %>%
  mutate(

    # --- Pathologic N stage (AJCC) ---
    pn_stage_1 = case_when(
      pn_stage_1 %in% c("0")          ~ "N0",
      pn_stage_1 %in% c("1")          ~ "N1",
      pn_stage_1 %in% c("2", "2=02")  ~ "N2",
      pn_stage_1 %in% c("8", "9")     ~ "NX",
      TRUE ~ NA_character_
    ),
    pn_stage_1 = factor(pn_stage_1, levels = c("N0", "N1", "N2", "NX")),

    # --- Pathologic T stage (AJCC) ---
    pt_stage_1 = case_when(
      pt_stage_1 == "0"   ~ "T0",
      pt_stage_1 == "Tis" ~ "Tis",
      pt_stage_1 == "T1"  ~ "T1",
      pt_stage_1 == "T2"  ~ "T2",
      pt_stage_1 == "T3"  ~ "T3",
      pt_stage_1 == "T4"  ~ "T4",
      TRUE ~ NA_character_
    ),
    pt_stage_1 = factor(pt_stage_1,
                        levels = c("T0", "Tis", "T1", "T2", "T3", "T4")),

    # --- Primary pathologic diagnosis (broad) ---
    path_dx1_sub = case_when(
      path_dx1_sub == "1" ~ "Adenocarcinoma (Panc/CBD/Ampulla/Duodenum)",
      path_dx1_sub == "2" ~ "Cystic disease (IPMN/MCN/SCN)",
      path_dx1_sub == "3" ~ "Pancreatitis",
      path_dx1_sub == "4" ~ "Miscellaneous periampullary",
      TRUE ~ NA_character_
    ),
    path_dx1_sub = factor(path_dx1_sub,
                          levels = c("Adenocarcinoma (Panc/CBD/Ampulla/Duodenum)",
                                     "Cystic disease (IPMN/MCN/SCN)",
                                     "Pancreatitis",
                                     "Miscellaneous periampullary")),

    # --- Primary pathologic diagnosis (specific) ---
    path_dx1_specific = case_when(
      # Malignant pancreatic
      path_dx1_specific == "1"  ~ "Pancreatic ductal adenocarcinoma",
      path_dx1_specific == "2"  ~ "Acinar cell carcinoma",
      path_dx1_specific == "3"  ~ "PNET",
      path_dx1_specific == "5"  ~ "Carcinoid",
      path_dx1_specific == "6"  ~ "Adenosquamous carcinoma",
      path_dx1_specific == "25" ~ "Adenosquamous carcinoma",
      # Other malignant
      path_dx1_specific == "4"  ~ "Cholangiocarcinoma",
      path_dx1_specific == "9"  ~ "GIST",
      path_dx1_specific == "16" ~ "Ampullary adenocarcinoma",
      path_dx1_specific == "24" ~ "Duodenal adenocarcinoma",
      # Cystic neoplasms
      path_dx1_specific == "10" ~ "Serous cystic neoplasm (SCN)",
      path_dx1_specific == "12" ~ "IPMN",
      path_dx1_specific == "13" ~ "Mucinous cystic neoplasm (MCN)",
      path_dx1_specific == "21" ~ "Solid pseudopapillary neoplasm (SPN)",
      # Benign / inflammatory
      path_dx1_specific == "17" ~ "Ampullary adenoma",
      path_dx1_specific == "19" ~ "Pseudocyst",
      path_dx1_specific == "20" ~ "Benign stricture",
      # Other
      path_dx1_specific == "97" ~ "Other benign",
      path_dx1_specific == "99" ~ "Other",
      TRUE ~ NA_character_
    ),
    path_dx1_specific = factor(path_dx1_specific,
                               levels = c(
                                 "Pancreatic ductal adenocarcinoma",
                                 "Acinar cell carcinoma",
                                 "PNET",
                                 "Carcinoid",
                                 "Adenosquamous carcinoma",
                                 "Cholangiocarcinoma",
                                 "Ampullary adenocarcinoma",
                                 "Duodenal adenocarcinoma",
                                 "GIST",
                                 "IPMN",
                                 "Mucinous cystic neoplasm (MCN)",
                                 "Serous cystic neoplasm (SCN)",
                                 "Solid pseudopapillary neoplasm (SPN)",
                                 "Ampullary adenoma",
                                 "Pseudocyst",
                                 "Benign stricture",
                                 "Other benign",
                                 "Other"
                               )),

    # --- POPF grade (ISGPS 2016) ---
    popf_grade = case_when(
      popf == 0          ~ NA_character_,
      popf_grade == "A"  ~ "Biochemical leak",
      popf_grade == "B"  ~ "Grade B",
      popf_grade == "C"  ~ "Grade C",
      TRUE ~ NA_character_
    ),
    popf_grade = factor(popf_grade,
                        levels = c("Biochemical leak", "Grade B", "Grade C")),

    # --- Clinically relevant POPF (CR-POPF): Grade B or C ---
    cr_popf = case_when(
      popf_grade %in% c("Grade B", "Grade C") ~ 1L,
      popf_grade == "Biochemical leak"         ~ 0L,
      popf == 0                                ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# -----------------------------------------------------------------------------
# 6c. DROP MOSTLY-EMPTY COLUMNS (>97% missing)
# -----------------------------------------------------------------------------

df <- df %>%
  select(-c(
    # NAT 3rd line chemo (100% missing)
    nat3_chemo_start_dt,
    nat3_chemo_end_dt,
    nat3_chemo_details,

    # AT 3rd line chemo (99.9% missing)
    at3_chemo_start_dt,
    at3_chemo_end_dt,
    at3_chemo_details,

    # AT 2nd line chemo (98.3% missing)
    at2_chemo_start_dt,
    at2_chemo_end_dt,
    at2_chemo_details,

    # NAT 2nd line chemo (97.6% missing)
    nat2_chemo_start_dt,
    nat2_chemo_end_dt,
    nat2_chemo_details,

    # Recurrence site (97.3% missing)
    recurr_perit_oment
  ))

cat("Columns after dropping mostly-empty variables:", ncol(df), "\n")

# -----------------------------------------------------------------------------
# Quick checks
# -----------------------------------------------------------------------------

cat("=== N stage ===\n");      print(table(df$pn_stage_1,      useNA = "ifany"))
cat("=== T stage ===\n");      print(table(df$pt_stage_1,      useNA = "ifany"))
cat("=== Path dx broad ===\n");print(table(df$path_dx1_sub,    useNA = "ifany"))
cat("=== Path dx specific ===\n"); print(table(df$path_dx1_specific, useNA = "ifany"))
cat("=== POPF grade ===\n");   print(table(df$popf_grade,      useNA = "ifany"))
cat("CR-POPF (Grade B+C):", sum(df$cr_popf == 1, na.rm = TRUE), "\n")

# -----------------------------------------------------------------------------
# 7. SANITY CHECKS & EXCLUSIONS
# -----------------------------------------------------------------------------

cat("=== Sanity Checks ===\n")

# --- DM date before surgery: data entry error → exclude ---
neg_dm <- df %>% filter(!is.na(days_to_dm) & days_to_dm < 0)
cat("Patients with DM date BEFORE surgery:", nrow(neg_dm),
    "-- removing from dataset\n")
if (nrow(neg_dm) > 0) print(select(neg_dm, mayo_id, dos_pancreatectomy, dm_date, days_to_dm))

n_before <- nrow(df)
df <- df %>% filter(is.na(days_to_dm) | days_to_dm >= 0)
cat("Patients removed:", n_before - nrow(df), "| Remaining:", nrow(df), "\n\n")

# --- Exo insufficiency before surgery: flag as pre-op, do not remove ---
neg_exo <- df %>% filter(!is.na(days_to_exo_insuff) & days_to_exo_insuff < 0)
cat("Patients with exo insuff BEFORE surgery:", nrow(neg_exo),
    "-- flagged as pre-op exo insufficiency\n")
if (nrow(neg_exo) > 0) print(select(neg_exo, mayo_id, dos_pancreatectomy,
                                     date_of_pancreatic_ins, days_to_exo_insuff))

df <- df %>%
  mutate(
    preop_exo_insuff = case_when(
      !is.na(days_to_exo_insuff) & days_to_exo_insuff < 0 ~ 1L,
      TRUE ~ 0L
    )
  )
cat("Patients flagged with pre-op exo insufficiency:",
    sum(df$preop_exo_insuff == 1, na.rm = TRUE), "\n\n")

# --- Exclude patients with surgery before 2010-01-01 ---
cat("=== Surgery Date Range (before exclusion) ===\n")
cat("Earliest surgery:", format(min(df$dos_pancreatectomy, na.rm = TRUE)), "\n")
cat("Latest surgery:  ", format(max(df$dos_pancreatectomy, na.rm = TRUE)), "\n")
cat("Patients before 2010-01-01:",
    sum(df$dos_pancreatectomy < as.Date("2010-01-01"), na.rm = TRUE), "\n\n")

cat("=== Pre-2010 patients by year ===\n")
df %>%
  filter(dos_pancreatectomy < as.Date("2010-01-01")) %>%
  mutate(surgery_year = year(dos_pancreatectomy)) %>%
  count(surgery_year) %>%
  print(n = 20)

cat("\n=== Pre-2010 patients by diagnosis ===\n")
df %>%
  filter(dos_pancreatectomy < as.Date("2010-01-01")) %>%
  count(path_dx1_sub) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

cat("\n=== Pre-2010 patients by resection type ===\n")
df %>%
  filter(dos_pancreatectomy < as.Date("2010-01-01")) %>%
  count(resection_type) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

n_before <- nrow(df)
df <- df %>% filter(dos_pancreatectomy >= as.Date("2010-01-01"))
cat("\nPatients removed (surgery before 2010-01-01):", n_before - nrow(df), "\n")
cat("Remaining patients:", nrow(df), "\n")
cat("Final surgery date range:",
    format(min(df$dos_pancreatectomy, na.rm = TRUE)), "to",
    format(max(df$dos_pancreatectomy, na.rm = TRUE)), "\n\n")

# --- Other checks ---
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