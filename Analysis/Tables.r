# =============================================================================
# Table 1 & Table 2: Baseline Characteristics
# Table 1: Patient and Preoperative Characteristics
# Table 2: Operative, Pathologic, and Postoperative Outcomes
# Format: Minimalist high-impact journal style (3-line rule)
# Continuous: Median (IQR) | Categorical: N (%)
# Output: kable markdown in terminal + HTML export
# =============================================================================

library(dplyr)
library(knitr)
library(kableExtra)

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

df <- read.csv(
  "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/datamerged_endo_exo_PD.csv",
  stringsAsFactors = FALSE,
  na.strings        = c("", "NA")
)

# -----------------------------------------------------------------------------
# 2. OUTPUT DIRECTORY
# -----------------------------------------------------------------------------

out_dir <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/Endo_Exo_Results/Tables"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 3. RENAME MIXED-CASE COLUMNS & DERIVE VARIABLES
# -----------------------------------------------------------------------------

# Use setNames-safe approach: rename by matching exact column names
rename_col <- function(df, from, to) {
  idx <- which(names(df) == from)
  if (length(idx) == 1) names(df)[idx] <- to
  df
}

df <- rename_col(df, "DOS_pancreatectomy",     "dos_pancreatectomy")
df <- rename_col(df, "Steroids",               "steroids")
df <- rename_col(df, "PD_Reason",              "pd_reason")
df <- rename_col(df, "Family_Hx_DM",           "family_hx_dm")
df <- rename_col(df, "HbA1c_Pre.OP",           "hb_a1c_pre_op")
df <- rename_col(df, "eAG_Pre.OP",             "eag_pre_op")
df <- rename_col(df, "HbA1c_Y1",               "hb_a1c_y1")
df <- rename_col(df, "HbA1c_Y2",               "hb_a1c_y2")
df <- rename_col(df, "HbA1c_Y3",               "hb_a1c_y3")
df <- rename_col(df, "HbA1c_Y4",               "hb_a1c_y4")
df <- rename_col(df, "HbA1c_Y5",               "hb_a1c_y5")
df <- rename_col(df, "HbA1c_Y6",               "hb_a1c_y6")
df <- rename_col(df, "HbA1c_Y7",               "hb_a1c_y7")
df <- rename_col(df, "HbA1c_Y8",               "hb_a1c_y8")
df <- rename_col(df, "HbA1c_Y9",               "hb_a1c_y9")
df <- rename_col(df, "HbA1c_Y10",              "hb_a1c_y10")
df <- rename_col(df, "new_onset_DM",            "new_onset_dm")
df <- rename_col(df, "DM_date",                "dm_date")
df <- rename_col(df, "days_to_DM",             "days_to_dm")
df <- rename_col(df, "Insulin",                "insulin")
df <- rename_col(df, "DM_Medication",          "dm_medication")
df <- rename_col(df, "DM_Medication_start_day","dm_medication_start_day")
df <- rename_col(df, "Date_of_Pancreatic_Ins", "date_of_pancreatic_ins")
df <- rename_col(df, "Abd_Pain",               "abd_pain")
df <- rename_col(df, "Diarrhea",               "diarrhea")
df <- rename_col(df, "PERT",                   "pert")
df <- rename_col(df, "PERT.enddate",           "pert_enddate")
df <- rename_col(df, "Cholestyramine",         "cholestyramine")
df <- rename_col(df, "Post.op.MR",             "post_op_mr")
df <- rename_col(df, "Post.op.CT",             "post_op_ct")

# Helper: Excel serial or ISO date string -> Date
excel_or_date <- function(x) {
  x      <- as.character(x)
  num    <- suppressWarnings(as.numeric(x))
  result <- as.Date(rep(NA, length(x)))
  # Excel serial numbers (numeric)
  result[!is.na(num)] <- as.Date(num[!is.na(num)], origin = "1899-12-30")
  # ISO date strings "YYYY-MM-DD"
  iso_idx <- is.na(num) & !is.na(x) & nchar(x) == 10
  result[iso_idx] <- as.Date(x[iso_idx], format = "%Y-%m-%d")
  return(result)
}

# Parse dates — CSV stores as ISO "YYYY-MM-DD" strings
to_date <- function(x) as.Date(as.character(x), format = "%Y-%m-%d")

df$dos_pancreatectomy     <- to_date(df$dos_pancreatectomy)
df$dob                    <- to_date(df$dob)
df$vital_fu_dt            <- to_date(df$vital_fu_dt)
df$dm_date                <- to_date(df$dm_date)

# date_of_pancreatic_ins may be Excel serial or ISO string
df$date_of_pancreatic_ins <- excel_or_date(df$date_of_pancreatic_ins)

# Derived continuous variables
df$age_at_surgery  <- as.numeric(difftime(df$dos_pancreatectomy, df$dob,
                                           units = "days")) / 365.25
df$followup_months <- as.numeric(difftime(df$vital_fu_dt, df$dos_pancreatectomy,
                                           units = "days")) / 30.44
df$days_to_dm         <- as.numeric(difftime(df$dm_date,
                                              df$dos_pancreatectomy, units = "days"))
df$days_to_exo_insuff <- as.numeric(difftime(df$date_of_pancreatic_ins,
                                              df$dos_pancreatectomy, units = "days"))

# Gland texture recode
df$gland_texture_cat <- dplyr::case_when(
  df$gland_texture >= 1 & df$gland_texture <= 5  ~ "Soft (1–5)",
  df$gland_texture >= 6 & df$gland_texture <= 10 ~ "Hard (6–10)",
  TRUE ~ NA_character_
)

# HbA1c pre-op category
df$hba1c_preop_cat <- dplyr::case_when(
  df$hb_a1c_pre_op < 5.7                              ~ "Normal (<5.7%)",
  df$hb_a1c_pre_op >= 5.7 & df$hb_a1c_pre_op < 6.5   ~ "Prediabetes (5.7–6.4%)",
  df$hb_a1c_pre_op >= 6.5                             ~ "Diabetic range (≥6.5%)",
  TRUE ~ NA_character_
)

total_n <- nrow(df)

# -----------------------------------------------------------------------------
# 4. HELPER FUNCTIONS
# -----------------------------------------------------------------------------

med_iqr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("\u2014")
  q <- quantile(x, probs = c(0.25, 0.5, 0.75))
  sprintf("%.1f (%.1f\u2013%.1f)", q[2], q[1], q[3])
}

n_pct <- function(x, level) {
  total <- sum(!is.na(x))
  n     <- sum(x == level, na.rm = TRUE)
  if (total == 0) return("\u2014")
  sprintf("%d (%.1f%%)", n, n / total * 100)
}

n_valid <- function(x) sum(!is.na(x))

# -----------------------------------------------------------------------------
# 5. ROW BUILDER FUNCTIONS
# Column order: Characteristic | N | Value
# -----------------------------------------------------------------------------

rows <- list()

add_header <- function(label) {
  rows[[length(rows) + 1]] <<- data.frame(
    Variable  = label,
    N         = "",
    Statistic = "",
    stringsAsFactors = FALSE
  )
}

add_cont <- function(label, var) {
  rows[[length(rows) + 1]] <<- data.frame(
    Variable  = paste0("    ", label),
    N         = as.character(n_valid(df[[var]])),
    Statistic = med_iqr(df[[var]]),
    stringsAsFactors = FALSE
  )
}

add_cat <- function(display_label, var, level) {
  rows[[length(rows) + 1]] <<- data.frame(
    Variable  = paste0("    ", display_label),
    N         = as.character(n_valid(df[[var]])),
    Statistic = n_pct(df[[var]], level),
    stringsAsFactors = FALSE
  )
}

add_cat_all <- function(label, var) {
  lvls <- sort(unique(as.character(df[[var]][!is.na(df[[var]])])))
  rows[[length(rows) + 1]] <<- data.frame(
    Variable  = paste0("    ", label),
    N         = as.character(n_valid(df[[var]])),
    Statistic = "",
    stringsAsFactors = FALSE
  )
  for (l in lvls) {
    rows[[length(rows) + 1]] <<- data.frame(
      Variable  = paste0("        ", l),
      N         = "",
      Statistic = n_pct(df[[var]], l),
      stringsAsFactors = FALSE
    )
  }
}

# =============================================================================
# TABLE 1 — Patient and Preoperative Characteristics
# =============================================================================

rows <- list()

# ---- Demographics ------------------------------------------------------------
add_header("Demographics")
add_cont  ("Age at surgery, years",              "age_at_surgery")
add_cat   ("Female sex",                         "sex", "F")
add_cat_all("Race/ethnicity",                    "race")
add_cont  ("Body mass index, kg/m\u00b2",        "bmi")

# ---- Pre-operative Comorbidities ---------------------------------------------
add_header("Pre-operative Comorbidities")
add_cat   ("Hypertension",                       "hypertension",            1)
add_cat   ("Hyperlipidemia",                     "high_cholesterol_tg",     1)
add_cat   ("Hepatic fibrosis or cirrhosis",      "hep_fib_cirr",            1)
add_cat   ("History of significant alcohol use", "hx_significant_etoh_use", 1)
add_cat_all("Tobacco use",                       "tobacco")
add_cat_all("History of pancreatitis",           "pancreatitis")
add_cat   ("Proton pump inhibitor use",          "ppi",                     1)
add_cat   ("Chronic steroid use",                "steroids",                1)
add_cat   ("Prior gastrointestinal malignancy",  "prior_gi_malig",          1)
add_cat   ("Other prior malignancy",             "other_malig",             1)
add_cat   ("Family history of diabetes mellitus","family_hx_dm",            1)

# ---- Pre-operative Glycemic Status -------------------------------------------
add_header("Pre-operative Glycemic Status")
add_cont  ("HbA1c, %",                           "hb_a1c_pre_op")
add_cat_all("HbA1c category",                    "hba1c_preop_cat")

# ---- Pre-operative Workup ----------------------------------------------------
add_header("Pre-operative Diagnostic Workup")
add_cat   ("Endoscopic ultrasound (EUS)",        "eus",        1)
add_cat   ("Fine needle aspiration (FNA)",       "fna",        1)
add_cat   ("Computed tomography (CT)",           "preop_ct",   1)
add_cat   ("Magnetic resonance imaging (MRI)",   "preop_mri",  1)
add_cat   ("Positron emission tomography (PET)", "pet",        1)
add_cont  ("Index lesion size, cm",              "lesion_max_cm")

# ---- Indication for Surgery --------------------------------------------------
add_header("Indication for Pancreaticoduodenectomy")
add_cat_all("Primary indication",                "pd_reason")

tbl1 <- bind_rows(rows)
colnames(tbl1) <- c("Characteristic", "No.", "Median (IQR) or No. (%)")
header_rows1   <- which(tbl1[["Median (IQR) or No. (%)"]] == "" &
                          tbl1[["No."]] == "")

# =============================================================================
# TABLE 2 — Operative, Pathologic, and Postoperative Outcomes
# =============================================================================

rows <- list()

# ---- Operative Details -------------------------------------------------------
add_header("Operative Characteristics")
add_cat_all("Resection type",                    "resection_type")
add_cat_all("Operative approach",                "laparoscopic")
add_cat_all("ASA physical status classification","asa")
add_cont  ("Operative time, minutes",            "op_time")
add_cont  ("Estimated blood loss, mL",           "ebl")
add_cat   ("Venous resection and reconstruction","vein_resxn",        1)
add_cat   ("Concomitant splenectomy",            "splenectomy",       1)
add_cat   ("Concomitant organ resection",        "comcomitant_resxn", 1)
add_cat_all("Pancreatic gland texture",          "gland_texture_cat")
add_cont  ("Main pancreatic duct diameter, mm",  "panc_duct_mm")
add_cont  ("Common bile duct diameter, mm",      "cbd_mm")
add_cat   ("Intraoperative drain placement",     "drain_placement",   1)
add_cat   ("Enteral feeding tube placement",     "feeding_tube",      1)
add_cat   ("Total parenteral nutrition",         "tpn",               1)

# ---- Postoperative Course ----------------------------------------------------
add_header("Postoperative Course")
add_cont  ("ICU length of stay, days",           "icu_days")
add_cont  ("Hospital length of stay, days",      "los")
add_cat   ("Unplanned readmission within 90 days","readmit",           1)
add_cat   ("Reoperation",                        "reop",               1)

# ---- Postoperative Complications ---------------------------------------------
add_header("Postoperative Complications")
add_cat   ("Clinically relevant POPF",           "popf",       1)
add_cat_all("POPF grade (ISGPS)",                "popf_grade")
add_cat   ("Delayed gastric emptying",           "dge",        1)
add_cat_all("DGE grade (ISGPS)",                 "dge_grade")
add_cat   ("Post-pancreatectomy hemorrhage",     "pph",        1)
add_cat_all("PPH grade (ISGPS)",                 "pph_grade")
add_cat   ("Surgical site infection",            "wound_inf",  1)
add_cat   ("Intra-abdominal abscess",            "abd_abscess",1)
add_cat_all("Clavien-Dindo severity grade",      "cd_grade")

# ---- Pathology ---------------------------------------------------------------
add_header("Pathologic Findings")
add_cat_all("Primary pathologic diagnosis",      "path_dx1_sub")
add_cont  ("Tumor size, cm",                     "tumor_max")
add_cat_all("Surgical margin status",            "margin")
add_cat   ("Lymphovascular invasion",            "lvi",        1)
add_cat   ("Perineural invasion",                "pni",        1)
add_cont  ("Positive lymph nodes, n",            "pos_ln_n")
add_cont  ("Total lymph nodes examined, n",      "total_ln_n")
add_cat_all("Pathologic T stage (AJCC)",         "pt_stage_1")
add_cat_all("Pathologic N stage (AJCC)",         "pn_stage_1")

# ---- Endocrine Outcomes ------------------------------------------------------
add_header("Endocrine Outcomes")
add_cat   ("New-onset diabetes mellitus",        "new_onset_dm",      1)
add_cont  ("Time to diabetes diagnosis, days",   "days_to_dm")
add_cat   ("Insulin therapy initiated",          "insulin",           1)

# ---- Exocrine Outcomes -------------------------------------------------------
add_header("Exocrine Outcomes")
add_cat   ("Pancreatic exocrine insufficiency",  "exo_insufficiency", 1)
add_cont  ("Time to exocrine insufficiency, days","days_to_exo_insuff")
add_cat   ("Pancreatic enzyme replacement (PERT)","pert",             1)
add_cat   ("Cholestyramine prescribed",          "cholestyramine",    1)
add_cat   ("Abdominal pain",                     "abd_pain",          1)
add_cat   ("diarrhea",                           "diarrhea",          1)

# ---- Follow-up ---------------------------------------------------------------
add_header("Follow-up")
add_cont  ("Follow-up duration, months",         "followup_months")
add_cat_all("Vital status at last follow-up",    "vital_status_fu")
add_cat   ("Disease recurrence",                 "recurr",            1)

tbl2 <- bind_rows(rows)
colnames(tbl2) <- c("Characteristic", "No.", "Median (IQR) or No. (%)")
header_rows2   <- which(tbl2[["Median (IQR) or No. (%)"]] == "" &
                          tbl2[["No."]] == "")

# =============================================================================
# 6. PRINT MARKDOWN TO TERMINAL
# =============================================================================

print_tbl <- function(tbl, title) {
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat(title, "\n")
  cat(strrep("=", 78), "\n")
  print(kable(tbl, format = "markdown", align = c("l","r","c")))
  cat("\nContinuous variables: Median (Q1\u2013Q3). Categorical variables: No. (%).\n\n")
}

print_tbl(tbl1, paste0("Table 1. Patient and Preoperative Characteristics (N = ", total_n, ")"))
print_tbl(tbl2, paste0("Table 2. Operative, Pathologic, and Postoperative Outcomes (N = ", total_n, ")"))

# =============================================================================
# 7. HTML BUILDER FUNCTION
# =============================================================================

build_html_table <- function(tbl, header_rows, table_number, title_text, caption_text) {

  last_row <- nrow(tbl)

  ht <- kable(tbl,
              format  = "html",
              align   = c("l","r","c"),
              escape  = FALSE,
              caption = paste0("<b>Table ", table_number, ".</b> ", caption_text)) %>%
    kable_styling(
      bootstrap_options = c("condensed"),
      full_width        = TRUE,
      font_size         = 12,
      position          = "center"
    ) %>%
    # Three-line rule: thick top + thick bottom on header
    row_spec(0,
             bold      = TRUE,
             color     = "#000000",
             extra_css = paste0(
               "border-top: 2px solid #000000 !important;",
               "border-bottom: 2px solid #000000 !important;",
               "background-color: #ffffff !important;"
             )) %>%
    # Thick bottom border on last data row
    row_spec(last_row,
             extra_css = "border-bottom: 2px solid #000000 !important;") %>%
    # No borders on all other body rows
    row_spec(seq_len(last_row - 1),
             extra_css = "border-top: none !important; border-bottom: none !important;") %>%
    # Section headers: bold, slight top spacing, no shading
    row_spec(header_rows,
             bold      = TRUE,
             extra_css = "background-color: #ffffff !important; padding-top: 10px !important;") %>%
    column_spec(1, width = "52%",
                extra_css = "border-left: none !important; border-right: none !important;") %>%
    column_spec(2, width = "8%",
                extra_css = paste0("text-align: right !important;",
                                   "border-left: none !important;",
                                   "border-right: none !important;",
                                   "color: #555555;")) %>%
    column_spec(3, width = "40%",
                extra_css = "text-align: center !important; border-left: none !important; border-right: none !important;") %>%
    footnote(
      general = paste0(
        "Data are presented as median (interquartile range) for continuous variables ",
        "and as number (percentage) for categorical variables. ",
        "BMI = body mass index; EUS = endoscopic ultrasound; FNA = fine needle aspiration; ",
        "EBL = estimated blood loss; CBD = common bile duct; TPN = total parenteral nutrition; ",
        "LVI = lymphovascular invasion; PNI = perineural invasion; ISGPS = International Study ",
        "Group on Pancreatic Surgery; POPF = postoperative pancreatic fistula; ",
        "DGE = delayed gastric emptying; PPH = post-pancreatectomy hemorrhage; ",
        "PERT = pancreatic enzyme replacement therapy; DM = diabetes mellitus; ",
        "AJCC = American Joint Committee on Cancer; IQR = interquartile range."
      ),
      general_title     = "Abbreviations: ",
      footnote_as_chunk = TRUE
    )

  # Wrap in standalone HTML page
  paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n",
    "  <meta charset='UTF-8'>\n",
    "  <title>Table ", table_number, " \u2013 ", title_text, "</title>\n",
    "  <style>\n",
    "    body   { font-family: Arial, sans-serif; margin: 60px 80px;",
    "             max-width: 820px; color: #111; background: #fff; }\n",
    "    caption { font-size: 13px; margin-bottom: 6px;",
    "              text-align: left; color: #111; caption-side: top; }\n",
    "    table  { border-collapse: collapse; width: 100%; }\n",
    "    td, th { padding: 4px 8px;",
    "             border-left: none !important;",
    "             border-right: none !important; }\n",
    "    .footnote-text { font-size: 11px; color: #444; margin-top: 6px; }\n",
    "  </style>\n",
    "</head>\n<body>\n",
    ht,
    "\n</body>\n</html>"
  )
}

# =============================================================================
# 8. EXPORT HTML FILES
# =============================================================================

html1 <- build_html_table(
  tbl          = tbl1,
  header_rows  = header_rows1,
  table_number = 1,
  title_text   = "Patient and Preoperative Characteristics",
  caption_text = paste0(
    "Patient and Preoperative Characteristics of ", total_n,
    " Patients Undergoing Pancreaticoduodenectomy"
  )
)

html2 <- build_html_table(
  tbl          = tbl2,
  header_rows  = header_rows2,
  table_number = 2,
  title_text   = "Operative, Pathologic, and Postoperative Outcomes",
  caption_text = paste0(
    "Operative, Pathologic, and Postoperative Outcomes of ", total_n,
    " Patients Undergoing Pancreaticoduodenectomy"
  )
)

path1 <- file.path(out_dir, "Table1_Patient_Preoperative_Characteristics.html")
path2 <- file.path(out_dir, "Table2_Operative_Pathologic_Outcomes.html")

writeLines(html1, path1)
writeLines(html2, path2)

cat("\u2713 Table 1 exported to:\n  ", path1, "\n")
cat("\u2713 Table 2 exported to:\n  ", path2, "\n\n")