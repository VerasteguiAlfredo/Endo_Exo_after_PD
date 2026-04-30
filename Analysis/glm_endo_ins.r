# =============================================================================
# Table 4A & 4B: Logistic Regression — Risk Factors for New-Onset DM
# 4A: Univariable | 4B: Multivariable (Model A: full cohort, Model B: HbA1c subcohort)
# =============================================================================

library(dplyr)
library(knitr)
library(kableExtra)
library(logistf)

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

df <- readRDS("C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/datamerged_endo_exo_PD_clean.rds")

out_dir <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/Endo_Exo_Results/Tables"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. PREPARE ANALYTIC VARIABLES
# -----------------------------------------------------------------------------

df <- df %>%
  mutate(

    # Outcome
    dm_outcome = as.integer(new_onset_dm == 1),

    # Gland texture binary
    soft_gland = case_when(
      as.numeric(as.character(gland_texture)) >= 1 &
        as.numeric(as.character(gland_texture)) <= 5  ~ 1L,
      as.numeric(as.character(gland_texture)) >= 6 &
        as.numeric(as.character(gland_texture)) <= 10 ~ 0L,
      TRUE ~ NA_integer_
    ),

    # HbA1c category — Normal as reference
    hba1c_preop_cat = factor(case_when(
        hb_a1c_pre_op < 5.7                         ~ "Normal (<5.7)",
        hb_a1c_pre_op >= 5.7 & hb_a1c_pre_op < 6.5 ~ "Prediabetes (5.7-6.4)",
        TRUE ~ NA_character_),
        levels = c("Normal (<5.7)", "Prediabetes (5.7-6.4)")),

    # BMI categories — Normal as reference
    bmi_cat = case_when(
      bmi < 18.5              ~ "Underweight (<18.5)",
      bmi >= 18.5 & bmi < 25 ~ "Normal (18.5-24.9)",
      bmi >= 25   & bmi < 30 ~ "Overweight (25-29.9)",
      bmi >= 30               ~ "Obese (>=30)",
      TRUE ~ NA_character_
    ),
    bmi_cat = factor(bmi_cat,
                     levels = c("Normal (18.5-24.9)",
                                "Underweight (<18.5)",
                                "Overweight (25-29.9)",
                                "Obese (>=30)")),

    # Pancreatitis history binary
    hx_pancreatitis = case_when(
      pancreatitis %in% c("acute", "chronic") ~ 1L,
      pancreatitis == "none"                  ~ 0L,
      TRUE ~ NA_integer_
    ),

    # NAT (neoadjuvant therapy)
    nat_binary = as.integer(!is.na(nat) & nat == 1),

    # Resection type binaries
    distal_resection = as.integer(resection_type == "Distal"),
    total_resection  = as.integer(resection_type %in% c("PP Total",
                                                         "Standard Total")),

    # POPF
    popf_any = as.integer(popf == 1),
    cr_popf  = as.integer(cr_popf == 1),

    # Margin
    margin_r0 = as.integer(margin == "R0"),

    # Tobacco binary
    tobacco_ever = case_when(
      tobacco == "never"                ~ 0L,
      tobacco %in% c("past", "current") ~ 1L,
      TRUE ~ NA_integer_
    ),

    # Adenocarcinoma
    adenocarcinoma = as.integer(
      path_dx1_sub == "Adenocarcinoma (Panc/CBD/Ampulla/Duodenum)"
    ),
    # BMI categories — Normal as reference
    bmi_cat = case_when(
      bmi < 18.5              ~ "Underweight (<18.5)",
      bmi >= 18.5 & bmi < 25 ~ "Normal (18.5-24.9)",
      bmi >= 25   & bmi < 30 ~ "Overweight (25-29.9)",
      bmi >= 30               ~ "Obese (>=30)",
      TRUE ~ NA_character_
    ),
    bmi_cat = factor(bmi_cat,
                     levels = c("Normal (18.5-24.9)",
                                "Underweight (<18.5)",
                                "Overweight (25-29.9)",
                                "Obese (>=30)"))
  )


cat("=== BMI categories ===\n")
print(table(df$bmi_cat, useNA = "ifany"))

n_events <- sum(df$dm_outcome == 1, na.rm = TRUE)
n_total  <- sum(!is.na(df$dm_outcome))
cat("\nOutcome: New-onset DM\n")
cat("Events:", n_events, "/", n_total,
    sprintf("(%.1f%%)\n\n", n_events / n_total * 100))

# -----------------------------------------------------------------------------
# 3. HELPER: Univariable logistic regression row builder
# -----------------------------------------------------------------------------

glm_row <- function(label, formula_str, data, ref_label = NULL) {

  formula <- as.formula(formula_str)
  fit <- tryCatch(
    glm(formula, data = data, family = binomial()),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(data.frame(Variable = label, N = NA_character_,
                      OR = "\u2014", CI = "\u2014", P = "\u2014",
                      stringsAsFactors = FALSE))
  }

  n_used <- nobs(fit)
  coefs  <- coef(summary(fit))
  ci     <- tryCatch(suppressMessages(confint(fit)), error = function(e) NULL)

  out <- list()

  if (!is.null(ref_label)) {
    out[[1]] <- data.frame(Variable = label, N = as.character(n_used),
                           OR = "", CI = "", P = "",
                           stringsAsFactors = FALSE)
    out[[2]] <- data.frame(Variable = paste0("    ", ref_label, " (reference)"),
                           N = "", OR = "1.00", CI = "\u2014", P = "\u2014",
                           stringsAsFactors = FALSE)
  }

  for (i in seq_len(nrow(coefs))) {
    nm <- rownames(coefs)[i]
    if (nm == "(Intercept)") next

    or_val <- exp(coefs[i, "Estimate"])
    p_val  <- coefs[i, "Pr(>|z|)"]

    ci_str <- if (!is.null(ci) && nm %in% rownames(ci))
      sprintf("%.2f\u2013%.2f", exp(ci[nm, 1]), exp(ci[nm, 2]))
    else "\u2014"

    p_str <- if (p_val < 0.001) "<0.001" else sprintf("%.3f", p_val)

    if (!is.null(ref_label)) {
      clean_nm <- sub(paste0("^", deparse(formula[[3]])), "", nm)
      if (nchar(trimws(clean_nm)) == 0) clean_nm <- nm
      display <- paste0("    ", clean_nm)
    } else {
      display <- label
    }

    new_row <- data.frame(
      Variable = display,
      N        = "",
      OR       = sprintf("%.2f", or_val),
      CI       = ci_str,
      P        = p_str,
      stringsAsFactors = FALSE
    )
    out <- c(out, list(new_row))
  }

  if (is.null(ref_label) && length(out) > 0) {
    out[[1]]$N <- as.character(n_used)
  }

  bind_rows(out)
}

# -----------------------------------------------------------------------------
# 4. TABLE 4A — UNIVARIABLE ANALYSIS
# -----------------------------------------------------------------------------

rows_4a <- list()

add_section <- function(label) {
  rows_4a[[length(rows_4a) + 1]] <<- data.frame(
    Variable = label, N = "", OR = "", CI = "", P = "",
    stringsAsFactors = FALSE
  )
}

# Demographics
add_section("Demographics")
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Age at surgery (per year)", "dm_outcome ~ age_at_surgery", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Female sex", "dm_outcome ~ sex", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "BMI (per unit, continuous)", "dm_outcome ~ bmi", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "BMI category", "dm_outcome ~ bmi_cat", df,
  ref_label = "Normal (18.5-24.9)")

# Pre-op glycemic status
add_section("Pre-operative Glycemic Status")
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Pre-op HbA1c (continuous)", "dm_outcome ~ hb_a1c_pre_op", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "HbA1c category", "dm_outcome ~ hba1c_preop_cat", df,
  ref_label = "Normal (<5.7%)")

# Comorbidities
add_section("Pre-operative Comorbidities")
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Hypertension", "dm_outcome ~ hypertension", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Hyperlipidemia", "dm_outcome ~ high_cholesterol_tg", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Family history of DM", "dm_outcome ~ family_hx_dm", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "History of pancreatitis", "dm_outcome ~ hx_pancreatitis", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Significant alcohol use", "dm_outcome ~ hx_significant_etoh_use", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Tobacco use (ever)", "dm_outcome ~ tobacco_ever", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Pre-op exocrine insufficiency", "dm_outcome ~ preop_exo_insuff", df)

# Surgical
add_section("Surgical Characteristics")
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Distal pancreatectomy", "dm_outcome ~ distal_resection", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Total pancreatectomy", "dm_outcome ~ total_resection", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Soft gland texture (score 1-5)", "dm_outcome ~ soft_gland", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Main pancreatic duct diameter (per mm)", "dm_outcome ~ panc_duct_mm", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Vein resection", "dm_outcome ~ vein_resxn", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Estimated blood loss (per 100 mL)", "dm_outcome ~ I(ebl/100)", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Operative time (per 10 min)", "dm_outcome ~ I(op_time/10)", df)

# Post-op complications
add_section("Postoperative Complications")
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Any POPF", "dm_outcome ~ popf_any", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Clinically relevant POPF (Grade B/C)", "dm_outcome ~ cr_popf", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Delayed gastric emptying", "dm_outcome ~ dge", df)

# Pathology
add_section("Pathologic Characteristics")
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Adenocarcinoma diagnosis", "dm_outcome ~ adenocarcinoma", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Tumor size (per cm)", "dm_outcome ~ tumor_max", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "R0 resection margin", "dm_outcome ~ margin_r0", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Perineural invasion", "dm_outcome ~ pni", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Lymphovascular invasion", "dm_outcome ~ lvi", df)

# Oncologic treatment
add_section("Oncologic Treatment")
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Neoadjuvant therapy", "dm_outcome ~ nat_binary", df)
rows_4a[[length(rows_4a) + 1]] <- glm_row(
  "Adjuvant chemotherapy", "dm_outcome ~ systemic_chemo", df)

tbl4a <- bind_rows(rows_4a)
colnames(tbl4a) <- c("Variable", "N", "OR", "95% CI", "P-Value")
header_rows_4a  <- which(tbl4a[["OR"]] == "" & tbl4a[["N"]] == "")

# -----------------------------------------------------------------------------
# 5. TABLE 4B — MULTIVARIABLE ANALYSIS
# Model A: Full cohort (excludes hba1c, soft_gland, panc_duct_mm)
# Model B: HbA1c subcohort (includes glycemic status)
# Both use Firth penalized likelihood
# -----------------------------------------------------------------------------

# --- Model A: Full cohort ---
mv_formula_A <- paste0(
  "dm_outcome ~ ",
  "age_at_surgery + bmi_cat + family_hx_dm + ",
  "cr_popf + nat_binary + adenocarcinoma + ",
  "hx_pancreatitis + preop_exo_insuff + tobacco_ever"
)

fit_A <- logistf(as.formula(mv_formula_A), data = df,
                 firth = TRUE, pl = TRUE)

cat("=== Model A summary (Full cohort, Firth) ===\n")
cat("N =", fit_A$n, "\n")
print(summary(fit_A))

# --- Model B: HbA1c subcohort ---
# hba1c_preop_cat now two levels only: Normal (<5.7%) vs Prediabetes (5.7-6.4%)
# DM range (>=6.5%) excluded from cohort at cleaning stage (n=11, ADA DM criteria)
mv_formula_B <- paste0(
  "dm_outcome ~ ",
  "age_at_surgery + bmi_cat + hba1c_preop_cat + family_hx_dm + ",
  "cr_popf + nat_binary + adenocarcinoma + ",
  "hx_pancreatitis + preop_exo_insuff"
)

fit_B <- logistf(as.formula(mv_formula_B), data = df,
                 firth = TRUE, pl = TRUE)

cat("=== Model B summary (HbA1c subcohort, Firth) ===\n")
cat("N =", fit_B$n, "\n")
print(summary(fit_B))

# --- Variable labels ---
var_labels <- c(
  "age_at_surgery"                        = "Age at surgery (per year)",
  "bmi_catUnderweight (<18.5)"            = "    BMI: Underweight vs Normal",
  "bmi_catOverweight (25-29.9)"           = "    BMI: Overweight vs Normal",
  "bmi_catObese (>=30)"                   = "    BMI: Obese vs Normal",
  "hba1c_preop_catPrediabetes (5.7-6.4)"  = "    HbA1c: Prediabetes vs Normal (<5.7%)",
  # DM range (>=6.5%) removed — patients excluded from cohort
  "family_hx_dm"                          = "Family history of DM",
  "cr_popf"                               = "Clinically relevant POPF",
  "nat_binary"                            = "Neoadjuvant therapy",
  "adenocarcinoma"                        = "Adenocarcinoma diagnosis",
  "hx_pancreatitis"                       = "History of pancreatitis",
  "preop_exo_insuff"                      = "Pre-op exocrine insufficiency",
  "tobacco_ever"                          = "Tobacco use (ever)"
)

# --- Helper: extract logistf results into rows ---
extract_logistf <- function(fit, model_label) {

  coefs  <- fit$coefficients
  pvals  <- fit$prob
  ci_lo  <- fit$ci.lower
  ci_hi  <- fit$ci.upper
  n_used <- fit$n

  rows <- list()

  # Model header row
  rows[[1]] <- data.frame(
    Variable = model_label,
    N        = as.character(n_used),
    OR = "", CI = "", P = "",
    stringsAsFactors = FALSE
  )

  for (nm in names(coefs)) {
    if (nm == "(Intercept)") next

    or_val <- exp(coefs[nm])
    p_val  <- pvals[nm]
    ci_str <- sprintf("%.2f\u2013%.2f", exp(ci_lo[nm]), exp(ci_hi[nm]))
    p_str  <- if (p_val < 0.001) "<0.001" else sprintf("%.3f", p_val)
    label  <- ifelse(nm %in% names(var_labels), var_labels[nm], nm)

    rows[[length(rows) + 1]] <- data.frame(
      Variable = label,
      N        = "",
      OR       = sprintf("%.2f", or_val),
      CI       = ci_str,
      P        = p_str,
      stringsAsFactors = FALSE
    )
  }
  bind_rows(rows)
}

rows_A <- extract_logistf(fit_A, "Model A — Full cohort (no HbA1c)")
rows_B <- extract_logistf(fit_B, "Model B — HbA1c subcohort")

# Spacer row between models
spacer <- data.frame(Variable = "", N = "", OR = "", CI = "", P = "",
                     stringsAsFactors = FALSE)

tbl4b <- bind_rows(rows_A, spacer, rows_B)
colnames(tbl4b) <- c("Variable", "N", "OR", "95% CI", "P-Value")

header_rows_4b <- which(tbl4b[["OR"]] == "" & tbl4b[["N"]] != "" |
                         tbl4b[["OR"]] == "" & tbl4b[["N"]] == "" &
                         tbl4b[["Variable"]] != "")

# -----------------------------------------------------------------------------
# 5b. SIGNIFICANCE MARKERS
# -----------------------------------------------------------------------------

mark_p <- function(p_str, threshold) {
  if (p_str == "\u2014" || p_str == "") return(p_str)
  p_num <- suppressWarnings(as.numeric(p_str))
  if (p_str == "<0.001") return(paste0(p_str, " *"))
  if (!is.na(p_num) && p_num < threshold) return(paste0(p_str, " *"))
  return(p_str)
}

tbl4a[["P-Value"]] <- sapply(tbl4a[["P-Value"]], mark_p, threshold = 0.1)
tbl4b[["P-Value"]] <- sapply(tbl4b[["P-Value"]], mark_p, threshold = 0.05)

cat("Significance markers applied:\n")
cat("  Table 4A: * = p < 0.10\n")
cat("  Table 4B: * = p < 0.05\n\n")

# -----------------------------------------------------------------------------
# 6. PRINT TO TERMINAL
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 78), "\n", sep = "")
cat("Table 4A. Univariable Logistic Regression — New-Onset DM\n")
cat(strrep("=", 78), "\n")
print(kable(tbl4a, format = "markdown", align = c("l","r","c","c","c")))

cat("\n", strrep("=", 78), "\n", sep = "")
cat("Table 4B. Multivariable Logistic Regression — New-Onset DM\n")
cat(strrep("=", 78), "\n")
print(kable(tbl4b, format = "markdown", align = c("l","r","c","c","c")))

# -----------------------------------------------------------------------------
# 7. HTML BUILDER
# -----------------------------------------------------------------------------

build_html_reg <- function(tbl, header_rows, table_id, caption_text) {

  last_row <- nrow(tbl)

  ht <- kable(tbl,
              format   = "html",
              align    = c("l","r","c","c","c"),
              escape   = FALSE,
              caption  = caption_text) %>%
    kable_styling(
      bootstrap_options = c("condensed"),
      full_width        = TRUE,
      font_size         = 12,
      position          = "center"
    ) %>%
    row_spec(0, bold = TRUE,
             extra_css = paste0(
               "border-top: 2px solid #000000 !important;",
               "border-bottom: 2px solid #000000 !important;",
               "background-color: #ffffff !important;"
             )) %>%
    row_spec(last_row,
             extra_css = "border-bottom: 2px solid #000000 !important;") %>%
    row_spec(seq_len(last_row - 1),
             extra_css = "border-top: none !important;
                          border-bottom: none !important;") %>%
    row_spec(header_rows, bold = TRUE,
             extra_css = "background-color: #ffffff !important;
                          padding-top: 10px !important;") %>%
    column_spec(1, width = "45%",
                extra_css = "border-left: none !important;
                             border-right: none !important;") %>%
    column_spec(2, width = "8%",
                extra_css = "text-align: right !important; color: #555555;
                             border-left: none !important;
                             border-right: none !important;") %>%
    column_spec(3:5, width = "15%",
                extra_css = "text-align: center !important;
                             border-left: none !important;
                             border-right: none !important;") %>%
    footnote(
      general = paste0(
        "OR = odds ratio; CI = confidence interval. ",
        "Table 4A: Odds ratios estimated by standard binary logistic regression; ",
        "* denotes p < 0.10. ",
        "Table 4B: Odds ratios estimated by Firth penalized likelihood regression ",
        "to address sparse data; profile likelihood confidence intervals reported; ",
        "* denotes p < 0.05. ",
        "Model A was fitted in the full analytic cohort, excluding predictors with ",
        ">25% missing data (pre-operative HbA1c, pancreatic duct diameter, gland texture). ",
        "Model B was restricted to patients with available pre-operative HbA1c ",
        "(n = ", fit_B$n, "). ",
        "Patients with pre-operative HbA1c >=6.5% were excluded from the analytic cohort ",
        "(n = 11) as this value meets the American Diabetes Association diagnostic ",
        "threshold for diabetes mellitus irrespective of formal clinical diagnosis. ",
        "Resection type (distal, total pancreatectomy) was excluded from both multivariable ",
        "models due to complete separation; univariable estimates are reported in Table 4A. ",
        "Reference categories: BMI, Normal (18.5-24.9 kg/m²); ",
        "HbA1c, Normal (<5.7%). ",
        "HbA1c category in Model B includes Normal (<5.7%) and Prediabetes (5.7-6.4%) only; ",
        "the dysglycemic range category (>=6.5%) was removed following cohort exclusion."
      ),
      general_title     = "Abbreviations and Notes: ",
      footnote_as_chunk = TRUE
    )

  paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n",
    "  <meta charset='UTF-8'>\n",
    "  <title>", table_id, "</title>\n",
    "  <style>\n",
    "    body   { font-family: 'Times New Roman', Times, serif;",
    "             margin: 60px 80px; max-width: 820px;",
    "             color: #111; background: #fff; }\n",
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

# --- Exclude patients with pre-op HbA1c >= 6.5% (meets ADA DM criteria) ---
n_before <- nrow(df)
df <- df %>%
  filter(is.na(hb_a1c_pre_op) | hb_a1c_pre_op < 6.5)
cat("Patients removed (pre-op HbA1c >= 6.5%, meets ADA DM criteria):",
    n_before - nrow(df), "\n")
cat("Remaining patients:", nrow(df), "\n\n")

# -----------------------------------------------------------------------------
# 8. EXPORT
# -----------------------------------------------------------------------------

caption_4a <- paste0(
  "<b>Table 4A.</b> Univariable Logistic Regression for Predictors of ",
  "New-Onset Diabetes Mellitus Following Pancreatectomy ",
  "(Events: ", n_events, "/", n_total, " [",
  sprintf("%.1f%%", n_events / n_total * 100), "])"
)

caption_4b <- paste0(
  "<b>Table 4B.</b> Multivariable Logistic Regression for Predictors of ",
  "New-Onset Diabetes Mellitus Following Pancreatectomy ",
  "(Events: ", n_events, "/", n_total, " [",
  sprintf("%.1f%%", n_events / n_total * 100), "]). ",
  "Model A: full cohort (N = ", fit_A$n, "); ",
  "Model B: HbA1c subcohort (N = ", fit_B$n, ")."
)

html_4a <- build_html_reg(
  tbl          = tbl4a,
  header_rows  = header_rows_4a,
  table_id     = "Table 4A - Univariable Logistic Regression",
  caption_text = caption_4a
)

html_4b <- build_html_reg(
  tbl          = tbl4b,
  header_rows  = header_rows_4b,
  table_id     = "Table 4B - Multivariable Logistic Regression",
  caption_text = caption_4b
)

path_4a <- file.path(out_dir, "Table4A_Univariable_DM.html")
path_4b <- file.path(out_dir, "Table4B_Multivariable_DM.html")

writeLines(html_4a, path_4a)
writeLines(html_4b, path_4b)

cat("\u2713 Table 4A exported to:\n  ", path_4a, "\n")
cat("\u2713 Table 4B exported to:\n  ", path_4b, "\n\n")