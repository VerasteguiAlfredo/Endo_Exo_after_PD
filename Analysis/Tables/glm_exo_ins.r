# =============================================================================
# Table 5A & 5B: Logistic Regression — Risk Factors for Exocrine Insufficiency
# 5A: Univariable | 5B: Multivariable (Model A: full cohort, Model B: HbA1c subcohort)
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

    # Outcome — post-op exocrine insufficiency
    # Exclude patients with pre-op exo insufficiency from outcome
    exo_outcome = case_when(
      preop_exo_insuff == 1             ~ NA_integer_,  # pre-existing, exclude
      exo_insufficiency == 1            ~ 1L,
      exo_insufficiency == 0            ~ 0L,
      TRUE                              ~ NA_integer_
    ),

    # Gland texture binary
    soft_gland = case_when(
      as.numeric(as.character(gland_texture)) >= 1 &
        as.numeric(as.character(gland_texture)) <= 5  ~ 1L,
      as.numeric(as.character(gland_texture)) >= 6 &
        as.numeric(as.character(gland_texture)) <= 10 ~ 0L,
      TRUE ~ NA_integer_
    ),

    # HbA1c category — Normal as reference (DM range excluded)
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

    # NAT
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
      tobacco == "never"                 ~ 0L,
      tobacco %in% c("past", "current") ~ 1L,
      TRUE ~ NA_integer_
    ),

    # Adenocarcinoma
    adenocarcinoma = as.integer(
      path_dx1_sub == "Adenocarcinoma (Panc/CBD/Ampulla/Duodenum)"
    ),

    # New-onset DM as predictor for exo model
    dm_predictor = as.integer(new_onset_dm == 1)
  )

# Check outcome
n_events <- sum(df$exo_outcome == 1, na.rm = TRUE)
n_total  <- sum(!is.na(df$exo_outcome))
cat("Outcome: Post-op Exocrine Insufficiency\n")
cat("Events:", n_events, "/", n_total,
    sprintf("(%.1f%%)\n", n_events / n_total * 100))
cat("Excluded (pre-op exo insuff):",
    sum(df$preop_exo_insuff == 1, na.rm = TRUE), "\n\n")

cat("=== BMI categories ===\n")
print(table(df$bmi_cat, useNA = "ifany"))
cat("\n")

# -----------------------------------------------------------------------------
# 3. HELPER: Univariable logistic regression row builder
# (same as Table 4 script)
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
# 4. TABLE 5A — UNIVARIABLE ANALYSIS
# -----------------------------------------------------------------------------

rows_5a <- list()

add_section <- function(label) {
  rows_5a[[length(rows_5a) + 1]] <<- data.frame(
    Variable = label, N = "", OR = "", CI = "", P = "",
    stringsAsFactors = FALSE
  )
}

# Demographics
add_section("Demographics")
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Age at surgery (per year)", "exo_outcome ~ age_at_surgery", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Female sex", "exo_outcome ~ sex", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "BMI (per unit, continuous)", "exo_outcome ~ bmi", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "BMI category", "exo_outcome ~ bmi_cat", df,
  ref_label = "Normal (18.5-24.9)")

# Pre-op glycemic status
add_section("Pre-operative Glycemic Status")
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Pre-op HbA1c (continuous)", "exo_outcome ~ hb_a1c_pre_op", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "HbA1c category", "exo_outcome ~ hba1c_preop_cat", df,
  ref_label = "Normal (<5.7%)")

# Comorbidities
add_section("Pre-operative Comorbidities")
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Hypertension", "exo_outcome ~ hypertension", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Hyperlipidemia", "exo_outcome ~ high_cholesterol_tg", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Family history of DM", "exo_outcome ~ family_hx_dm", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "History of pancreatitis", "exo_outcome ~ hx_pancreatitis", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Significant alcohol use", "exo_outcome ~ hx_significant_etoh_use", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Tobacco use (ever)", "exo_outcome ~ tobacco_ever", df)

# Surgical
add_section("Surgical Characteristics")
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Distal pancreatectomy", "exo_outcome ~ distal_resection", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Total pancreatectomy", "exo_outcome ~ total_resection", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Soft gland texture (score 1-5)", "exo_outcome ~ soft_gland", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Main pancreatic duct diameter (per mm)", "exo_outcome ~ panc_duct_mm", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Vein resection", "exo_outcome ~ vein_resxn", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Estimated blood loss (per 100 mL)", "exo_outcome ~ I(ebl/100)", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Operative time (per 10 min)", "exo_outcome ~ I(op_time/10)", df)

# Post-op complications
add_section("Postoperative Complications")
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Any POPF", "exo_outcome ~ popf_any", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Clinically relevant POPF (Grade B/C)", "exo_outcome ~ cr_popf", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Delayed gastric emptying", "exo_outcome ~ dge", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "New-onset diabetes mellitus", "exo_outcome ~ dm_predictor", df)

# Pathology
add_section("Pathologic Characteristics")
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Adenocarcinoma diagnosis", "exo_outcome ~ adenocarcinoma", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Tumor size (per cm)", "exo_outcome ~ tumor_max", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "R0 resection margin", "exo_outcome ~ margin_r0", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Perineural invasion", "exo_outcome ~ pni", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Lymphovascular invasion", "exo_outcome ~ lvi", df)

# Oncologic treatment
add_section("Oncologic Treatment")
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Neoadjuvant therapy", "exo_outcome ~ nat_binary", df)
rows_5a[[length(rows_5a) + 1]] <- glm_row(
  "Adjuvant chemotherapy", "exo_outcome ~ systemic_chemo", df)

tbl5a <- bind_rows(rows_5a)
colnames(tbl5a) <- c("Variable", "N", "OR", "95% CI", "P-Value")
header_rows_5a  <- which(tbl5a[["OR"]] == "" & tbl5a[["N"]] == "")

# -----------------------------------------------------------------------------
# 5. TABLE 5B — MULTIVARIABLE ANALYSIS
# Model A: Full cohort
# Model B: HbA1c subcohort
# Both use Firth penalized likelihood
# -----------------------------------------------------------------------------

# --- Model A: Full cohort ---
mv_formula_A <- paste0(
  "exo_outcome ~ ",
  "age_at_surgery + bmi_cat + family_hx_dm + ",
  "cr_popf + nat_binary + adenocarcinoma + ",
  "hx_pancreatitis + tobacco_ever + ",
  "distal_resection + dm_predictor"
)

fit_A <- logistf(as.formula(mv_formula_A), data = df,
                 firth = TRUE, pl = TRUE)

cat("=== Model A summary (Full cohort, Firth) ===\n")
cat("N =", fit_A$n, "\n")
print(summary(fit_A))

# --- Model B: HbA1c subcohort ---
mv_formula_B <- paste0(
  "exo_outcome ~ ",
  "age_at_surgery + bmi_cat + hba1c_preop_cat + family_hx_dm + ",
  "cr_popf + nat_binary + adenocarcinoma + ",
  "hx_pancreatitis + dm_predictor"
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
  "family_hx_dm"                          = "Family history of DM",
  "cr_popf"                               = "Clinically relevant POPF",
  "nat_binary"                            = "Neoadjuvant therapy",
  "adenocarcinoma"                        = "Adenocarcinoma diagnosis",
  "hx_pancreatitis"                       = "History of pancreatitis",
  "tobacco_ever"                          = "Tobacco use (ever)",
  "distal_resection"                      = "Distal pancreatectomy",
  "dm_predictor"                          = "New-onset diabetes mellitus"
)

# --- Helper: extract logistf results ---
extract_logistf <- function(fit, model_label) {

  coefs  <- fit$coefficients
  pvals  <- fit$prob
  ci_lo  <- fit$ci.lower
  ci_hi  <- fit$ci.upper
  n_used <- fit$n

  rows <- list()

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

rows_A <- extract_logistf(fit_A, "Model A — Full cohort")
rows_B <- extract_logistf(fit_B, "Model B — HbA1c subcohort")

spacer <- data.frame(Variable = "", N = "", OR = "", CI = "", P = "",
                     stringsAsFactors = FALSE)

tbl5b <- bind_rows(rows_A, spacer, rows_B)
colnames(tbl5b) <- c("Variable", "N", "OR", "95% CI", "P-Value")

header_rows_5b <- which(tbl5b[["OR"]] == "" & tbl5b[["N"]] != "" |
                         tbl5b[["OR"]] == "" & tbl5b[["N"]] == "" &
                         tbl5b[["Variable"]] != "")

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

tbl5a[["P-Value"]] <- sapply(tbl5a[["P-Value"]], mark_p, threshold = 0.1)
tbl5b[["P-Value"]] <- sapply(tbl5b[["P-Value"]], mark_p, threshold = 0.05)

cat("Significance markers applied:\n")
cat("  Table 5A: * = p < 0.10\n")
cat("  Table 5B: * = p < 0.05\n\n")

# -----------------------------------------------------------------------------
# 6. PRINT TO TERMINAL
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 78), "\n", sep = "")
cat("Table 5A. Univariable Logistic Regression — Exocrine Insufficiency\n")
cat(strrep("=", 78), "\n")
print(kable(tbl5a, format = "markdown", align = c("l","r","c","c","c")))

cat("\n", strrep("=", 78), "\n", sep = "")
cat("Table 5B. Multivariable Logistic Regression — Exocrine Insufficiency\n")
cat(strrep("=", 78), "\n")
print(kable(tbl5b, format = "markdown", align = c("l","r","c","c","c")))

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
        "Table 5A: Odds ratios estimated by standard binary logistic regression; ",
        "* denotes p < 0.10. ",
        "Table 5B: Odds ratios estimated by Firth penalized likelihood regression ",
        "to address sparse data; profile likelihood confidence intervals reported; ",
        "* denotes p < 0.05. ",
        "Patients with pre-operative exocrine insufficiency were excluded from ",
        "the outcome analysis (n = ", sum(df$preop_exo_insuff == 1, na.rm = TRUE), ") ",
        "as this precluded attribution of exocrine dysfunction to the surgical procedure. ",
        "Model A was fitted in the full analytic cohort, excluding predictors with ",
        ">25% missing data (pre-operative HbA1c, pancreatic duct diameter, gland texture). ",
        "Model B was restricted to patients with available pre-operative HbA1c ",
        "(n = ", fit_B$n, "). ",
        "New-onset diabetes mellitus was included as a concurrent marker of ",
        "pancreatic parenchymal loss rather than a causal predictor; ",
        "its association with exocrine insufficiency reflects shared ",
        "pathophysiologic mechanisms of type 3c pancreatic dysfunction. ",
        "The inverse association of distal pancreatectomy with exocrine insufficiency ",
        "likely reflects preservation of the main pancreatic ductal outflow mechanism ",
        "and sphincter of Oddi function compared with pancreaticoduodenectomy. ",
        "Reference categories: BMI, Normal (18.5-24.9 kg/m²); ",
        "HbA1c, Normal (<5.7%). ",
        "Patients with pre-operative HbA1c >=6.5% were excluded from the analytic ",
        "cohort (n = 11) as this value meets the American Diabetes Association ",
        "diagnostic threshold for diabetes mellitus irrespective of formal clinical diagnosis."
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

# -----------------------------------------------------------------------------
# 8. EXPORT
# -----------------------------------------------------------------------------

caption_5a <- paste0(
  "<b>Table 5A.</b> Univariable Logistic Regression for Predictors of ",
  "Post-Operative Exocrine Insufficiency Following Pancreatectomy ",
  "(Events: ", n_events, "/", n_total, " [",
  sprintf("%.1f%%", n_events / n_total * 100), "])"
)

caption_5b <- paste0(
  "<b>Table 5B.</b> Multivariable Logistic Regression for Predictors of ",
  "Post-Operative Exocrine Insufficiency Following Pancreatectomy ",
  "(Events: ", n_events, "/", n_total, " [",
  sprintf("%.1f%%", n_events / n_total * 100), "]). ",
  "Model A: full cohort (N = ", fit_A$n, "); ",
  "Model B: HbA1c subcohort (N = ", fit_B$n, ")."
)

html_5a <- build_html_reg(
  tbl          = tbl5a,
  header_rows  = header_rows_5a,
  table_id     = "Table 5A - Univariable Logistic Regression - Exocrine",
  caption_text = caption_5a
)

html_5b <- build_html_reg(
  tbl          = tbl5b,
  header_rows  = header_rows_5b,
  table_id     = "Table 5B - Multivariable Logistic Regression - Exocrine",
  caption_text = caption_5b
)

path_5a <- file.path(out_dir, "Table5A_Univariable_Exocrine.html")
path_5b <- file.path(out_dir, "Table5B_Multivariable_Exocrine.html")

writeLines(html_5a, path_5a)
writeLines(html_5b, path_5b)

cat("\u2713 Table 5A exported to:\n  ", path_5a, "\n")
cat("\u2713 Table 5B exported to:\n  ", path_5b, "\n\n")