# =============================================================================
# Figure 7: Forest Plot — Predictors of Post-Operative Exocrine Insufficiency
# =============================================================================

library(dplyr)
library(logistf)
library(forestploter)
library(grid)

df <- readRDS("C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/datamerged_endo_exo_PD_clean.rds")

fig_out <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/Endo_Exo_Results/Figures"
dir.create(fig_out, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. PREPARE DATA
# -----------------------------------------------------------------------------

df <- df %>%
  mutate(
    # Outcome — exclude patients with pre-existing exocrine insufficiency
    exo_outcome = case_when(
      preop_exo_insuff == 1  ~ NA_integer_,
      exo_insufficiency == 1 ~ 1L,
      exo_insufficiency == 0 ~ 0L,
      TRUE                   ~ NA_integer_
    ),

    bmi_cat = factor(case_when(
      bmi < 18.5              ~ "Underweight (<18.5)",
      bmi >= 18.5 & bmi < 25 ~ "Normal (18.5-24.9)",
      bmi >= 25   & bmi < 30 ~ "Overweight (25-29.9)",
      bmi >= 30               ~ "Obese (>=30)",
      TRUE ~ NA_character_),
      levels = c("Normal (18.5-24.9)", "Underweight (<18.5)",
                 "Overweight (25-29.9)", "Obese (>=30)")),

    hba1c_preop_cat = factor(case_when(
      hb_a1c_pre_op < 5.7                         ~ "Normal (<5.7)",
      hb_a1c_pre_op >= 5.7 & hb_a1c_pre_op < 6.5 ~ "Prediabetes (5.7-6.4)",
      TRUE ~ NA_character_),
      levels = c("Normal (<5.7)", "Prediabetes (5.7-6.4)")),

    hx_pancreatitis = case_when(
      pancreatitis %in% c("acute", "chronic") ~ 1L,
      pancreatitis == "none"                  ~ 0L,
      TRUE ~ NA_integer_),

    nat_binary       = as.integer(!is.na(nat) & nat == 1),
    cr_popf          = as.integer(cr_popf == 1),
    tobacco_ever     = case_when(
      tobacco == "never"                 ~ 0L,
      tobacco %in% c("past", "current") ~ 1L,
      TRUE ~ NA_integer_),
    adenocarcinoma   = as.integer(
      path_dx1_sub == "Adenocarcinoma (Panc/CBD/Ampulla/Duodenum)"),
    distal_resection = as.integer(resection_type == "Distal"),
    dm_predictor     = as.integer(new_onset_dm == 1)
  )

# -----------------------------------------------------------------------------
# 2. FIT MODELS
# -----------------------------------------------------------------------------

fit_A <- logistf(
  exo_outcome ~ age_at_surgery + bmi_cat + family_hx_dm +
    cr_popf + nat_binary + adenocarcinoma +
    hx_pancreatitis + tobacco_ever +
    distal_resection + dm_predictor,
  data = df, firth = TRUE, pl = TRUE)

fit_B <- logistf(
  exo_outcome ~ age_at_surgery + bmi_cat + hba1c_preop_cat +
    family_hx_dm + cr_popf + nat_binary + adenocarcinoma +
    hx_pancreatitis + dm_predictor,
  data = df, firth = TRUE, pl = TRUE)

cat("Model A N:", fit_A$n, "\n")
cat("Model B N:", fit_B$n, "\n\n")

# -----------------------------------------------------------------------------
# 3. EXTRACT RESULTS HELPERS
# -----------------------------------------------------------------------------

format_p <- function(p) {
  if (is.na(p)) return("")
  symbol <- dplyr::case_when(
    p < 0.05 ~ " *",
    p < 0.10 ~ " ~",
    TRUE     ~ ""
  )
  if (p < 0.001) return(paste0("< 0.001", symbol))
  paste0(sprintf("%.3f", p), symbol)
}

extract_model <- function(fit) {
  data.frame(
    term = names(fit$coefficients),
    or   = exp(fit$coefficients),
    lo   = exp(fit$ci.lower),
    hi   = exp(fit$ci.upper),
    pval = fit$prob,
    stringsAsFactors = FALSE
  ) %>% filter(term != "(Intercept)")
}

res_A <- extract_model(fit_A)
res_B <- extract_model(fit_B)

# -----------------------------------------------------------------------------
# 4. BUILD FOREST PLOT DATA FRAME
# -----------------------------------------------------------------------------

get_row <- function(res, term_name, display_label, indent = TRUE) {
  r      <- res %>% filter(term == term_name)
  prefix <- if (indent) "    " else ""

  if (nrow(r) == 0) {
    return(data.frame(
      Variable  = paste0(prefix, display_label),
      OR        = "\u2014",
      `95% CI`  = "\u2014",
      `P-Value` = "\u2014",
      or = NA_real_, lo = NA_real_, hi = NA_real_, pval = NA_real_,
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }

  data.frame(
    Variable  = paste0(prefix, display_label),
    OR        = sprintf("%.2f", r$or),
    `95% CI`  = sprintf("%.2f\u2013%.2f", r$lo, r$hi),
    `P-Value` = format_p(r$pval),
    or        = r$or,
    lo        = r$lo,
    hi        = r$hi,
    pval      = r$pval,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

header_row <- function(label) {
  data.frame(
    Variable = label,
    OR = "", `95% CI` = "", `P-Value` = "",
    or = NA_real_, lo = NA_real_, hi = NA_real_, pval = NA_real_,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

ref_row <- function(label) {
  data.frame(
    Variable  = paste0("    ", label, " (Ref)"),
    OR        = "1.00",
    `95% CI`  = "\u2014",
    `P-Value` = "\u2014",
    or = 1, lo = NA_real_, hi = NA_real_, pval = NA_real_,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

spacer_row <- function() {
  data.frame(
    Variable = "",
    OR = "", `95% CI` = "", `P-Value` = "",
    or = NA_real_, lo = NA_real_, hi = NA_real_, pval = NA_real_,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# --- Build Model A rows ---
rows_A <- bind_rows(
  header_row(paste0("Model A \u2014 Full Cohort (N = ", fit_A$n, ")")),
  header_row("Demographics"),
  get_row(res_A, "age_at_surgery",               "Age at surgery (per year)"),
  header_row("BMI (ref: Normal 18.5-24.9)"),
  ref_row("Normal (18.5-24.9)"),
  get_row(res_A, "bmi_catUnderweight (<18.5)",    "Underweight (<18.5)"),
  get_row(res_A, "bmi_catOverweight (25-29.9)",   "Overweight (25-29.9)"),
  get_row(res_A, "bmi_catObese (>=30)",           "Obese (\u226530)"),
  header_row("Pre-operative Comorbidities"),
  get_row(res_A, "family_hx_dm",                 "Family history of DM"),
  get_row(res_A, "hx_pancreatitis",              "History of pancreatitis"),
  get_row(res_A, "tobacco_ever",                 "Tobacco use (ever)"),
  header_row("Peri-operative Factors"),
  get_row(res_A, "cr_popf",                      "Clinically relevant POPF"),
  get_row(res_A, "nat_binary",                   "Neoadjuvant therapy"),
  get_row(res_A, "distal_resection",             "Distal pancreatectomy"),
  header_row("Pathologic Characteristics"),
  get_row(res_A, "adenocarcinoma",               "Adenocarcinoma diagnosis"),
  header_row("Post-operative Outcomes"),
  get_row(res_A, "dm_predictor",                 "New-onset diabetes mellitus")
)

# --- Build Model B rows ---
rows_B <- bind_rows(
  spacer_row(),
  header_row(paste0("Model B \u2014 HbA1c Subcohort (N = ", fit_B$n, ")")),
  header_row("Demographics"),
  get_row(res_B, "age_at_surgery",               "Age at surgery (per year)"),
  header_row("BMI (ref: Normal 18.5-24.9)"),
  ref_row("Normal (18.5-24.9)"),
  get_row(res_B, "bmi_catUnderweight (<18.5)",    "Underweight (<18.5)"),
  get_row(res_B, "bmi_catOverweight (25-29.9)",   "Overweight (25-29.9)"),
  get_row(res_B, "bmi_catObese (>=30)",           "Obese (\u226530)"),
  header_row("Pre-operative Glycemic Status"),
  ref_row("Normal HbA1c (<5.7%)"),
  get_row(res_B, "hba1c_preop_catPrediabetes (5.7-6.4)", "Prediabetes (5.7-6.4%)"),
  header_row("Pre-operative Comorbidities"),
  get_row(res_B, "family_hx_dm",                 "Family history of DM"),
  get_row(res_B, "hx_pancreatitis",              "History of pancreatitis"),
  header_row("Peri-operative Factors"),
  get_row(res_B, "cr_popf",                      "Clinically relevant POPF"),
  get_row(res_B, "nat_binary",                   "Neoadjuvant therapy"),
  header_row("Pathologic Characteristics"),
  get_row(res_B, "adenocarcinoma",               "Adenocarcinoma diagnosis"),
  header_row("Post-operative Outcomes"),
  get_row(res_B, "dm_predictor",                 "New-onset diabetes mellitus")
)

fp_data <- bind_rows(rows_A, rows_B)

# Silence NA CI warnings on ref rows
ref_mask             <- grepl("\\(Ref\\)", fp_data$Variable)
fp_data$lo[ref_mask] <- 1
fp_data$hi[ref_mask] <- 1

# Blank column required by forestploter for the CI plot area
fp_data$` ` <- paste(rep(" ", 20), collapse = " ")

# -----------------------------------------------------------------------------
# 5. IDENTIFY ROW TYPES
# -----------------------------------------------------------------------------

is_header    <- fp_data$OR == "" & fp_data$Variable != ""
is_model_hdr <- grepl("^Model [AB]", fp_data$Variable)

# -----------------------------------------------------------------------------
# 6. THEME
# -----------------------------------------------------------------------------

fp_theme <- forest_theme(
  base_size        = 9,
  base_family      = "sans",
  ci_lty           = 1,
  ci_lwd           = 1.5,
  ci_Theight       = 0.2,
  ci_col           = "#1A7340",   # green palette to distinguish from Figure 6
  ci_fill          = "#1A7340",
  ci_alpha         = 0.9,
  refline_gp       = gpar(col = "grey50", lty = "dashed", lwd = 0.8),
  arrow_type       = "closed",
  colhead_col      = "black",
  colhead_fontface = "bold",
  body_col         = "black",
  body_fontface    = "plain",
  summary_col      = "black",
  summary_fill     = "#EAFAF1",   # light green tint for section headers
  summary_fontface = "bold"
)

# -----------------------------------------------------------------------------
# 7. BUILD FOREST PLOT
# -----------------------------------------------------------------------------

fp <- forest(
  data         = fp_data[, c("Variable", "OR", "95% CI", "P-Value", " ")],
  est          = fp_data$or,
  lower        = fp_data$lo,
  upper        = fp_data$hi,
  ci_column    = 5,
  ref_line     = 1,
  x_trans      = "log",

  ## ADJUST X-AXIS RANGE OF THE FOREST PLOT — c(min, max) on log scale
  xlim         = c(0.1, 10),

  ## ADJUST X-AXIS TICK POSITIONS
  ticks_at     = c(0.25, 0.5, 1, 2, 4, 8),

  ticks_digits = 1,
  theme        = fp_theme,
  is_summary   = is_header,

  ## ADJUST COLUMN WIDTHS (in mm): c(Variable, OR, 95%CI, P-Value, Plot)
  ## Increase last value to widen CI plot area
  ## Increase first value if Variable labels get clipped
  col_widths   = unit(c(62, 10, 18, 12, 68), "mm")
)

# -----------------------------------------------------------------------------
# 8. POST-PROCESS: BOLD MODEL HEADERS + COLOR SIG / TREND ROWS
# -----------------------------------------------------------------------------

sig_rows   <- which(!is.na(fp_data$pval) & fp_data$pval < 0.05)
trend_rows <- which(!is.na(fp_data$pval) & fp_data$pval >= 0.05 & fp_data$pval < 0.10)

ci_col_idx   <- 5   # " " plot column
pval_col_idx <- 4   # P-Value column

# Significant rows → red CI + bold red p-value
for (r in sig_rows) {
  fp <- edit_plot(fp, row = r, col = ci_col_idx,   which = "ci",
                  gp = gpar(col = "#C0392B", fill = "#C0392B"))
  fp <- edit_plot(fp, row = r, col = pval_col_idx, which = "text",
                  gp = gpar(fontface = "bold", col = "#C0392B"))
}

# Trend rows → orange CI + italic orange p-value
for (r in trend_rows) {
  fp <- edit_plot(fp, row = r, col = ci_col_idx,   which = "ci",
                  gp = gpar(col = "#E67E22", fill = "#E67E22"))
  fp <- edit_plot(fp, row = r, col = pval_col_idx, which = "text",
                  gp = gpar(fontface = "italic", col = "#E67E22"))
}

# Bold + slightly larger font for Model A / Model B header rows
model_rows <- which(is_model_hdr)
for (r in model_rows) {
  fp <- edit_plot(fp, row = r, col = 1, which = "text",
                  gp = gpar(fontface = "bold", fontsize = 10))
}

# -----------------------------------------------------------------------------
# 9. EXPORT
# -----------------------------------------------------------------------------

path_png <- file.path(fig_out, "Figure7_ForestPlot_Exocrine_Predictors.png")
path_pdf <- file.path(fig_out, "Figure7_ForestPlot_Exocrine_Predictors.pdf")

## ADJUST OVERALL IMAGE SIZE (inches): width controls lateral space,
## height controls vertical space — increase if rows get cramped
png(path_png, width = 6, height = 10, units = "in", res = 600, bg = "white")
print(fp)
dev.off()

pdf(path_pdf, width = 6, height = 10, bg = "white")
print(fp)
dev.off()

cat("\u2713 Figure 7 PNG:", path_png, "\n")
cat("\u2713 Figure 7 PDF:", path_pdf, "\n\n")

# -----------------------------------------------------------------------------
# 10. FIGURE LEGEND
# -----------------------------------------------------------------------------

cat("=== Suggested figure legend ===\n")
cat(paste0(
  "Figure 7. Forest plot of multivariable predictors of post-operative exocrine ",
  "insufficiency following pancreatectomy. ",
  "Odds ratios with 95% confidence intervals from Firth penalized likelihood ",
  "logistic regression are shown for Model A (full analytic cohort, N = ",
  fit_A$n, ") and Model B (pre-operative HbA1c subcohort, N = ", fit_B$n, "). ",
  "Red markers (*) denote predictors reaching statistical significance (p < 0.05); ",
  "orange markers (~) denote trends toward significance (0.05 \u2264 p < 0.10). ",
  "Patients with pre-operative exocrine insufficiency were excluded from the ",
  "outcome analysis as this precluded attribution of exocrine dysfunction to the ",
  "surgical procedure. ",
  "Model A excludes pre-operative HbA1c due to >25% missingness; ",
  "Model B is restricted to patients with available pre-operative HbA1c. ",
  "Patients with pre-operative HbA1c \u22656.5% were excluded (n = 11). ",
  "New-onset diabetes mellitus is included as a concurrent marker of pancreatic ",
  "parenchymal loss rather than a causal predictor. ",
  "OR = odds ratio; CI = confidence interval; ",
  "POPF = postoperative pancreatic fistula; DM = diabetes mellitus; ",
  "Ref = reference category."
), "\n")