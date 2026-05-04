# =============================================================================
# Linear Mixed Model: HbA1c Trajectory Following Pancreatectomy
# Model 1: Unadjusted (N~700)
# Model 2A: Adjusted full cohort — no pre-op HbA1c (N~696)
# Model 2B: Adjusted HbA1c subcohort — includes pre-op HbA1c (N~303)
# =============================================================================

library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(patchwork)
library(knitr)
library(kableExtra)

df <- readRDS("C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/datamerged_endo_exo_PD_clean.rds")

out_dir <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/Endo_Exo_Results"
dir.create(file.path(out_dir, "Tables"),  showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "Figures"), showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. PREPARE BASELINE DATASET
# -----------------------------------------------------------------------------

df_base <- df %>%
  filter(!is.na(new_onset_dm)) %>%
  filter(is.na(hb_a1c_pre_op) | hb_a1c_pre_op < 6.5) %>%
  mutate(

    dm_hba1c_group = case_when(
      new_onset_dm == 1                                           ~ "New-Onset DM",
      new_onset_dm == 0 &
        hba1c_preop_cat == "Prediabetes (5.7-6.4)"               ~ "No DM — Prediabetes",
      new_onset_dm == 0 &
        hba1c_preop_cat == "Normal (<5.7)"                       ~ "No DM — Normal HbA1c",
      new_onset_dm == 0 & is.na(hba1c_preop_cat)                 ~ "No DM — HbA1c unknown",
      TRUE ~ NA_character_
    ),
    dm_hba1c_group = factor(dm_hba1c_group,
                            levels = c("No DM — Normal HbA1c",
                                       "No DM — Prediabetes",
                                       "No DM — HbA1c unknown",
                                       "New-Onset DM")),

    bmi_cat = case_when(
      bmi < 18.5              ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal",
      bmi >= 25   & bmi < 30 ~ "Overweight",
      bmi >= 30               ~ "Obese",
      TRUE ~ NA_character_
    ),
    bmi_cat = factor(bmi_cat,
                     levels = c("Normal", "Underweight",
                                "Overweight", "Obese")),

    distal_resection = as.integer(resection_type == "Distal"),
    nat_binary       = as.integer(!is.na(nat) & nat == 1),
    adenocarcinoma   = as.integer(
      path_dx1_sub == "Adenocarcinoma (Panc/CBD/Ampulla/Duodenum)"),
    tobacco_ever = case_when(
      tobacco == "never"                 ~ 0L,
      tobacco %in% c("past", "current") ~ 1L,
      TRUE ~ NA_integer_
    )
  )

# -----------------------------------------------------------------------------
# 2. RESHAPE TO LONG FORMAT
# -----------------------------------------------------------------------------

hba1c_long <- df_base %>%
  select(mayo_id, dm_hba1c_group, new_onset_dm,
         hb_a1c_pre_op, bmi_cat, age_at_surgery,
         distal_resection, nat_binary, adenocarcinoma, tobacco_ever,
         hb_a1c_y1:hb_a1c_y10) %>%
  pivot_longer(
    cols      = hb_a1c_y1:hb_a1c_y10,
    names_to  = "timepoint",
    values_to = "hba1c"
  ) %>%
  mutate(
    time   = as.numeric(gsub("hb_a1c_y", "", timepoint)),
    time_c = time - 1
  ) %>%
  filter(!is.na(hba1c))

cat("=== Long format ===\n")
cat("Observations:", nrow(hba1c_long), "\n")
cat("Patients:", n_distinct(hba1c_long$mayo_id), "\n\n")

# -----------------------------------------------------------------------------
# 3. FIT MODELS
# -----------------------------------------------------------------------------

cat("=== Model 1: Unadjusted, random intercept + slope ===\n")
m1 <- lmer(
  hba1c ~ time_c * dm_hba1c_group + (1 + time_c | mayo_id),
  data    = hba1c_long,
  REML    = TRUE,
  control = lmerControl(optimizer = "bobyqa",
                        optCtrl   = list(maxfun = 2e5))
)
cat("N obs:", nrow(model.frame(m1)),
    "| N pts:", n_distinct(model.frame(m1)$mayo_id), "\n")
print(summary(m1))

cat("\n=== Model 2A: Adjusted full cohort (no pre-op HbA1c) ===\n")
m2a <- lmer(
  hba1c ~ time_c * dm_hba1c_group +
    age_at_surgery + bmi_cat +
    distal_resection + nat_binary +
    adenocarcinoma + tobacco_ever +
    (1 | mayo_id),
  data    = hba1c_long,
  REML    = TRUE,
  control = lmerControl(optimizer = "bobyqa",
                        optCtrl   = list(maxfun = 2e5))
)
cat("N obs:", nrow(model.frame(m2a)),
    "| N pts:", n_distinct(model.frame(m2a)$mayo_id), "\n")
print(summary(m2a))

cat("\n=== Model 2B: Adjusted HbA1c subcohort (with pre-op HbA1c) ===\n")
m2b <- lmer(
  hba1c ~ time_c * dm_hba1c_group +
    hb_a1c_pre_op + age_at_surgery + bmi_cat +
    distal_resection + nat_binary +
    adenocarcinoma + tobacco_ever +
    (1 | mayo_id),
  data    = hba1c_long %>% filter(!is.na(hb_a1c_pre_op)),
  REML    = TRUE,
  control = lmerControl(optimizer = "bobyqa",
                        optCtrl   = list(maxfun = 2e5))
)
cat("N obs:", nrow(model.frame(m2b)),
    "| N pts:", n_distinct(model.frame(m2b)$mayo_id), "\n")
print(summary(m2b))

# AIC comparison
cat("\n=== AIC comparison ===\n")
cat("Model 1  AIC:", AIC(m1),  "\n")
cat("Model 2A AIC:", AIC(m2a), "\n")
cat("Model 2B AIC:", AIC(m2b), "\n\n")

# -----------------------------------------------------------------------------
# 4. EXTRACT FIXED EFFECTS TABLE
# -----------------------------------------------------------------------------

term_labels <- c(
  "(Intercept)"                                        = "Intercept",
  "time_c"                                             = "Time (per year after Y1)",
  "dm_hba1c_groupNo DM \u2014 Prediabetes"             = "No DM \u2014 Prediabetes vs Normal HbA1c",
  "dm_hba1c_groupNo DM \u2014 HbA1c unknown"           = "No DM \u2014 HbA1c unknown vs Normal HbA1c",
  "dm_hba1c_groupNew-Onset DM"                         = "New-Onset DM vs Normal HbA1c",
  "hb_a1c_pre_op"                                      = "Pre-op HbA1c (per 1%)",
  "age_at_surgery"                                     = "Age at surgery (per year)",
  "bmi_catUnderweight"                                 = "    BMI: Underweight vs Normal",
  "bmi_catOverweight"                                  = "    BMI: Overweight vs Normal",
  "bmi_catObese"                                       = "    BMI: Obese vs Normal",
  "distal_resection"                                   = "Distal pancreatectomy",
  "nat_binary"                                         = "Neoadjuvant therapy",
  "adenocarcinoma"                                     = "Adenocarcinoma diagnosis",
  "tobacco_ever"                                       = "Tobacco use (ever)",
  "time_c:dm_hba1c_groupNo DM \u2014 Prediabetes"      = "Time \u00d7 No DM \u2014 Prediabetes",
  "time_c:dm_hba1c_groupNo DM \u2014 HbA1c unknown"    = "Time \u00d7 No DM \u2014 HbA1c unknown",
  "time_c:dm_hba1c_groupNew-Onset DM"                  = "Time \u00d7 New-Onset DM"
)

sections <- list(
  "Group and Time Effects" = c(
    "(Intercept)", "time_c",
    "dm_hba1c_groupNo DM \u2014 Prediabetes",
    "dm_hba1c_groupNo DM \u2014 HbA1c unknown",
    "dm_hba1c_groupNew-Onset DM"
  ),
  "Time \u00d7 Group Interactions" = c(
    "time_c:dm_hba1c_groupNo DM \u2014 Prediabetes",
    "time_c:dm_hba1c_groupNo DM \u2014 HbA1c unknown",
    "time_c:dm_hba1c_groupNew-Onset DM"
  ),
  "Baseline Covariates" = c(
    "hb_a1c_pre_op", "age_at_surgery",
    "bmi_catUnderweight", "bmi_catOverweight", "bmi_catObese",
    "distal_resection", "nat_binary",
    "adenocarcinoma", "tobacco_ever"
  )
)

## ----

extract_lmm <- function(model, include_covariates = TRUE) {

  coefs <- as.data.frame(coef(summary(model)))
  coefs$term <- rownames(coefs)
  rows <- list()

  # Sections to include
  sections_to_run <- if (include_covariates) {
    names(sections)
  } else {
    c("Group and Time Effects", "Time \u00d7 Group Interactions")
  }

  for (section_name in sections_to_run) {
    rows[[length(rows) + 1]] <- data.frame(
      Variable = section_name,
      Beta = "", SE = "", CI = "", P = "",
      stringsAsFactors = FALSE
    )

    for (tm in sections[[section_name]]) {
      if (!tm %in% coefs$term) {
        label <- ifelse(tm %in% names(term_labels), term_labels[tm], tm)
        rows[[length(rows) + 1]] <- data.frame(
          Variable = paste0("    ", label),
          Beta = "\u2014", SE = "\u2014", CI = "\u2014", P = "\u2014",
          stringsAsFactors = FALSE
        )
        next
      }

      row   <- coefs[coefs$term == tm, ]
      beta  <- row$Estimate
      se    <- row$`Std. Error`
      pval  <- row$`Pr(>|t|)`
      ci_lo <- beta - 1.96 * se
      ci_hi <- beta + 1.96 * se

      p_str <- if (is.na(pval))       "\u2014"
               else if (pval < 0.001) "<0.001 *"
               else if (pval < 0.05)  paste0(sprintf("%.3f", pval), " *")
               else if (pval < 0.10)  paste0(sprintf("%.3f", pval), " \u2020")
               else                   sprintf("%.3f", pval)

      label <- ifelse(tm %in% names(term_labels), term_labels[tm], tm)

      rows[[length(rows) + 1]] <- data.frame(
        Variable = paste0("    ", label),
        Beta     = sprintf("%.3f", beta),
        SE       = sprintf("%.3f", se),
        CI       = sprintf("%.3f\u2013%.3f", ci_lo, ci_hi),
        P        = p_str,
        stringsAsFactors = FALSE
      )
    }
  }
  bind_rows(rows)
}

# Model 1 — no covariates section
tbl_m1  <- extract_lmm(m1,  include_covariates = FALSE)

# Models 2A and 2B — include covariates section
tbl_m2a <- extract_lmm(m2a, include_covariates = TRUE)
tbl_m2b <- extract_lmm(m2b, include_covariates = TRUE)

### ----

# Add N row and model headers
make_header <- function(label, n_obs, n_pts) {
  data.frame(
    Variable = paste0(label, " (N obs = ", n_obs,
                      "; N patients = ", n_pts, ")"),
    Beta = "", SE = "", CI = "", P = "",
    stringsAsFactors = FALSE
  )
}

spacer <- data.frame(Variable = "", Beta = "", SE = "",
                     CI = "", P = "", stringsAsFactors = FALSE)

tbl_combined <- bind_rows(
  make_header("Model 1 — Unadjusted",
              nrow(model.frame(m1)),
              n_distinct(model.frame(m1)$mayo_id)),
  tbl_m1,
  spacer,
  make_header("Model 2A — Adjusted, full cohort",
              nrow(model.frame(m2a)),
              n_distinct(model.frame(m2a)$mayo_id)),
  tbl_m2a,
  spacer,
  make_header("Model 2B — Adjusted, HbA1c subcohort",
              nrow(model.frame(m2b)),
              n_distinct(model.frame(m2b)$mayo_id)),
  tbl_m2b
)

colnames(tbl_combined) <- c("Variable", "\u03b2", "SE", "95% CI", "P-Value")

# -----------------------------------------------------------------------------
# 5. PRINT TO TERMINAL
# -----------------------------------------------------------------------------

cat("\n=== TABLE 7: LMM Fixed Effects ===\n")
print(kable(tbl_combined, format = "markdown",
            align = c("l","r","r","c","c")))

# -----------------------------------------------------------------------------
# 6. KEY RESULTS SUMMARY
# -----------------------------------------------------------------------------

summarize_model <- function(model, label) {
  fe <- fixef(model)
  cat(paste0("\n--- ", label, " ---\n"))
  cat(sprintf("  Time (No DM Normal ref):           %.3f%% per year\n",
              fe["time_c"]))
  if ("time_c:dm_hba1c_groupNew-Onset DM" %in% names(fe))
    cat(sprintf("  Time x New-Onset DM interaction:   %.3f%% per year (p=%.3f)\n",
                fe["time_c:dm_hba1c_groupNew-Onset DM"],
                coef(summary(model))["time_c:dm_hba1c_groupNew-Onset DM",
                                     "Pr(>|t|)"]))
  if ("time_c:dm_hba1c_groupNo DM \u2014 Prediabetes" %in% names(fe))
    cat(sprintf("  Time x No DM Prediabetes:          %.3f%% per year\n",
                fe["time_c:dm_hba1c_groupNo DM \u2014 Prediabetes"]))
  if ("dm_hba1c_groupNew-Onset DM" %in% names(fe))
    cat(sprintf("  New-Onset DM baseline offset:      %.3f%%\n",
                fe["dm_hba1c_groupNew-Onset DM"]))
}

cat("\n=== KEY RESULTS SUMMARY ===\n")
summarize_model(m1,  "Model 1 — Unadjusted")
summarize_model(m2a, "Model 2A — Adjusted full cohort")
summarize_model(m2b, "Model 2B — Adjusted HbA1c subcohort")

# -----------------------------------------------------------------------------
# 7. ESTIMATED MARGINAL MEANS — Model 2A (primary, largest N)
# Suppress No DM — Prediabetes due to unreliable estimates (n~50)
# -----------------------------------------------------------------------------

emm_m2a <- emmeans(m2a,
                   specs   = ~ dm_hba1c_group * time_c,
                   at      = list(time_c = 0:9),
                   lmer.df = "satterthwaite")

emm_df <- as.data.frame(emm_m2a) %>%
  mutate(time = time_c + 1) %>%
  # Suppress Prediabetes group — too few observations for reliable EMMs
  filter(dm_hba1c_group != "No DM — Prediabetes")

cat("\n=== EMM summary (Model 2A, Prediabetes group suppressed) ===\n")
print(emm_df %>% select(dm_hba1c_group, time, emmean, lower.CL, upper.CL))

# -----------------------------------------------------------------------------
# 8. FIGURE — Model 2A EMM trajectory
# -----------------------------------------------------------------------------

cols_3 <- c(
  "No DM — Normal HbA1c"  = "#2471A3",
  "No DM — HbA1c unknown" = "#AAB7B8",
  "New-Onset DM"          = "#C0392B"
)

shapes_3 <- c(
  "No DM — Normal HbA1c"  = 21,
  "No DM — HbA1c unknown" = 23,
  "New-Onset DM"          = 24
)

fig_main <- ggplot(emm_df,
                   aes(x     = time,
                       y     = emmean,
                       color = dm_hba1c_group,
                       fill  = dm_hba1c_group,
                       group = dm_hba1c_group,
                       shape = dm_hba1c_group)) +

  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL),
              alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.8, color = "white", stroke = 1.2) +

  # Reference lines
  geom_hline(yintercept = 6.5, linetype = "dashed",
             color = "grey50", linewidth = 0.45) +
  annotate("text", x = 1.1, y = 6.62,
           label = "DM threshold (6.5%)",
           hjust = 0, size = 2.5, color = "grey40",
           fontface = "italic", family = "sans") +
  geom_hline(yintercept = 5.7, linetype = "dotted",
             color = "grey60", linewidth = 0.45) +
  annotate("text", x = 1.1, y = 5.82,
           label = "Prediabetes threshold (5.7%)",
           hjust = 0, size = 2.5, color = "grey50",
           fontface = "italic", family = "sans") +

  scale_x_continuous(breaks = 1:10,
                     labels = paste0("Y", 1:10),
                     expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(limits = c(4.0, 10.0),
                     breaks = seq(4.0, 10.0, 0.5),
                     name   = "Estimated HbA1c (%)") +
  scale_color_manual(values = cols_3, name = NULL) +
  scale_fill_manual(values  = cols_3, name = NULL) +
  scale_shape_manual(values = shapes_3, name = NULL) +

  labs(
    x        = NULL,
    subtitle = "Estimated marginal means from Model 2A (adjusted); shaded area = 95% CI"
  ) +

  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.text.x        = element_blank(),
    axis.text.y        = element_text(color = "black", size = 8),
    axis.title.y       = element_text(color = "black", size = 9,
                                      margin = margin(r = 6)),
    plot.subtitle      = element_text(size = 7.5, color = "grey40",
                                      margin = margin(b = 4)),
    legend.position    = c(0.18, 0.88),
    legend.background  = element_blank(),
    legend.key         = element_blank(),
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.35, "cm"),
    panel.grid.major.y = element_line(color = "grey93", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(t = 8, r = 8, b = 0, l = 8)
  )

# N-at-risk panel
n_risk <- hba1c_long %>%
  filter(dm_hba1c_group != "No DM — Prediabetes") %>%
  group_by(dm_hba1c_group, time) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    y_pos = case_when(
      dm_hba1c_group == "New-Onset DM"          ~ 2,
      dm_hba1c_group == "No DM — Normal HbA1c"  ~ 1,
      dm_hba1c_group == "No DM — HbA1c unknown" ~ 0
    )
  )

fig_ntable <- ggplot(n_risk,
                     aes(x = time, y = y_pos,
                         label = n, color = dm_hba1c_group)) +
  geom_text(size = 2.6, family = "sans") +

  annotate("text", x = 0.6, y = 2,
           label = "New-Onset DM",
           hjust = 1, size = 2.5,
           color = cols_3["New-Onset DM"],
           family = "sans", fontface = "bold") +
  annotate("text", x = 0.6, y = 1,
           label = "No DM — Normal HbA1c",
           hjust = 1, size = 2.5,
           color = cols_3["No DM — Normal HbA1c"],
           family = "sans", fontface = "bold") +
  annotate("text", x = 0.6, y = 0,
           label = "No DM — HbA1c unknown",
           hjust = 1, size = 2.5,
           color = cols_3["No DM — HbA1c unknown"],
           family = "sans", fontface = "bold") +

  scale_x_continuous(breaks = 1:10,
                     labels = paste0("Y", 1:10),
                     expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(limits = c(-0.6, 2.6)) +
  scale_color_manual(values = cols_3) +

  labs(x = "Time Since Pancreatectomy (Years)", y = NULL) +

  theme_void(base_family = "sans") +
  theme(
    axis.text.x     = element_text(color = "black", size = 8,
                                   margin = margin(t = 3)),
    axis.title.x    = element_text(color = "black", size = 9,
                                   margin = margin(t = 4)),
    legend.position = "none",
    plot.margin     = margin(t = 2, r = 8, b = 8, l = 8)
  )

fig_lmm <- fig_main / fig_ntable +
  plot_layout(heights = c(5, 1.2))

# -----------------------------------------------------------------------------
# 9. EXPORT FIGURE
# -----------------------------------------------------------------------------

path_png <- file.path(out_dir, "Figures/Figure2_LMM_HbA1c_Trajectory.png")
path_pdf <- file.path(out_dir, "Figures/Figure2_LMM_HbA1c_Trajectory.pdf")

ggsave(path_png, plot = fig_lmm,
       width = 7, height = 5.5, dpi = 600, bg = "white")
ggsave(path_pdf, plot = fig_lmm,
       width = 7, height = 5.5, device = cairo_pdf, bg = "white")

cat("\u2713 Figure 2 PNG:", path_png, "\n")
cat("\u2713 Figure 2 PDF:", path_pdf, "\n\n")

# -----------------------------------------------------------------------------
# 10. EXPORT HTML TABLE
# -----------------------------------------------------------------------------

header_rows <- which(tbl_combined[["SE"]] == "" &
                       tbl_combined[["\u03b2"]] == "")
last_row    <- nrow(tbl_combined)

ht7 <- kable(tbl_combined,
             format   = "html",
             align    = c("l","r","r","c","c"),
             escape   = FALSE,
             caption  = paste0(
               "<b>Table 7.</b> Linear Mixed Model Fixed Effects for ",
               "Post-Operative HbA1c Trajectory Following Pancreatectomy"
             )) %>%
  kable_styling(bootstrap_options = c("condensed"),
                full_width = TRUE, font_size = 12) %>%
  row_spec(0, bold = TRUE,
           extra_css = paste0(
             "border-top: 2px solid #000000 !important;",
             "border-bottom: 2px solid #000000 !important;"
           )) %>%
  row_spec(last_row,
           extra_css = "border-bottom: 2px solid #000000 !important;") %>%
  row_spec(seq_len(last_row - 1),
           extra_css = "border-top: none !important;
                        border-bottom: none !important;") %>%
  row_spec(header_rows, bold = TRUE,
           extra_css = "background-color: #f8f8f8 !important;
                        padding-top: 10px !important;") %>%
  column_spec(1, width = "45%",
              extra_css = "border-left: none !important;
                           border-right: none !important;") %>%
  column_spec(2:5, width = "13%",
              extra_css = "text-align: center !important;
                           border-left: none !important;
                           border-right: none !important;") %>%
  footnote(
    general = paste0(
      "\u03b2 = unstandardized regression coefficient (HbA1c %, change per unit). ",
      "SE = standard error. CI = 95% confidence interval (Wald). ",
      "Models fitted by restricted maximum likelihood (REML) using lme4. ",
      "P-values from Satterthwaite degrees of freedom approximation (lmerTest). ",
      "* p < 0.05; \u2020 p < 0.10. ",
      "Time is centered at Year 1 (time_c = 0 corresponds to Year 1 post-op). ",
      "The \u03b2 for Time represents the annual HbA1c change in the reference group ",
      "(No DM \u2014 Normal HbA1c). ",
      "Time \u00d7 Group interaction terms represent the additional annual change ",
      "in each group relative to the reference. ",
      "Model 1: unadjusted, random intercept + slope. ",
      "Model 2A: adjusted, random intercept, full cohort (pre-op HbA1c excluded ",
      "due to 56% missingness). ",
      "Model 2B: adjusted, random intercept, restricted to patients with ",
      "available pre-op HbA1c. ",
      "The No DM \u2014 Prediabetes group is included in model estimation but ",
      "suppressed from the trajectory figure due to unreliable estimated marginal ",
      "means arising from limited post-operative HbA1c observations (n \u2248 50). ",
      "Reference categories: DM/glycemic group, No DM \u2014 Normal HbA1c; ",
      "BMI, Normal (18.5\u201324.9 kg/m\u00b2). ",
      "Pre-operative HbA1c \u22656.5% excluded (n = 11, ADA DM criteria)."
    ),
    general_title     = "Abbreviations and Notes: ",
    footnote_as_chunk = TRUE
  )

html7 <- paste0(
  "<!DOCTYPE html>\n<html>\n<head>\n",
  "  <meta charset='UTF-8'>\n",
  "  <title>Table 7 - LMM HbA1c Trajectory</title>\n",
  "  <style>\n",
  "    body   { font-family: 'Times New Roman', Times, serif;",
  "             margin: 60px 80px; max-width: 860px;",
  "             color: #111; background: #fff; }\n",
  "    caption { font-size: 13px; margin-bottom: 6px;",
  "              text-align: left; caption-side: top; }\n",
  "    table  { border-collapse: collapse; width: 100%; }\n",
  "    td, th { padding: 4px 8px;",
  "             border-left: none !important;",
  "             border-right: none !important; }\n",
  "    .footnote-text { font-size: 11px; color: #444; margin-top: 6px; }\n",
  "  </style>\n",
  "</head>\n<body>\n",
  ht7,
  "\n</body>\n</html>"
)

path_tbl <- file.path(out_dir, "Tables/Table7_LMM_HbA1c_Trajectory.html")
writeLines(html7, path_tbl)
cat("\u2713 Table 7 exported to:\n  ", path_tbl, "\n\n")