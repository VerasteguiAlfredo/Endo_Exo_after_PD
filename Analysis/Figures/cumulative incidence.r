# =============================================================================
# Figure 3: Cumulative Incidence of New-Onset DM — Competing Risks Analysis
# Event: New-onset DM | Competing event: Death without DM
# =============================================================================

library(dplyr)
library(ggplot2)
library(patchwork)
library(cmprsk)      # cuminc() and crr()
library(tidycmprsk)  # tidy interface for competing risks
# install.packages(c("cmprsk", "tidycmprsk")) if needed

df <- readRDS("C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/datamerged_endo_exo_PD_clean.rds")

out_dir <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/Endo_Exo_Results"
dir.create(file.path(out_dir, "Figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "Tables"),  showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. PREPARE TIME-TO-EVENT DATASET
# -----------------------------------------------------------------------------

df_tte <- df %>%
  # Same cohort exclusions as regression models
  filter(!is.na(new_onset_dm)) %>%
  filter(is.na(hb_a1c_pre_op) | hb_a1c_pre_op < 6.5) %>%
  mutate(

    # --- Time to first event (days) ---
    # For DM patients: time from surgery to DM diagnosis
    # For non-DM patients: time from surgery to death or last follow-up
    time_to_event_days = case_when(
      new_onset_dm == 1 ~ days_to_dm,
      vital_status_fu == "Dead" ~ as.numeric(
        difftime(vital_fu_dt, dos_pancreatectomy, units = "days")),
      TRUE ~ as.numeric(
        difftime(vital_fu_dt, dos_pancreatectomy, units = "days"))
    ),

    # Convert to months for cleaner axis
    time_to_event_months = time_to_event_days / 30.44,

    # --- Event type ---
    # 0 = censored, 1 = new-onset DM, 2 = death without DM
    event_type = case_when(
      new_onset_dm == 1                              ~ 1L,
      vital_status_fu == "Dead" & new_onset_dm == 0  ~ 2L,
      TRUE                                           ~ 0L
    ),
    event_type = factor(event_type,
                        levels = c(0, 1, 2),
                        labels = c("Censored",
                                   "New-Onset DM",
                                   "Death without DM")),

    # --- Stratification variables ---
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

    resection_group = case_when(
      resection_type == "Distal" ~ "Distal",
      TRUE                       ~ "Non-Distal (PD/Total)"
    ),
    resection_group = factor(resection_group,
                             levels = c("Non-Distal (PD/Total)", "Distal")),

    adenocarcinoma = as.integer(
      path_dx1_sub == "Adenocarcinoma (Panc/CBD/Ampulla/Duodenum)"),

    nat_binary = as.integer(!is.na(nat) & nat == 1),

    hba1c_preop_cat = factor(case_when(
      hb_a1c_pre_op < 5.7                         ~ "Normal (<5.7%)",
      hb_a1c_pre_op >= 5.7 & hb_a1c_pre_op < 6.5 ~ "Prediabetes (5.7-6.4%)",
      TRUE ~ NA_character_),
      levels = c("Normal (<5.7%)", "Prediabetes (5.7-6.4%)"))
  ) %>%
  # Remove patients with missing or non-positive follow-up time
  filter(!is.na(time_to_event_days) & time_to_event_days > 0)

cat("=== Time-to-event dataset ===\n")
cat("Patients:", nrow(df_tte), "\n")
cat("\nEvent type distribution:\n")
print(table(df_tte$event_type, useNA = "ifany"))
cat("\nMedian follow-up (months):",
    round(median(df_tte$time_to_event_months, na.rm = TRUE), 1), "\n")
cat("Max follow-up (months):",
    round(max(df_tte$time_to_event_months, na.rm = TRUE), 1), "\n\n")

# -----------------------------------------------------------------------------
# 2. OVERALL CUMULATIVE INCIDENCE FUNCTION
# -----------------------------------------------------------------------------

cif_overall <- tidycmprsk::cuminc(
  Surv(time_to_event_months, event_type) ~ 1,
  data = df_tte
)

cat("=== Overall CIF at key time points ===\n")
print(tidy(cif_overall,
           times = c(12, 24, 36, 60, 120)) %>%
        filter(outcome == "New-Onset DM") %>%
        select(time, estimate, conf.low, conf.high))

# -----------------------------------------------------------------------------
# 3. CIF BY RESECTION TYPE
# -----------------------------------------------------------------------------

cif_resection <- tidycmprsk::cuminc(
  Surv(time_to_event_months, event_type) ~ resection_group,
  data = df_tte
)

cat("\n=== CIF by resection type at key time points ===\n")
print(tidy(cif_resection,
           times = c(12, 24, 36, 60, 120)) %>%
        filter(outcome == "New-Onset DM") %>%
        select(time, strata, estimate, conf.low, conf.high))

# Gray's test p-value
cat("\nGray's test (resection type):\n")
print(cif_resection$cmprsk$Tests)

# -----------------------------------------------------------------------------
# 4. CIF BY BMI CATEGORY
# -----------------------------------------------------------------------------

cif_bmi <- tidycmprsk::cuminc(
  Surv(time_to_event_months, event_type) ~ bmi_cat,
  data = df_tte %>% filter(!is.na(bmi_cat))
)

cat("\n=== CIF by BMI category at key time points ===\n")
print(tidy(cif_bmi,
           times = c(12, 24, 36, 60, 120)) %>%
        filter(outcome == "New-Onset DM") %>%
        select(time, strata, estimate, conf.low, conf.high))

cat("\nGray's test (BMI):\n")
print(cif_bmi$cmprsk$Tests)

# -----------------------------------------------------------------------------
# 5. FINE-GRAY SUBDISTRIBUTION HAZARD MODEL
# Regression accounting for competing risks
# Covariates from your significant regression predictors
# -----------------------------------------------------------------------------

cat("\n=== Fine-Gray Model: New-Onset DM ===\n")

cat("=== Event type numeric check ===\n")
cat("0 = Censored:", sum(as.integer(fg_data$event_type) - 1 == 0), "\n")
cat("1 = New-Onset DM:", sum(as.integer(fg_data$event_type) - 1 == 1), "\n")
cat("2 = Death:", sum(as.integer(fg_data$event_type) - 1 == 2), "\n\n")

fg_data <- df_tte %>%
  filter(!is.na(bmi_cat) & !is.na(adenocarcinoma) &
           !is.na(nat_binary) & !is.na(distal_resection)) %>%
  mutate(
    bmi_overweight = as.integer(bmi_cat == "Overweight"),
    bmi_obese      = as.integer(bmi_cat == "Obese"),
    bmi_underweight = as.integer(bmi_cat == "Underweight")
  )

cov_matrix <- as.matrix(fg_data %>%
  select(bmi_overweight, bmi_obese, bmi_underweight,
         distal_resection, adenocarcinoma, nat_binary,
         family_hx_dm))

fg_fit <- cmprsk::crr(
  ftime   = fg_data$time_to_event_months,
  fstatus = as.integer(fg_data$event_type) - 1,  # 0=cens, 1=DM, 2=death
  cov1    = cov_matrix,
  failcode = 1,   # DM is the event of interest
  cencode  = 0
)

cat("Fine-Gray model summary:\n")
print(summary(fg_fit))

# -----------------------------------------------------------------------------
# 6. EXTRACT FINE-GRAY RESULTS FOR TABLE
# -----------------------------------------------------------------------------

fg_coefs <- summary(fg_fit)$coef
fg_ci    <- summary(fg_fit)$conf.int

var_labels_fg <- c(
  "bmi_overweight"   = "BMI: Overweight vs Normal",
  "bmi_obese"        = "BMI: Obese vs Normal",
  "bmi_underweight"  = "BMI: Underweight vs Normal",
  "distal_resection" = "Distal pancreatectomy",
  "adenocarcinoma"   = "Adenocarcinoma diagnosis",
  "nat_binary"       = "Neoadjuvant therapy",
  "family_hx_dm"     = "Family history of DM"
)

fg_rows <- list()

for (nm in rownames(fg_coefs)) {
  shr    <- exp(fg_coefs[nm, "coef"])
  p_val  <- fg_coefs[nm, "p-value"]
  ci_lo  <- fg_ci[nm, "2.5%"]
  ci_hi  <- fg_ci[nm, "97.5%"]

  p_str <- if (p_val < 0.001) "<0.001 *"
            else if (p_val < 0.05) paste0(sprintf("%.3f", p_val), " *")
            else if (p_val < 0.10) paste0(sprintf("%.3f", p_val), " \u2020")
            else sprintf("%.3f", p_val)

  label <- ifelse(nm %in% names(var_labels_fg), var_labels_fg[nm], nm)

  fg_rows[[length(fg_rows) + 1]] <- data.frame(
    Variable = label,
    N        = ifelse(length(fg_rows) == 0, as.character(nrow(fg_data)), ""),
    SHR      = sprintf("%.2f", shr),
    CI       = sprintf("%.2f\u2013%.2f", ci_lo, ci_hi),
    P        = p_str,
    stringsAsFactors = FALSE
  )
}

tbl_fg <- bind_rows(fg_rows)
colnames(tbl_fg) <- c("Variable", "N", "SHR", "95% CI", "P-Value")

cat("\n=== Fine-Gray results table ===\n")
print(knitr::kable(tbl_fg, format = "markdown",
                   align = c("l","r","c","c","c")))

# -----------------------------------------------------------------------------
# 7. FIGURE 3A — Overall CIF: DM vs Death
# -----------------------------------------------------------------------------

cif_overall_df <- tidy(cif_overall) %>%
  filter(outcome %in% c("New-Onset DM", "Death without DM"))

cols_events <- c(
  "New-Onset DM"     = "#C0392B",
  "Death without DM" = "#2471A3"
)

fig_3a <- ggplot(cif_overall_df,
                 aes(x = time, y = estimate,
                     color = outcome, fill = outcome)) +

  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +

  scale_x_continuous(
    limits = c(0, 120),
    breaks = seq(0, 120, 12),
    labels = seq(0, 120, 12),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 0.60),
    breaks = seq(0, 0.60, 0.10),
    labels = scales::percent_format(accuracy = 1),
    name   = "Cumulative Incidence"
  ) +
  scale_color_manual(values = cols_events, name = NULL) +
  scale_fill_manual(values  = cols_events, name = NULL) +

  labs(
    x        = NULL,
    subtitle = "Overall cohort"
  ) +

  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.text.x        = element_blank(),
    axis.text.y        = element_text(color = "black", size = 8),
    axis.title.y       = element_text(color = "black", size = 9,
                                      margin = margin(r = 6)),
    plot.subtitle      = element_text(size = 8, color = "grey40"),
    legend.position    = c(0.25, 0.88),
    legend.background  = element_blank(),
    legend.key         = element_blank(),
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.35, "cm"),
    panel.grid.major.y = element_line(color = "grey93", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(t = 8, r = 8, b = 0, l = 8)
  )

# -----------------------------------------------------------------------------
# 8. FIGURE 3B — CIF by Resection Type (DM only)
# -----------------------------------------------------------------------------

cif_resection_df <- tidy(cif_resection) %>%
  filter(outcome == "New-Onset DM")

cols_resection <- c(
  "Non-Distal (PD/Total)" = "#2471A3",
  "Distal"                = "#E67E22"
)

fig_3b <- ggplot(cif_resection_df,
                 aes(x = time, y = estimate,
                     color = strata, fill = strata)) +

  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +

  # Gray's test p-value annotation
  annotate("text", x = 5, y = 0.56,
           label = paste0("Gray's test p = ",
                          sprintf("%.4f",
                                  cif_resection$cmprsk$Tests[1, "pv"])),
           hjust = 0, size = 2.8, color = "grey30",
           family = "sans", fontface = "italic") +

  scale_x_continuous(
    limits = c(0, 120),
    breaks = seq(0, 120, 12),
    labels = seq(0, 120, 12),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 0.60),
    breaks = seq(0, 0.60, 0.10),
    labels = scales::percent_format(accuracy = 1),
    name   = "Cumulative Incidence of New-Onset DM"
  ) +
  scale_color_manual(values = cols_resection, name = NULL) +
  scale_fill_manual(values  = cols_resection, name = NULL) +

  labs(
    x        = NULL,
    subtitle = "Stratified by resection type"
  ) +

  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.text.x        = element_blank(),
    axis.text.y        = element_text(color = "black", size = 8),
    axis.title.y       = element_text(color = "black", size = 9,
                                      margin = margin(r = 6)),
    plot.subtitle      = element_text(size = 8, color = "grey40"),
    legend.position    = c(0.25, 0.88),
    legend.background  = element_blank(),
    legend.key         = element_blank(),
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.35, "cm"),
    panel.grid.major.y = element_line(color = "grey93", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(t = 8, r = 8, b = 0, l = 8)
  )

# -----------------------------------------------------------------------------
# 9. FIGURE 3C — CIF by BMI Category (DM only)
# -----------------------------------------------------------------------------

cif_bmi_df <- tidy(cif_bmi) %>%
  filter(outcome == "New-Onset DM")

cols_bmi <- c(
  "Normal"      = "#2471A3",
  "Overweight"  = "#E67E22",
  "Obese"       = "#C0392B",
  "Underweight" = "#AAB7B8"
)

fig_3c <- ggplot(cif_bmi_df,
                 aes(x = time, y = estimate,
                     color = strata, fill = strata)) +

  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +

  annotate("text", x = 5, y = 0.56,
           label = paste0("Gray's test p = ",
                          sprintf("%.3f",
                                  cif_bmi$cmprsk$Tests[1, "pv"])),
           hjust = 0, size = 2.8, color = "grey30",
           family = "sans", fontface = "italic") +

  scale_x_continuous(
    limits = c(0, 120),
    breaks = seq(0, 120, 12),
    labels = seq(0, 120, 12),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 0.60),
    breaks = seq(0, 0.60, 0.10),
    labels = scales::percent_format(accuracy = 1),
    name   = "Cumulative Incidence of New-Onset DM"
  ) +
  scale_color_manual(values = cols_bmi, name = NULL) +
  scale_fill_manual(values  = cols_bmi, name = NULL) +

  labs(
    x        = NULL,
    subtitle = "Stratified by BMI category"
  ) +

  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.text.x        = element_blank(),
    axis.text.y        = element_text(color = "black", size = 8),
    axis.title.y       = element_text(color = "black", size = 9,
                                      margin = margin(r = 6)),
    plot.subtitle      = element_text(size = 8, color = "grey40"),
    legend.position    = c(0.25, 0.88),
    legend.background  = element_blank(),
    legend.key         = element_blank(),
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.35, "cm"),
    panel.grid.major.y = element_line(color = "grey93", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(t = 8, r = 8, b = 0, l = 8)
  )

# -----------------------------------------------------------------------------
# 10. N-AT-RISK PANELS FOR EACH FIGURE
# -----------------------------------------------------------------------------

make_n_risk_panel <- function(cif_obj, outcome_label,
                              strata_cols, strata_order,
                              time_points = seq(0, 120, 12)) {

  # Extract raw data for n-at-risk
  raw_data <- cif_obj$data

  n_risk_rows <- list()

  for (grp in strata_order) {
    grp_data <- raw_data %>%
      filter(if ("strata" %in% names(.)) strata == grp else TRUE)

    for (t in time_points) {
      n <- sum(grp_data$time >= t, na.rm = TRUE)
      n_risk_rows[[length(n_risk_rows) + 1]] <- data.frame(
        strata = grp, time = t, n = n,
        stringsAsFactors = FALSE
      )
    }
  }

  n_risk_df <- bind_rows(n_risk_rows) %>%
    mutate(
      y_pos = as.numeric(factor(strata,
                                levels = rev(strata_order))) - 1
    )

  ggplot(n_risk_df, aes(x = time, y = y_pos,
                        label = n, color = strata)) +
    geom_text(size = 2.5, family = "sans") +
    {
      lapply(seq_along(strata_order), function(i) {
        annotate("text", x = -3, y = length(strata_order) - i,
                 label = strata_order[i],
                 hjust = 1, size = 2.5,
                 color = strata_cols[strata_order[i]],
                 family = "sans", fontface = "bold")
      })
    } +
    scale_x_continuous(
      limits = c(0, 120),
      breaks = seq(0, 120, 12),
      labels = seq(0, 120, 12),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(
      limits = c(-0.6, length(strata_order) - 0.4)
    ) +
    scale_color_manual(values = strata_cols) +
    labs(x = "Time Since Pancreatectomy (Months)", y = NULL) +
    theme_void(base_family = "sans") +
    theme(
      axis.text.x     = element_text(color = "black", size = 7,
                                     margin = margin(t = 3)),
      axis.title.x    = element_text(color = "black", size = 8,
                                     margin = margin(t = 4)),
      legend.position = "none",
      plot.margin     = margin(t = 2, r = 8, b = 8, l = 8)
    )
}

# Overall n-at-risk (single group)
n_overall <- df_tte %>%
  mutate(strata = "Overall") %>%
  {
    rows <- list()
    for (t in seq(0, 120, 12)) {
      rows[[length(rows) + 1]] <- data.frame(
        strata = "Overall",
        time   = t,
        n      = sum(.$time_to_event_months >= t, na.rm = TRUE),
        y_pos  = 0
      )
    }
    bind_rows(rows)
  }

fig_3a_n <- ggplot(n_overall,
                   aes(x = time, y = y_pos, label = n)) +
  geom_text(size = 2.5, family = "sans", color = "grey30") +
  annotate("text", x = -3, y = 0, label = "Overall",
           hjust = 1, size = 2.5, color = "grey30",
           family = "sans", fontface = "bold") +
  scale_x_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, 12),
                     labels = seq(0, 120, 12),
                     expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = c(-0.6, 0.6)) +
  labs(x = "Time Since Pancreatectomy (Months)", y = NULL) +
  theme_void(base_family = "sans") +
  theme(
    axis.text.x  = element_text(color = "black", size = 7,
                                margin = margin(t = 3)),
    axis.title.x = element_text(color = "black", size = 8,
                                margin = margin(t = 4)),
    plot.margin  = margin(t = 2, r = 8, b = 8, l = 8)
  )

# Resection n-at-risk
n_resection <- list()
for (grp in c("Non-Distal (PD/Total)", "Distal")) {
  for (t in seq(0, 120, 12)) {
    n <- sum(df_tte$resection_group == grp &
               df_tte$time_to_event_months >= t, na.rm = TRUE)
    n_resection[[length(n_resection) + 1]] <- data.frame(
      strata = grp, time = t, n = n,
      y_pos = ifelse(grp == "Non-Distal (PD/Total)", 1, 0)
    )
  }
}
n_resection_df <- bind_rows(n_resection)

fig_3b_n <- ggplot(n_resection_df,
                   aes(x = time, y = y_pos,
                       label = n, color = strata)) +
  geom_text(size = 2.5, family = "sans") +
  annotate("text", x = -3, y = 1, label = "Non-Distal",
           hjust = 1, size = 2.5,
           color = cols_resection["Non-Distal (PD/Total)"],
           family = "sans", fontface = "bold") +
  annotate("text", x = -3, y = 0, label = "Distal",
           hjust = 1, size = 2.5,
           color = cols_resection["Distal"],
           family = "sans", fontface = "bold") +
  scale_x_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, 12),
                     labels = seq(0, 120, 12),
                     expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = c(-0.6, 1.6)) +
  scale_color_manual(values = cols_resection) +
  labs(x = "Time Since Pancreatectomy (Months)", y = NULL) +
  theme_void(base_family = "sans") +
  theme(
    axis.text.x     = element_text(color = "black", size = 7,
                                   margin = margin(t = 3)),
    axis.title.x    = element_text(color = "black", size = 8,
                                   margin = margin(t = 4)),
    legend.position = "none",
    plot.margin     = margin(t = 2, r = 8, b = 8, l = 8)
  )

# BMI n-at-risk
bmi_order <- c("Normal", "Overweight", "Obese", "Underweight")
n_bmi <- list()
for (i in seq_along(bmi_order)) {
  grp <- bmi_order[i]
  for (t in seq(0, 120, 12)) {
    n <- sum(df_tte$bmi_cat == grp &
               df_tte$time_to_event_months >= t, na.rm = TRUE)
    n_bmi[[length(n_bmi) + 1]] <- data.frame(
      strata = grp, time = t, n = n,
      y_pos = length(bmi_order) - i
    )
  }
}
n_bmi_df <- bind_rows(n_bmi)

fig_3c_n <- ggplot(n_bmi_df,
                   aes(x = time, y = y_pos,
                       label = n, color = strata)) +
  geom_text(size = 2.5, family = "sans") +
  {
    lapply(seq_along(bmi_order), function(i) {
      annotate("text", x = -3, y = length(bmi_order) - i,
               label = bmi_order[i], hjust = 1, size = 2.5,
               color = cols_bmi[bmi_order[i]],
               family = "sans", fontface = "bold")
    })
  } +
  scale_x_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, 12),
                     labels = seq(0, 120, 12),
                     expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = c(-0.6, length(bmi_order) - 0.4)) +
  scale_color_manual(values = cols_bmi) +
  labs(x = "Time Since Pancreatectomy (Months)", y = NULL) +
  theme_void(base_family = "sans") +
  theme(
    axis.text.x     = element_text(color = "black", size = 7,
                                   margin = margin(t = 3)),
    axis.title.x    = element_text(color = "black", size = 8,
                                   margin = margin(t = 4)),
    legend.position = "none",
    plot.margin     = margin(t = 2, r = 8, b = 8, l = 8)
  )

# -----------------------------------------------------------------------------
# 11. ASSEMBLE COMBINED FIGURE
# -----------------------------------------------------------------------------

fig_3_combined <- (fig_3a / fig_3a_n) |
                  (fig_3b / fig_3b_n) |
                  (fig_3c / fig_3c_n) +
  plot_layout(heights = c(5, 1))

# Add panel labels A, B, C
fig_3_final <- fig_3_combined +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 10, face = "bold",
                              family = "sans")
    )
  )

# -----------------------------------------------------------------------------
# 12. EXPORT FIGURE
# -----------------------------------------------------------------------------

fig_out <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/Endo_Exo_Results/Figures"
dir.create(fig_out, showWarnings = FALSE, recursive = TRUE)

path_png <- file.path(fig_out, "Figure3_CumulativeIncidence_DM.png")
path_pdf <- file.path(fig_out, "Figure3_CumulativeIncidence_DM.pdf")

ggsave(path_png, plot = fig_3_final,
       width = 12, height = 5.5, dpi = 300, bg = "white")
ggsave(path_pdf, plot = fig_3_final,
       width = 12, height = 5.5, device = cairo_pdf, bg = "white")

cat("\u2713 Figure 3 PNG:", path_png, "\n")
cat("\u2713 Figure 3 PDF:", path_pdf, "\n\n")

# -----------------------------------------------------------------------------
# 13. EXPORT FINE-GRAY TABLE AS HTML
# -----------------------------------------------------------------------------

library(knitr)
library(kableExtra)

last_row <- nrow(tbl_fg)

ht_fg <- kable(tbl_fg,
               format   = "html",
               align    = c("l","r","c","c","c"),
               escape   = FALSE,
               caption  = paste0(
                 "<b>Table 8.</b> Fine-Gray Subdistribution Hazard Model ",
                 "for New-Onset Diabetes Mellitus Following Pancreatectomy ",
                 "(N = ", nrow(fg_data), "; Events = ",
                 sum(fg_data$event_type == "New-Onset DM"), ")"
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
  column_spec(1, width = "45%",
              extra_css = "border-left: none !important;
                           border-right: none !important;") %>%
  column_spec(2:5, width = "13%",
              extra_css = "text-align: center !important;
                           border-left: none !important;
                           border-right: none !important;") %>%
  footnote(
    general = paste0(
      "SHR = subdistribution hazard ratio; CI = confidence interval. ",
      "Fine-Gray regression accounts for death as a competing event ",
      "for new-onset diabetes mellitus. ",
      "The SHR represents the instantaneous risk of DM in patients ",
      "who have not yet developed DM, accounting for the competing risk of death. ",
      "* p < 0.05; \u2020 p < 0.10. ",
      "Reference categories: BMI, Normal (18.5-24.9 kg/m\u00b2). ",
      "Patients with pre-operative HbA1c \u22656.5% excluded (n = 11)."
    ),
    general_title     = "Abbreviations and Notes: ",
    footnote_as_chunk = TRUE
  )

html_fg <- paste0(
  "<!DOCTYPE html>\n<html>\n<head>\n",
  "  <meta charset='UTF-8'>\n",
  "  <title>Table 8 - Fine-Gray Model</title>\n",
  "  <style>\n",
  "    body   { font-family: 'Times New Roman', Times, serif;",
  "             margin: 60px 80px; max-width: 820px;",
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
  ht_fg,
  "\n</body>\n</html>"
)

path_tbl8 <- file.path(out_dir,
                       "Tables/Table8_FineGray_DM.html")
writeLines(html_fg, path_tbl8)
cat("\u2713 Table 8 exported to:\n  ", path_tbl8, "\n\n")

# -----------------------------------------------------------------------------
# 14. FIGURE LEGEND TEXT
# -----------------------------------------------------------------------------

cat("=== Suggested figure legend ===\n")
cat(paste0(
  "Figure 3. Cumulative incidence of new-onset diabetes mellitus ",
  "following pancreatectomy accounting for the competing risk of death. ",
  "(A) Overall cohort (N = ", nrow(df_tte), "). ",
  "(B) Stratified by resection type. ",
  "(C) Stratified by body mass index category. ",
  "Shaded areas represent 95% confidence intervals. ",
  "Group differences were assessed using Gray's test. ",
  "Numbers at risk are shown below each panel at 12-month intervals. ",
  "DM = diabetes mellitus; PD = pancreaticoduodenectomy."
), "\n")