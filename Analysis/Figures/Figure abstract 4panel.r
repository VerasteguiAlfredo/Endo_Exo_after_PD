# =============================================================================
# Abstract Figure: 4-Panel Cumulative Incidence
# Panel A: Overall NODM | Panel B: Overall EPI
# Panel C: NODM by BMI  | Panel D: EPI by BMI
# =============================================================================

library(dplyr)
library(ggplot2)
library(patchwork)
library(cmprsk)
library(tidycmprsk)
library(scales)

# --- PATHS (auto-detects Mac vs Windows) -------------------------------------
if (.Platform$OS.type == "windows") {
  base_dir <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD"
} else {
  base_dir <- "/Users/alfredoverastegui/Endo_Exo_after_PD"
}

data_path <- file.path(base_dir, "data", "datamerged_endo_exo_PD_clean.rds")
fig_out   <- file.path(base_dir, "Endo_Exo_Results", "Figures")

cat("Running on:", .Platform$OS.type, "\n")
cat("Data path: ", data_path, "\n")

df <- readRDS(data_path)
dir.create(fig_out, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 1. PREPARE DATASETS
# =============================================================================

# --- DM dataset --------------------------------------------------------------
df_dm <- df %>%
  filter(!is.na(new_onset_dm)) %>%
  filter(is.na(hb_a1c_pre_op) | hb_a1c_pre_op < 6.5) %>%
  mutate(
    time_to_event_months = case_when(
      new_onset_dm == 1         ~ days_to_dm,
      vital_status_fu == "Dead" ~ as.numeric(difftime(vital_fu_dt, dos_pancreatectomy, units = "days")),
      TRUE                      ~ as.numeric(difftime(vital_fu_dt, dos_pancreatectomy, units = "days"))
    ) / 30.44,
    event_type = factor(
      case_when(
        new_onset_dm == 1                             ~ 1L,
        vital_status_fu == "Dead" & new_onset_dm == 0 ~ 2L,
        TRUE                                          ~ 0L
      ),
      levels = c(0, 1, 2),
      labels = c("Censored", "New-Onset DM", "Death without DM")
    ),
    bmi_cat = factor(case_when(
      bmi < 18.5              ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal",
      bmi >= 25   & bmi < 30 ~ "Overweight",
      bmi >= 30               ~ "Obese",
      TRUE ~ NA_character_),
      levels = c("Normal", "Underweight", "Overweight", "Obese"))
  ) %>%
  filter(!is.na(time_to_event_months) & time_to_event_months > 0)

# --- EPI dataset -------------------------------------------------------------
df_exo <- df %>%
  filter(preop_exo_insuff == 0) %>%
  filter(!is.na(exo_insufficiency)) %>%
  mutate(
    time_to_event_months = case_when(
      exo_insufficiency == 1    ~ days_to_exo_insuff,
      vital_status_fu == "Dead" ~ as.numeric(difftime(vital_fu_dt, dos_pancreatectomy, units = "days")),
      TRUE                      ~ as.numeric(difftime(vital_fu_dt, dos_pancreatectomy, units = "days"))
    ) / 30.44,
    event_type = factor(
      case_when(
        exo_insufficiency == 1                              ~ 1L,
        vital_status_fu == "Dead" & exo_insufficiency == 0 ~ 2L,
        TRUE                                               ~ 0L
      ),
      levels = c(0, 1, 2),
      labels = c("Censored", "Exocrine Insufficiency", "Death without Exo Insuff")
    ),
    bmi_cat = factor(case_when(
      bmi < 18.5              ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal",
      bmi >= 25   & bmi < 30 ~ "Overweight",
      bmi >= 30               ~ "Obese",
      TRUE ~ NA_character_),
      levels = c("Normal", "Underweight", "Overweight", "Obese"))
  ) %>%
  filter(!is.na(time_to_event_months) & time_to_event_months > 0)

# =============================================================================
# 2. CIF CALCULATIONS
# =============================================================================

# Overall (no stratification — dummy single-group variable)
df_dm$overall  <- "Overall"
df_exo$overall <- "Overall"

cif_dm_overall  <- tidycmprsk::cuminc(Surv(time_to_event_months, event_type) ~ overall, data = df_dm)
cif_exo_overall <- tidycmprsk::cuminc(Surv(time_to_event_months, event_type) ~ overall, data = df_exo)

# BMI-stratified
df_dm_bmi  <- df_dm  %>% filter(!is.na(bmi_cat))
df_exo_bmi <- df_exo %>% filter(!is.na(bmi_cat))

cif_dm_bmi  <- tidycmprsk::cuminc(Surv(time_to_event_months, event_type) ~ bmi_cat, data = df_dm_bmi)
cif_exo_bmi <- tidycmprsk::cuminc(Surv(time_to_event_months, event_type) ~ bmi_cat, data = df_exo_bmi)

# =============================================================================
# 3. SHARED AESTHETICS
# =============================================================================

col_overall <- "#2C3E50"
col_overall_fill <- "#2C3E50"

bmi_order <- c("Normal", "Underweight", "Overweight", "Obese")
cols_bmi <- c(
  "Normal"      = "#2471A3",
  "Underweight" = "#AAB7B8",
  "Overweight"  = "#E67E22",
  "Obese"       = "#C0392B"
)

format_gray_p <- function(p) {
  if (p < 0.001) return("Gray's P < 0.001")
  sprintf("Gray's P = %.3f", p)
}

x_scale <- scale_x_continuous(
  limits = c(-4, 122),
  breaks = seq(0, 120, 12),
  labels = as.character(seq(0, 120, 12)),
  expand = expansion(mult = c(0, 0.01))
)

theme_cif <- theme_classic(base_size = 10, base_family = "sans") +
  theme(
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.text.x        = element_blank(),
    axis.text.y        = element_text(color = "black", size = 8),
    axis.title.y       = element_text(color = "black", size = 9, margin = margin(r = 6)),
    plot.subtitle      = element_text(size = 9, color = "grey20", face = "bold", margin = margin(b = 4)),
    legend.background  = element_blank(),
    legend.key         = element_blank(),
    legend.text        = element_text(size = 7.5),
    legend.key.size    = unit(0.35, "cm"),
    panel.grid.major.y = element_line(color = "grey93", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(t = 8, r = 8, b = 0, l = 8)
  )

# =============================================================================
# 4. N-AT-RISK HELPERS
# =============================================================================

make_n_strip <- function(data_list, time_points = seq(0, 120, 12)) {
  rows <- list()
  for (item in data_list) {
    for (t in time_points) {
      n <- sum(item$data$time_to_event_months >= t, na.rm = TRUE)
      rows[[length(rows) + 1]] <- data.frame(
        label = item$label, time = t, n = n,
        y_pos = item$y, color = item$color,
        stringsAsFactors = FALSE
      )
    }
  }
  bind_rows(rows)
}

build_n_panel <- function(n_df, x_labels = FALSE) {
  labels_df <- n_df %>% distinct(label, y_pos, color)
  n_groups  <- nrow(labels_df)

  ggplot(n_df, aes(x = time, y = y_pos, label = n, color = color)) +
    geom_text(size = 2.3, family = "sans") +
    {
      lapply(seq_len(n_groups), function(i) {
        annotate("text", x = -4, y = labels_df$y_pos[i],
                 label = labels_df$label[i],
                 hjust = 1, size = 2.2,
                 color = labels_df$color[i],
                 family = "sans", fontface = "bold")
      })
    } +
    scale_x_continuous(
      limits = c(-4, 122),
      breaks = seq(0, 120, 12),
      labels = as.character(seq(0, 120, 12)),
      expand = expansion(mult = c(0, 0.01))
    ) +
    scale_y_continuous(limits = c(min(n_df$y_pos) - 0.5, max(n_df$y_pos) + 0.5)) +
    scale_color_identity() +
    labs(
      x = if (x_labels) "Time Since Pancreatectomy (Months)" else NULL,
      y = NULL
    ) +
    theme_void(base_family = "sans") +
    theme(
      axis.text.x  = if (x_labels) element_text(color = "black", size = 7, margin = margin(t = 2)) else element_blank(),
      axis.title.x = if (x_labels) element_text(color = "black", size = 8, margin = margin(t = 3)) else element_blank(),
      legend.position = "none",
      plot.margin  = margin(t = 1, r = 8, b = 4, l = 60)
    )
}

# =============================================================================
# 5. PANEL A — Overall NODM
# =============================================================================

cif_dm_overall_df <- tidy(cif_dm_overall) %>%
  filter(outcome == "New-Onset DM")

# Median time annotation
median_dm <- min(cif_dm_overall_df$time[cif_dm_overall_df$estimate >= 0.5], na.rm = TRUE)

fig_A <- ggplot(cif_dm_overall_df, aes(x = time, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.12, fill = col_overall, color = NA) +
  geom_line(color = col_overall, linewidth = 0.9) +
  annotate("text", x = 5, y = 0.38,
           label = paste0("10-yr CIF: 25.5% (95% CI 21.6–29.7%)"),
           hjust = 0, size = 2.5, color = "grey30", family = "sans", fontface = "italic") +
  annotate("text", x = 5, y = 0.34,
           label = "Median time to NODM: 24.4 months",
           hjust = 0, size = 2.5, color = "grey30", family = "sans", fontface = "italic") +
  x_scale +
  scale_y_continuous(
    limits = c(0, 0.45),
    breaks = seq(0, 0.40, 0.10),
    labels = percent_format(accuracy = 1),
    name   = "Cumulative Incidence"
  ) +
  labs(x = NULL, subtitle = "A  |  New-Onset Diabetes Mellitus (Overall)") +
  theme_cif

n_dm_overall <- make_n_strip(list(
  list(label = "Overall", data = df_dm, y = 0, color = col_overall)
))
fig_A_n <- build_n_panel(n_dm_overall, x_labels = FALSE)

# =============================================================================
# 6. PANEL B — Overall EPI
# =============================================================================

cif_exo_overall_df <- tidy(cif_exo_overall) %>%
  filter(outcome == "Exocrine Insufficiency")

fig_B <- ggplot(cif_exo_overall_df, aes(x = time, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.12, fill = col_overall, color = NA) +
  geom_line(color = col_overall, linewidth = 0.9) +
  annotate("text", x = 5, y = 0.72,
           label = paste0("10-yr CIF: 46.7% (95% CI 41.9–51.3%)"),
           hjust = 0, size = 2.5, color = "grey30", family = "sans", fontface = "italic") +
  annotate("text", x = 5, y = 0.67,
           label = "Median time to EPI: 5.0 months",
           hjust = 0, size = 2.5, color = "grey30", family = "sans", fontface = "italic") +
  x_scale +
  scale_y_continuous(
    limits = c(0, 0.80),
    breaks = seq(0, 0.80, 0.10),
    labels = percent_format(accuracy = 1),
    name   = "Cumulative Incidence"
  ) +
  labs(x = NULL, subtitle = "B  |  Exocrine Pancreatic Insufficiency (Overall)") +
  theme_cif

n_exo_overall <- make_n_strip(list(
  list(label = "Overall", data = df_exo, y = 0, color = col_overall)
))
fig_B_n <- build_n_panel(n_exo_overall, x_labels = FALSE)

# =============================================================================
# 7. PANEL C — NODM by BMI
# =============================================================================

cif_dm_bmi_df <- tidy(cif_dm_bmi) %>%
  filter(outcome == "New-Onset DM")

fig_C <- ggplot(cif_dm_bmi_df, aes(x = time, y = estimate, color = strata, fill = strata)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +
  annotate("text", x = 5, y = 0.58,
           label = format_gray_p(cif_dm_bmi$cmprsk$Tests[1, "pv"]),
           hjust = 0, size = 2.5, color = "grey30", family = "sans", fontface = "italic") +
  x_scale +
  scale_y_continuous(
    limits = c(0, 0.65),
    breaks = seq(0, 0.60, 0.10),
    labels = percent_format(accuracy = 1),
    name   = "Cumulative Incidence"
  ) +
  scale_color_manual(values = cols_bmi, breaks = bmi_order, name = NULL) +
  scale_fill_manual(values  = cols_bmi, breaks = bmi_order, name = NULL) +
  labs(x = NULL, subtitle = "C  |  New-Onset Diabetes Mellitus by BMI") +
  theme_cif +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.50, 0.94),
    legend.justification   = c("center", "top"),
    legend.direction       = "horizontal"
  )

n_dm_bmi <- make_n_strip(list(
  list(label = "Normal",      data = df_dm_bmi %>% filter(bmi_cat == "Normal"),      y = 3, color = cols_bmi["Normal"]),
  list(label = "Underweight", data = df_dm_bmi %>% filter(bmi_cat == "Underweight"), y = 2, color = cols_bmi["Underweight"]),
  list(label = "Overweight",  data = df_dm_bmi %>% filter(bmi_cat == "Overweight"),  y = 1, color = cols_bmi["Overweight"]),
  list(label = "Obese",       data = df_dm_bmi %>% filter(bmi_cat == "Obese"),       y = 0, color = cols_bmi["Obese"])
))
fig_C_n <- build_n_panel(n_dm_bmi, x_labels = TRUE)

# =============================================================================
# 8. PANEL D — EPI by BMI
# =============================================================================

cif_exo_bmi_df <- tidy(cif_exo_bmi) %>%
  filter(outcome == "Exocrine Insufficiency")

fig_D <- ggplot(cif_exo_bmi_df, aes(x = time, y = estimate, color = strata, fill = strata)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +
  annotate("text", x = 5, y = 0.82,
           label = format_gray_p(cif_exo_bmi$cmprsk$Tests[1, "pv"]),
           hjust = 0, size = 2.5, color = "grey30", family = "sans", fontface = "italic") +
  x_scale +
  scale_y_continuous(
    limits = c(0, 0.90),
    breaks = seq(0, 0.90, 0.10),
    labels = percent_format(accuracy = 1),
    name   = "Cumulative Incidence"
  ) +
  scale_color_manual(values = cols_bmi, breaks = bmi_order, name = NULL) +
  scale_fill_manual(values  = cols_bmi, breaks = bmi_order, name = NULL) +
  labs(x = NULL, subtitle = "D  |  Exocrine Pancreatic Insufficiency by BMI") +
  theme_cif +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.50, 0.94),
    legend.justification   = c("center", "top"),
    legend.direction       = "horizontal"
  )

n_exo_bmi <- make_n_strip(list(
  list(label = "Normal",      data = df_exo_bmi %>% filter(bmi_cat == "Normal"),      y = 3, color = cols_bmi["Normal"]),
  list(label = "Underweight", data = df_exo_bmi %>% filter(bmi_cat == "Underweight"), y = 2, color = cols_bmi["Underweight"]),
  list(label = "Overweight",  data = df_exo_bmi %>% filter(bmi_cat == "Overweight"),  y = 1, color = cols_bmi["Overweight"]),
  list(label = "Obese",       data = df_exo_bmi %>% filter(bmi_cat == "Obese"),       y = 0, color = cols_bmi["Obese"])
))
fig_D_n <- build_n_panel(n_exo_bmi, x_labels = TRUE)

# =============================================================================
# 9. ASSEMBLE 4-PANEL LAYOUT
# =============================================================================

# Build each column as a self-contained patchwork first,
# then combine columns side by side using wrap_plots.
# This avoids patchwork's ambiguous nesting with mixed / and | operators.

panel_A <- fig_A / fig_A_n + plot_layout(heights = c(5, 0.8))
panel_B <- fig_B / fig_B_n + plot_layout(heights = c(5, 0.8))
panel_C <- fig_C / fig_C_n + plot_layout(heights = c(5, 2.2))
panel_D <- fig_D / fig_D_n + plot_layout(heights = c(5, 2.2))

col1 <- wrap_plots(panel_A, panel_C, ncol = 1, heights = c(1, 1.4))
col2 <- wrap_plots(panel_B, panel_D, ncol = 1, heights = c(1, 1.4))

fig_abstract <- wrap_plots(col1, col2, ncol = 2) +
  plot_annotation(
    title   = "Cumulative Incidence of Endocrine and Exocrine Dysfunction After Partial Pancreatectomy",
    caption = "Competing risk of death accounted for using the Fine-Gray framework. Shaded areas represent 95% confidence intervals. Numbers at risk shown at 12-month intervals.",
    theme   = theme(
      plot.title   = element_text(size = 11, face = "bold", hjust = 0.5, margin = margin(b = 6)),
      plot.caption = element_text(size = 7,  color = "grey40", hjust = 0.5, margin = margin(t = 6))
    )
  )

# =============================================================================
# 10. EXPORT
# =============================================================================

path_png <- file.path(fig_out, "Figure_Abstract_4panel_CIF.png")

ggsave(path_png,
       plot   = fig_abstract,
       width  = 14,
       height = 12,
       dpi    = 600,
       bg     = "white")

cat("\u2713 Abstract figure exported to:\n  ", path_png, "\n")

# =============================================================================
# 11. FIGURE LEGEND
# =============================================================================

cat("\n=== Suggested figure legend ===\n")
cat(paste0(
  "Figure. Cumulative incidence of new-onset diabetes mellitus (NODM) and ",
  "exocrine pancreatic insufficiency (EPI) following partial pancreatectomy, ",
  "accounting for the competing risk of death. ",
  "Panels A and B show overall cumulative incidence for NODM (N = ", nrow(df_dm), ") ",
  "and EPI (N = ", nrow(df_exo), "), respectively, with median time to event and ",
  "10-year cumulative incidence annotated. ",
  "Panels C and D show cumulative incidence stratified by body mass index category ",
  "for NODM and EPI, respectively. ",
  "Group differences were assessed using Gray's test. ",
  "Shaded areas represent 95% confidence intervals. ",
  "Numbers at risk are shown below each panel at 12-month intervals. ",
  "Patients with pre-operative HbA1c >= 6.5% were excluded from NODM analyses (n = 11). ",
  "Patients with pre-operative exocrine insufficiency were excluded from EPI analyses (n = 123). ",
  "BMI categories: Normal 18.5-24.9 kg/m2; Underweight <18.5 kg/m2; ",
  "Overweight 25.0-29.9 kg/m2; Obese >= 30.0 kg/m2."
), "\n")