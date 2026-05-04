# =============================================================================
# Figure 8: Cumulative Incidence Stratified by BMI Category
# Panel A: New-Onset DM | Panel B: Exocrine Insufficiency
# =============================================================================

library(dplyr)
library(ggplot2)
library(patchwork)
library(cmprsk)
library(tidycmprsk)
library(scales)

df <- readRDS("C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/datamerged_endo_exo_PD_clean.rds")

fig_out <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/Endo_Exo_Results/Figures"
dir.create(fig_out, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. PREPARE DM DATASET (Panel A)
# -----------------------------------------------------------------------------

df_dm <- df %>%
  filter(!is.na(new_onset_dm)) %>%
  filter(is.na(hb_a1c_pre_op) | hb_a1c_pre_op < 6.5) %>%
  mutate(

    time_to_event_days = case_when(
      new_onset_dm == 1         ~ days_to_dm,
      vital_status_fu == "Dead" ~ as.numeric(
        difftime(vital_fu_dt, dos_pancreatectomy, units = "days")),
      TRUE                      ~ as.numeric(
        difftime(vital_fu_dt, dos_pancreatectomy, units = "days"))
    ),
    time_to_event_months = time_to_event_days / 30.44,

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
  filter(!is.na(time_to_event_days) & time_to_event_days > 0 &
           !is.na(bmi_cat))

# -----------------------------------------------------------------------------
# 2. PREPARE EXO DATASET (Panel B)
# -----------------------------------------------------------------------------

df_exo <- df %>%
  filter(preop_exo_insuff == 0) %>%
  filter(!is.na(exo_insufficiency)) %>%
  mutate(

    time_to_event_days = case_when(
      exo_insufficiency == 1    ~ days_to_exo_insuff,
      vital_status_fu == "Dead" ~ as.numeric(
        difftime(vital_fu_dt, dos_pancreatectomy, units = "days")),
      TRUE                      ~ as.numeric(
        difftime(vital_fu_dt, dos_pancreatectomy, units = "days"))
    ),
    time_to_event_months = time_to_event_days / 30.44,

    event_type = factor(
      case_when(
        exo_insufficiency == 1                              ~ 1L,
        vital_status_fu == "Dead" & exo_insufficiency == 0 ~ 2L,
        TRUE                                               ~ 0L
      ),
      levels = c(0, 1, 2),
      labels = c("Censored", "Exocrine Insufficiency",
                 "Death without Exo Insuff")
    ),

    bmi_cat = factor(case_when(
      bmi < 18.5              ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal",
      bmi >= 25   & bmi < 30 ~ "Overweight",
      bmi >= 30               ~ "Obese",
      TRUE ~ NA_character_),
      levels = c("Normal", "Underweight", "Overweight", "Obese"))

  ) %>%
  filter(!is.na(time_to_event_days) & time_to_event_days > 0 &
           !is.na(bmi_cat))

# -----------------------------------------------------------------------------
# 3. CIF CALCULATIONS
# -----------------------------------------------------------------------------

cif_dm_bmi  <- tidycmprsk::cuminc(
  Surv(time_to_event_months, event_type) ~ bmi_cat,
  data = df_dm)

cif_exo_bmi <- tidycmprsk::cuminc(
  Surv(time_to_event_months, event_type) ~ bmi_cat,
  data = df_exo)

# Print key time points
cat("=== DM CIF by BMI at key time points ===\n")
print(tidy(cif_dm_bmi, times = c(12, 24, 36, 60, 120)) %>%
        filter(outcome == "New-Onset DM") %>%
        select(time, strata, estimate, conf.low, conf.high))
cat("Gray's test (DM ~ BMI):\n")
print(cif_dm_bmi$cmprsk$Tests)

cat("\n=== Exo CIF by BMI at key time points ===\n")
print(tidy(cif_exo_bmi, times = c(12, 24, 36, 60, 120)) %>%
        filter(outcome == "Exocrine Insufficiency") %>%
        select(time, strata, estimate, conf.low, conf.high))
cat("Gray's test (Exo ~ BMI):\n")
print(cif_exo_bmi$cmprsk$Tests)

# -----------------------------------------------------------------------------
# 4. SHARED SETTINGS
# -----------------------------------------------------------------------------

bmi_order  <- c("Normal", "Underweight", "Overweight", "Obese")

cols_bmi <- c(
  "Normal"      = "#2471A3",
  "Underweight" = "#AAB7B8",
  "Overweight"  = "#E67E22",
  "Obese"       = "#C0392B"
)

format_gray_p <- function(p) {
  if (p < 0.001) return("Gray's p < 0.001")
  sprintf("Gray's p = %.3f", p)
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
    axis.title.y       = element_text(color = "black", size = 9,
                                      margin = margin(r = 6)),
    plot.subtitle      = element_text(size = 9, color = "grey30",
                                      face = "bold",
                                      margin = margin(b = 4)),
    legend.background  = element_blank(),
    legend.key         = element_blank(),
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.35, "cm"),
    panel.grid.major.y = element_line(color = "grey93", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(t = 8, r = 8, b = 0, l = 8)
  )

# -----------------------------------------------------------------------------
# 5. N-AT-RISK HELPER
# -----------------------------------------------------------------------------

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

build_n_panel <- function(n_df, x_labels = TRUE) {
  labels_df <- n_df %>% distinct(label, y_pos, color)
  n_groups  <- nrow(labels_df)

  ggplot(n_df, aes(x = time, y = y_pos, label = n, color = color)) +
    geom_text(size = 2.4, family = "sans") +
    {
      lapply(seq_len(n_groups), function(i) {
        annotate("text", x = -4, y = labels_df$y_pos[i],
                 label = labels_df$label[i],
                 hjust = 1, size = 2.3,
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
    scale_y_continuous(
      limits = c(min(n_df$y_pos) - 0.5,
                 max(n_df$y_pos) + 0.5)
    ) +
    scale_color_identity() +
    labs(
      x = if (x_labels) "Time Since Pancreatectomy (Months)" else NULL,
      y = NULL
    ) +
    theme_void(base_family = "sans") +
    theme(
      axis.text.x     = if (x_labels)
        element_text(color = "black", size = 7, margin = margin(t = 2))
      else element_blank(),
      axis.title.x    = if (x_labels)
        element_text(color = "black", size = 8, margin = margin(t = 3))
      else element_blank(),
      legend.position = "none",
      plot.margin     = margin(t = 1, r = 8, b = 4, l = 60)
    )
}

# -----------------------------------------------------------------------------
# 6. PANEL A — DM by BMI
# -----------------------------------------------------------------------------

cif_dm_bmi_df <- tidy(cif_dm_bmi) %>%
  filter(outcome == "New-Onset DM")

fig_8a <- ggplot(cif_dm_bmi_df,
                 aes(x = time, y = estimate,
                     color = strata, fill = strata)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +
  annotate("text", x = 5, y = 0.50,
           label = format_gray_p(cif_dm_bmi$cmprsk$Tests[1, "pv"]),
           hjust = 0, size = 2.6, color = "grey30",
           family = "sans", fontface = "italic") +
  x_scale +
  scale_y_continuous(
    limits = c(0, 0.60),
    breaks = seq(0, 0.60, 0.10),
    labels = percent_format(accuracy = 1),
    name   = "Cumulative Incidence"
  ) +
  scale_color_manual(values = cols_bmi, breaks = bmi_order, name = NULL) +
  scale_fill_manual(values  = cols_bmi, breaks = bmi_order, name = NULL) +
  labs(x = NULL,
       subtitle = "A  |  New-Onset Diabetes Mellitus") +
  theme_cif +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.50, 0.92),
    legend.justification   = c("center", "top"),
    legend.direction       = "horizontal"
  )

# -----------------------------------------------------------------------------
# 7. PANEL B — Exo by BMI
# -----------------------------------------------------------------------------

cif_exo_bmi_df <- tidy(cif_exo_bmi) %>%
  filter(outcome == "Exocrine Insufficiency")

fig_8b <- ggplot(cif_exo_bmi_df,
                 aes(x = time, y = estimate,
                     color = strata, fill = strata)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +
  annotate("text", x = 5, y = 0.65,
           label = format_gray_p(cif_exo_bmi$cmprsk$Tests[1, "pv"]),
           hjust = 0, size = 2.6, color = "grey30",
           family = "sans", fontface = "italic") +
  x_scale +
  scale_y_continuous(
    limits = c(0, 0.75),
    breaks = seq(0, 0.75, 0.10),
    labels = percent_format(accuracy = 1),
    name   = "Cumulative Incidence"
  ) +
  scale_color_manual(values = cols_bmi, breaks = bmi_order, name = NULL) +
  scale_fill_manual(values  = cols_bmi, breaks = bmi_order, name = NULL) +
  labs(x = NULL,
       subtitle = "B  |  Exocrine Insufficiency") +
  theme_cif +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.50, 0.92),
    legend.justification   = c("center", "top"),
    legend.direction       = "horizontal"
  )

# -----------------------------------------------------------------------------
# 8. N-AT-RISK PANELS
# -----------------------------------------------------------------------------

# Panel A — DM, four BMI groups
n_dm_bmi <- make_n_strip(list(
  list(label = "Normal",
       data  = df_dm %>% filter(bmi_cat == "Normal"),
       y = 3, color = cols_bmi["Normal"]),
  list(label = "Underweight",
       data  = df_dm %>% filter(bmi_cat == "Underweight"),
       y = 2, color = cols_bmi["Underweight"]),
  list(label = "Overweight",
       data  = df_dm %>% filter(bmi_cat == "Overweight"),
       y = 1, color = cols_bmi["Overweight"]),
  list(label = "Obese",
       data  = df_dm %>% filter(bmi_cat == "Obese"),
       y = 0, color = cols_bmi["Obese"])
))
fig_8a_n <- build_n_panel(n_dm_bmi, x_labels = TRUE)

# Panel B — Exo, four BMI groups
n_exo_bmi <- make_n_strip(list(
  list(label = "Normal",
       data  = df_exo %>% filter(bmi_cat == "Normal"),
       y = 3, color = cols_bmi["Normal"]),
  list(label = "Underweight",
       data  = df_exo %>% filter(bmi_cat == "Underweight"),
       y = 2, color = cols_bmi["Underweight"]),
  list(label = "Overweight",
       data  = df_exo %>% filter(bmi_cat == "Overweight"),
       y = 1, color = cols_bmi["Overweight"]),
  list(label = "Obese",
       data  = df_exo %>% filter(bmi_cat == "Obese"),
       y = 0, color = cols_bmi["Obese"])
))
fig_8b_n <- build_n_panel(n_exo_bmi, x_labels = TRUE)

# -----------------------------------------------------------------------------
# 9. ASSEMBLE SIDE-BY-SIDE LAYOUT
# -----------------------------------------------------------------------------

fig_8_combined <-
  (fig_8a / fig_8a_n + plot_layout(heights = c(5, 1.8))) |
  (fig_8b / fig_8b_n + plot_layout(heights = c(5, 1.8)))

# -----------------------------------------------------------------------------
# 10. EXPORT
# -----------------------------------------------------------------------------

path_png <- file.path(fig_out,
  "Figure8_CumulativeIncidence_BMI_DM_Exo.png")
path_pdf <- file.path(fig_out,
  "Figure8_CumulativeIncidence_BMI_DM_Exo.pdf")

ggsave(path_png, plot = fig_8_combined,
       width = 12, height = 6, dpi = 600, bg = "white")
ggsave(path_pdf, plot = fig_8_combined,
       width = 12, height = 6, device = cairo_pdf, bg = "white")

cat("\u2713 Figure 8 PNG:", path_png, "\n")
cat("\u2713 Figure 8 PDF:", path_pdf, "\n\n")

# -----------------------------------------------------------------------------
# 11. FIGURE LEGEND
# -----------------------------------------------------------------------------

cat("=== Suggested figure legend ===\n")
cat(paste0(
  "Figure 8. Cumulative incidence of pancreatic endocrine and exocrine ",
  "dysfunction following pancreatectomy, stratified by body mass index ",
  "category, accounting for the competing risk of death. ",
  "(A) New-onset diabetes mellitus (N = ", nrow(df_dm), "). ",
  "(B) Post-operative exocrine insufficiency (N = ", nrow(df_exo), "; ",
  "patients with pre-operative exocrine insufficiency excluded). ",
  "Shaded areas represent 95% confidence intervals. ",
  "Group differences were assessed using Gray's test. ",
  "Numbers at risk are shown below each panel at 12-month intervals ",
  "for each BMI category. ",
  "Patients with pre-operative HbA1c \u22656.5% were excluded from ",
  "Panel A (n = 11). ",
  "BMI categories: Normal, 18.5-24.9 kg/m\u00b2; ",
  "Underweight, <18.5 kg/m\u00b2; ",
  "Overweight, 25.0-29.9 kg/m\u00b2; ",
  "Obese, \u226530.0 kg/m\u00b2. ",
  "DM = diabetes mellitus."
), "\n")