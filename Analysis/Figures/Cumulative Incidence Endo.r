# =============================================================================
# Figure 3: Cumulative Incidence of New-Onset DM — Competing Risks Analysis
# Panels: A=Overall, B=Resection type, C=Adenocarcinoma, D=NAT
# =============================================================================

library(dplyr)
library(ggplot2)
library(patchwork)
library(cmprsk)
library(tidycmprsk)
library(knitr)
library(kableExtra)
library(scales)

df <- readRDS("C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/datamerged_endo_exo_PD_clean.rds")

fig_out <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/Endo_Exo_Results/Figures"
tbl_out <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/Endo_Exo_Results/Tables"
dir.create(fig_out, showWarnings = FALSE, recursive = TRUE)
dir.create(tbl_out, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. PREPARE TIME-TO-EVENT DATASET
# -----------------------------------------------------------------------------

df_tte <- df %>%
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

    resection_group = factor(
      case_when(
        resection_type == "Distal" ~ "Distal",
        TRUE                       ~ "Non-Distal (PD/Total)"
      ),
      levels = c("Non-Distal (PD/Total)", "Distal")
    ),

    adenocarcinoma = as.integer(
      path_dx1_sub == "Adenocarcinoma (Panc/CBD/Ampulla/Duodenum)"),
    adenocarcinoma_group = factor(
      case_when(
        adenocarcinoma == 1 ~ "Adenocarcinoma",
        adenocarcinoma == 0 ~ "Non-Adenocarcinoma"
      ),
      levels = c("Non-Adenocarcinoma", "Adenocarcinoma")
    ),

    nat_binary = as.integer(!is.na(nat) & nat == 1),
    nat_group = factor(
      case_when(
        nat_binary == 1 ~ "Neoadjuvant Therapy",
        nat_binary == 0 ~ "No Neoadjuvant Therapy"
      ),
      levels = c("No Neoadjuvant Therapy", "Neoadjuvant Therapy")
    )
  ) %>%
  filter(!is.na(time_to_event_days) & time_to_event_days > 0)

cat("=== New-onset DM TTE dataset ===\n")
cat("Patients:", nrow(df_tte), "\n")
cat("\nEvent type distribution:\n")
print(table(df_tte$event_type, useNA = "ifany"))
cat("\nMedian follow-up (months):",
    round(median(df_tte$time_to_event_months, na.rm = TRUE), 1), "\n")
cat("Max follow-up (months):",
    round(max(df_tte$time_to_event_months, na.rm = TRUE), 1), "\n\n")

# -----------------------------------------------------------------------------
# 2. CIF CALCULATIONS
# -----------------------------------------------------------------------------

cif_overall   <- tidycmprsk::cuminc(
  Surv(time_to_event_months, event_type) ~ 1,
  data = df_tte)

cif_resection <- tidycmprsk::cuminc(
  Surv(time_to_event_months, event_type) ~ resection_group,
  data = df_tte)

cif_adeno     <- tidycmprsk::cuminc(
  Surv(time_to_event_months, event_type) ~ adenocarcinoma_group,
  data = df_tte)

cif_nat       <- tidycmprsk::cuminc(
  Surv(time_to_event_months, event_type) ~ nat_group,
  data = df_tte)

# Print key time points
cat("=== Overall CIF at key time points ===\n")
print(tidy(cif_overall, times = c(12, 24, 36, 60, 120)) %>%
        filter(outcome == "New-Onset DM") %>%
        select(time, estimate, conf.low, conf.high))

cat("\n=== CIF by resection type ===\n")
print(tidy(cif_resection, times = c(12, 24, 36, 60, 120)) %>%
        filter(outcome == "New-Onset DM") %>%
        select(time, strata, estimate, conf.low, conf.high))
cat("Gray's test:\n"); print(cif_resection$cmprsk$Tests)

cat("\n=== CIF by adenocarcinoma ===\n")
print(tidy(cif_adeno, times = c(12, 24, 36, 60, 120)) %>%
        filter(outcome == "New-Onset DM") %>%
        select(time, strata, estimate, conf.low, conf.high))
cat("Gray's test:\n"); print(cif_adeno$cmprsk$Tests)

cat("\n=== CIF by NAT ===\n")
print(tidy(cif_nat, times = c(12, 24, 36, 60, 120)) %>%
        filter(outcome == "New-Onset DM") %>%
        select(time, strata, estimate, conf.low, conf.high))
cat("Gray's test:\n"); print(cif_nat$cmprsk$Tests)

# -----------------------------------------------------------------------------
# 3. FINE-GRAY MODEL
# -----------------------------------------------------------------------------

cat("\n=== Fine-Gray Model: New-Onset DM ===\n")

fg_data <- df_tte %>%
  filter(!is.na(resection_group) & !is.na(adenocarcinoma) &
           !is.na(nat_binary) & !is.na(family_hx_dm)) %>%
  mutate(
    distal          = as.integer(resection_group == "Distal"),
    bmi_overweight  = as.integer(bmi >= 25 & bmi < 30),
    bmi_obese       = as.integer(bmi >= 30),
    bmi_underweight = as.integer(bmi < 18.5)
  )

cov_matrix <- as.matrix(fg_data %>%
  select(bmi_overweight, bmi_obese, bmi_underweight,
         distal, adenocarcinoma, nat_binary, family_hx_dm))

fg_fit <- cmprsk::crr(
  ftime    = fg_data$time_to_event_months,
  fstatus  = as.integer(fg_data$event_type) - 1,
  cov1     = cov_matrix,
  failcode = 1,
  cencode  = 0
)

cat("Fine-Gray model summary:\n")
print(summary(fg_fit))

# Extract results table
fg_coefs <- summary(fg_fit)$coef
fg_ci    <- summary(fg_fit)$conf.int

var_labels_fg <- c(
  "bmi_overweight"  = "BMI: Overweight vs Normal",
  "bmi_obese"       = "BMI: Obese vs Normal",
  "bmi_underweight" = "BMI: Underweight vs Normal",
  "distal"          = "Distal pancreatectomy",
  "adenocarcinoma"  = "Adenocarcinoma diagnosis",
  "nat_binary"      = "Neoadjuvant therapy",
  "family_hx_dm"    = "Family history of DM"
)

fg_rows <- list()
for (nm in rownames(fg_coefs)) {
  shr   <- exp(fg_coefs[nm, "coef"])
  p_val <- fg_coefs[nm, "p-value"]
  ci_lo <- fg_ci[nm, "2.5%"]
  ci_hi <- fg_ci[nm, "97.5%"]

  p_str <- if (p_val < 0.001)      "<0.001 *"
            else if (p_val < 0.05) paste0(sprintf("%.3f", p_val), " *")
            else if (p_val < 0.10) paste0(sprintf("%.3f", p_val), " \u2020")
            else                   sprintf("%.3f", p_val)

  label <- ifelse(nm %in% names(var_labels_fg), var_labels_fg[nm], nm)

  fg_rows[[length(fg_rows) + 1]] <- data.frame(
    Variable = label,
    N        = ifelse(length(fg_rows) == 0,
                      as.character(nrow(fg_data)), ""),
    SHR      = sprintf("%.2f", shr),
    CI       = sprintf("%.2f\u2013%.2f", ci_lo, ci_hi),
    P        = p_str,
    stringsAsFactors = FALSE
  )
}

tbl_fg <- bind_rows(fg_rows)
colnames(tbl_fg) <- c("Variable", "N", "SHR", "95% CI", "P-Value")

cat("\n=== Fine-Gray results table ===\n")
print(kable(tbl_fg, format = "markdown", align = c("l","r","c","c","c")))

# -----------------------------------------------------------------------------
# 4. HELPER FUNCTIONS
# -----------------------------------------------------------------------------

format_gray_p <- function(p) {
  if (p < 0.001) return("Gray's p < 0.001")
  sprintf("Gray's p = %.3f", p)
}

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
      limits = c(min(n_df$y_pos) - 0.4,
                 max(n_df$y_pos) + 0.4)
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
# 5. SHARED THEME AND SCALES
# -----------------------------------------------------------------------------

theme_cif <- theme_classic(base_size = 10, base_family = "sans") +
  theme(
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.text.x        = element_blank(),
    axis.text.y        = element_text(color = "black", size = 8),
    axis.title.y       = element_text(color = "black", size = 9,
                                      margin = margin(r = 6)),
    plot.subtitle      = element_text(size = 8, color = "grey40",
                                      margin = margin(b = 4)),
    legend.background  = element_blank(),
    legend.key         = element_blank(),
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.35, "cm"),
    panel.grid.major.y = element_line(color = "grey93", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(t = 8, r = 8, b = 0, l = 8)
  )

x_scale <- scale_x_continuous(
  limits = c(-4, 122),
  breaks = seq(0, 120, 12),
  labels = as.character(seq(0, 120, 12)),
  expand = expansion(mult = c(0, 0.01))
)

y_scale_dm <- scale_y_continuous(
  limits = c(0, 0.60),
  breaks = seq(0, 0.60, 0.10),
  labels = percent_format(accuracy = 1),
  name   = "Cumulative Incidence"
)

# -----------------------------------------------------------------------------
# 6. COLOR PALETTES
# -----------------------------------------------------------------------------

cols_events <- c(
  "New-Onset DM"     = "#C0392B",
  "Death without DM" = "#2471A3"
)

cols_res <- c(
  "Non-Distal (PD/Total)" = "#2471A3",
  "Distal"                = "#E67E22"
)

cols_adeno <- c(
  "Non-Adenocarcinoma" = "#2471A3",
  "Adenocarcinoma"     = "#C0392B"
)

cols_nat <- c(
  "No Neoadjuvant Therapy" = "#2471A3",
  "Neoadjuvant Therapy"    = "#8E44AD"
)

# -----------------------------------------------------------------------------
# 7. PANEL A — Overall CIF (DM + Death)
# -----------------------------------------------------------------------------

fig_3a <- ggplot(cif_overall_df,
                 aes(x = time, y = estimate,
                     color = outcome, fill = outcome)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +
  x_scale + y_scale_dm +
  scale_color_manual(values = cols_events, name = NULL) +
  scale_fill_manual(values  = cols_events, name = NULL) +
  labs(x = NULL, subtitle = "A  |  Overall cohort") +
  theme_cif +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.50, 0.92),
    legend.justification   = c("center", "top"),
    legend.direction       = "horizontal"
  )

# -----------------------------------------------------------------------------
# 8. PANEL B — By Resection Type
# -----------------------------------------------------------------------------

fig_3b <- ggplot(cif_res_df,
                 aes(x = time, y = estimate,
                     color = strata, fill = strata)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +
  annotate("text", x = 5, y = 0.50,          # moved down from 0.56
           label = format_gray_p(cif_resection$cmprsk$Tests[1, "pv"]),
           hjust = 0, size = 2.6, color = "grey30",
           family = "sans", fontface = "italic") +
  x_scale + y_scale_dm +
  scale_color_manual(values = cols_res, name = NULL) +
  scale_fill_manual(values  = cols_res, name = NULL) +
  labs(x = NULL, subtitle = "B  |  Stratified by resection type") +
  theme_cif +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.50, 0.92),
    legend.justification   = c("center", "top"),
    legend.direction       = "horizontal",
    axis.title.y           = element_blank(),
    axis.text.y            = element_blank(),
    axis.ticks.y           = element_blank(),
    axis.line.y            = element_blank()
  )

# -----------------------------------------------------------------------------
# 9. PANEL C — By Adenocarcinoma
# -----------------------------------------------------------------------------

fig_3c <- ggplot(cif_adeno_df,
                 aes(x = time, y = estimate,
                     color = strata, fill = strata)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +
  annotate("text", x = 5, y = 0.50,          # moved down from 0.56
           label = format_gray_p(cif_adeno$cmprsk$Tests[1, "pv"]),
           hjust = 0, size = 2.6, color = "grey30",
           family = "sans", fontface = "italic") +
  x_scale + y_scale_dm +
  scale_color_manual(values = cols_adeno, name = NULL) +
  scale_fill_manual(values  = cols_adeno, name = NULL) +
  labs(x = NULL, subtitle = "C  |  Stratified by pathologic diagnosis") +
  theme_cif +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.50, 0.92),
    legend.justification   = c("center", "top"),
    legend.direction       = "horizontal"
  )

# -----------------------------------------------------------------------------
# 10. PANEL D — By NAT
# -----------------------------------------------------------------------------

fig_3d <- ggplot(cif_nat_df,
                 aes(x = time, y = estimate,
                     color = strata, fill = strata)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +
  annotate("text", x = 5, y = 0.50,          # moved down from 0.56
           label = format_gray_p(cif_nat$cmprsk$Tests[1, "pv"]),
           hjust = 0, size = 2.6, color = "grey30",
           family = "sans", fontface = "italic") +
  x_scale + y_scale_dm +
  scale_color_manual(values = cols_nat, name = NULL) +
  scale_fill_manual(values  = cols_nat, name = NULL) +
  labs(x = NULL, subtitle = "D  |  Stratified by neoadjuvant therapy") +
  theme_cif +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.50, 0.92),
    legend.justification   = c("center", "top"),
    legend.direction       = "horizontal",
    axis.title.y           = element_blank(),
    axis.text.y            = element_blank(),
    axis.ticks.y           = element_blank(),
    axis.line.y            = element_blank()
  )

# -----------------------------------------------------------------------------
# 11. N-AT-RISK PANELS
# -----------------------------------------------------------------------------

# Panel A — two rows: DM + Death
n_overall_data <- make_n_strip(list(
  list(label = "New-Onset DM",
       data  = df_tte,
       y     = 1,
       color = cols_events["New-Onset DM"]),
  list(label = "Death w/o DM",
       data  = df_tte,
       y     = 0,
       color = cols_events["Death without DM"])
))
fig_3a_n <- build_n_panel(n_overall_data, x_labels = TRUE)

# Panel B — resection type
n_res_data <- make_n_strip(list(
  list(label = "Non-Distal",
       data  = df_tte %>% filter(resection_group == "Non-Distal (PD/Total)"),
       y     = 1,
       color = cols_res["Non-Distal (PD/Total)"]),
  list(label = "Distal",
       data  = df_tte %>% filter(resection_group == "Distal"),
       y     = 0,
       color = cols_res["Distal"])
))
fig_3b_n <- build_n_panel(n_res_data, x_labels = TRUE)

# Panel C — adenocarcinoma
n_adeno_data <- make_n_strip(list(
  list(label = "Non-Adeno",
       data  = df_tte %>% filter(adenocarcinoma == 0),
       y     = 1,
       color = cols_adeno["Non-Adenocarcinoma"]),
  list(label = "Adeno",
       data  = df_tte %>% filter(adenocarcinoma == 1),
       y     = 0,
       color = cols_adeno["Adenocarcinoma"])
))
fig_3c_n <- build_n_panel(n_adeno_data, x_labels = TRUE)

# Panel D — NAT
n_nat_data <- make_n_strip(list(
  list(label = "No NAT",
       data  = df_tte %>% filter(nat_binary == 0),
       y     = 1,
       color = cols_nat["No Neoadjuvant Therapy"]),
  list(label = "NAT",
       data  = df_tte %>% filter(nat_binary == 1),
       y     = 0,
       color = cols_nat["Neoadjuvant Therapy"])
))
fig_3d_n <- build_n_panel(n_nat_data, x_labels = TRUE)

# -----------------------------------------------------------------------------
# 12. ASSEMBLE 2x2 LAYOUT
# -----------------------------------------------------------------------------

fig_3_combined <- (
  (fig_3a / fig_3a_n + plot_layout(heights = c(5, 1.2))) |
  (fig_3b / fig_3b_n + plot_layout(heights = c(5, 1.2)))
) /
(
  (fig_3c / fig_3c_n + plot_layout(heights = c(5, 1.2))) |
  (fig_3d / fig_3d_n + plot_layout(heights = c(5, 1.2)))
)

# -----------------------------------------------------------------------------
# 13. EXPORT FIGURE
# -----------------------------------------------------------------------------

path_png <- file.path(fig_out, "Figure3_CumulativeIncidence_DM.png")
path_pdf <- file.path(fig_out, "Figure3_CumulativeIncidence_DM.pdf")

ggsave(path_png, plot = fig_3_combined,
       width = 12, height = 10, dpi = 600, bg = "white")
ggsave(path_pdf, plot = fig_3_combined,
       width = 12, height = 10, device = cairo_pdf, bg = "white")

cat("\u2713 Figure 3 PNG:", path_png, "\n")
cat("\u2713 Figure 3 PDF:", path_pdf, "\n\n")

# -----------------------------------------------------------------------------
# 14. EXPORT FINE-GRAY TABLE AS HTML
# -----------------------------------------------------------------------------

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
      "* p < 0.05; \u2020 p < 0.10. ",
      "Reference category: BMI, Normal (18.5-24.9 kg/m\u00b2). ",
      "Patients with pre-operative HbA1c \u22656.5% excluded (n = 11)."
    ),
    general_title     = "Abbreviations and Notes: ",
    footnote_as_chunk = TRUE
  )

html_fg <- paste0(
  "<!DOCTYPE html>\n<html>\n<head>\n",
  "  <meta charset='UTF-8'>\n",
  "  <title>Table 8 - Fine-Gray DM</title>\n",
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

path_tbl8 <- file.path(tbl_out, "Table8_FineGray_DM.html")
writeLines(html_fg, path_tbl8)
cat("\u2713 Table 8 exported to:\n  ", path_tbl8, "\n\n")

# -----------------------------------------------------------------------------
# 15. FIGURE LEGEND
# -----------------------------------------------------------------------------

cat("=== Suggested figure legend ===\n")
cat(paste0(
  "Figure 3. Cumulative incidence of new-onset diabetes mellitus ",
  "following pancreatectomy accounting for the competing risk of death. ",
  "(A) Overall cohort (N = ", nrow(df_tte), "); both new-onset DM ",
  "and death without DM are shown. ",
  "(B) Stratified by resection type. ",
  "(C) Stratified by pathologic diagnosis (adenocarcinoma vs other). ",
  "(D) Stratified by receipt of neoadjuvant therapy. ",
  "Shaded areas represent 95% confidence intervals. ",
  "Group differences were assessed using Gray's test. ",
  "Numbers at risk are shown below each panel at 12-month intervals. ",
  "Patients with pre-operative HbA1c \u22656.5% were excluded (n = 11). ",
  "DM = diabetes mellitus; NAT = neoadjuvant therapy; ",
  "PD = pancreaticoduodenectomy."
), "\n")