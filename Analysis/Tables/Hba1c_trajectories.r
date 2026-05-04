# =============================================================================
# Table 6: HbA1c Trajectory by New-Onset Diabetes Status
# Median (IQR) HbA1c at each time point stratified by DM outcome
# =============================================================================

library(dplyr)
library(knitr)
library(kableExtra)

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

df <- readRDS("C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/datamerged_endo_exo_PD_clean.rds")

out_dir <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/Endo_Exo_Results/Tables"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 2. PREPARE DATA
# -----------------------------------------------------------------------------

df <- df %>%
  mutate(
    dm_group = case_when(
      new_onset_dm == 1 ~ "New-Onset DM",
      new_onset_dm == 0 ~ "No DM",
      TRUE              ~ NA_character_
    )
  ) %>%
  filter(!is.na(dm_group))

cat("=== DM group counts ===\n")
print(table(df$dm_group))
cat("\n")

# -----------------------------------------------------------------------------
# 3. HELPER FUNCTIONS
# -----------------------------------------------------------------------------

med_iqr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3) return(sprintf("— (n=%d)", length(x)))
  q <- quantile(x, probs = c(0.25, 0.5, 0.75))
  sprintf("%.1f (%.1f\u2013%.1f)", q[2], q[1], q[3])
}

n_obs <- function(x) sum(!is.na(x))

# Wilcoxon p-value between two groups
wilcox_p <- function(x, group, g1 = "New-Onset DM", g2 = "No DM") {
  a <- x[group == g1 & !is.na(x)]
  b <- x[group == g2 & !is.na(x)]
  if (length(a) < 3 || length(b) < 3) return("\u2014")
  p <- tryCatch(
    wilcox.test(a, b, exact = FALSE)$p.value,
    error = function(e) NA
  )
  if (is.na(p)) return("\u2014")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

# -----------------------------------------------------------------------------
# 4. BUILD TABLE
# -----------------------------------------------------------------------------

# Time points and labels
timepoints <- list(
  list(var = "hb_a1c_pre_op", label = "Pre-operative"),
  list(var = "hb_a1c_y1",     label = "Year 1"),
  list(var = "hb_a1c_y2",     label = "Year 2"),
  list(var = "hb_a1c_y3",     label = "Year 3"),
  list(var = "hb_a1c_y4",     label = "Year 4"),
  list(var = "hb_a1c_y5",     label = "Year 5"),
  list(var = "hb_a1c_y6",     label = "Year 6"),
  list(var = "hb_a1c_y7",     label = "Year 7"),
  list(var = "hb_a1c_y8",     label = "Year 8"),
  list(var = "hb_a1c_y9",     label = "Year 9"),
  list(var = "hb_a1c_y10",    label = "Year 10")
)

rows <- list()

for (tp in timepoints) {

  var   <- tp$var
  label <- tp$label
  vals  <- df[[var]]
  grp   <- df$dm_group

  dm_vals  <- vals[grp == "New-Onset DM"]
  nodm_vals <- vals[grp == "No DM"]

  rows[[length(rows) + 1]] <- data.frame(
    Timepoint  = label,
    N_DM       = as.character(n_obs(dm_vals)),
    Median_DM  = med_iqr(dm_vals),
    N_NoDM     = as.character(n_obs(nodm_vals)),
    Median_NoDM = med_iqr(nodm_vals),
    P_value    = wilcox_p(vals, grp),
    stringsAsFactors = FALSE
  )
}

tbl6 <- bind_rows(rows)

colnames(tbl6) <- c(
  "Time Point",
  "N",
  "New-Onset DM\nMedian HbA1c, % (IQR)",
  "N",
  "No DM\nMedian HbA1c, % (IQR)",
  "P-Value\u00b9"
)

# -----------------------------------------------------------------------------
# 5. PRINT TO TERMINAL
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 78), "\n", sep = "")
cat("Table 6. HbA1c Trajectory by New-Onset Diabetes Status\n")
cat(strrep("=", 78), "\n")
print(kable(tbl6, format = "markdown", align = c("l","r","c","r","c","c")))
cat("\n")

# -----------------------------------------------------------------------------
# 6. HTML EXPORT
# -----------------------------------------------------------------------------

n_dm   <- sum(df$dm_group == "New-Onset DM", na.rm = TRUE)
n_nodm <- sum(df$dm_group == "No DM",        na.rm = TRUE)

last_row <- nrow(tbl6)

ht6 <- kable(tbl6,
             format  = "html",
             align   = c("l","r","c","r","c","c"),
             escape  = FALSE,
             caption = paste0(
               "<b>Table 6.</b> HbA1c Trajectory Following Pancreatectomy ",
               "Stratified by New-Onset Diabetes Status ",
               "(New-Onset DM: n = ", n_dm, "; No DM: n = ", n_nodm, ")"
             )) %>%
  kable_styling(
    bootstrap_options = c("condensed"),
    full_width        = TRUE,
    font_size         = 12,
    position          = "center"
  ) %>%
  # Three-line rule
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
  # Grouped column headers
  add_header_above(
    c(" " = 1,
      "New-Onset DM" = 2,
      "No DM" = 2,
      " " = 1),
    bold      = TRUE,
    extra_css = paste0(
      "border-top: none !important;",
      "border-bottom: 1px solid #000000 !important;"
    )
  ) %>%
  column_spec(1, width = "15%",
              extra_css = "border-left: none !important;
                           border-right: none !important;") %>%
  column_spec(c(2, 4), width = "5%",
              extra_css = "text-align: right !important; color: #555555;
                           border-left: none !important;
                           border-right: none !important;") %>%
  column_spec(c(3, 5), width = "30%",
              extra_css = "text-align: center !important;
                           border-left: none !important;
                           border-right: none !important;") %>%
  column_spec(6, width = "10%",
              extra_css = "text-align: center !important;
                           border-left: none !important;
                           border-right: none !important;") %>%
  footnote(
    general = paste0(
      "HbA1c values reported as median (interquartile range) in percent (%). ",
      "N denotes the number of patients with available HbA1c measurement at each time point. ",
      "High rates of missingness at later time points reflect loss to follow-up ",
      "and mortality, particularly in the adenocarcinoma subgroup. ",
      "Pre-operative HbA1c excludes patients with values >=6.5% (n = 11) ",
      "who were removed from the analytic cohort. ",
      "IQR = interquartile range; DM = diabetes mellitus."
    ),
    number = paste0(
      "P-value from two-sided Wilcoxon rank-sum test comparing HbA1c values ",
      "between groups at each time point. Time points with fewer than 3 ",
      "observations in either group are suppressed (\u2014)."
    ),
    general_title     = "Abbreviations: ",
    footnote_as_chunk = FALSE
  )

html6 <- paste0(
  "<!DOCTYPE html>\n<html>\n<head>\n",
  "  <meta charset='UTF-8'>\n",
  "  <title>Table 6 - HbA1c Trajectory</title>\n",
  "  <style>\n",
  "    body   { font-family: 'Times New Roman', Times, serif;",
  "             margin: 60px 80px; max-width: 860px;",
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
  ht6,
  "\n</body>\n</html>"
)

path6 <- file.path(out_dir, "Table6_HbA1c_Trajectory.html")
writeLines(html6, path6)
cat("\u2713 Table 6 exported to:\n  ", path6, "\n\n")

# -----------------------------------------------------------------------------
# 7. QUICK SUMMARY TO TERMINAL
# -----------------------------------------------------------------------------

cat("=== HbA1c trajectory summary ===\n")
cat(sprintf("%-15s %6s %-22s %6s %-22s %10s\n",
            "Time Point", "N(DM)", "DM Median (IQR)",
            "N(No DM)", "No DM Median (IQR)", "P-Value"))
cat(strrep("-", 85), "\n")
for (i in seq_len(nrow(tbl6))) {
  cat(sprintf("%-15s %6s %-22s %6s %-22s %10s\n",
              tbl6[i, 1], tbl6[i, 2], tbl6[i, 3],
              tbl6[i, 4], tbl6[i, 5], tbl6[i, 6]))
}

# =============================================================================
# Figure 1: HbA1c Trajectory by New-Onset Diabetes Status
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)  # for combining figure + n table

# install.packages("patchwork") if needed

df <- readRDS("C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/datamerged_endo_exo_PD_clean.rds")

out_dir <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/Endo_Exo_Results/Figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. RESHAPE TO LONG FORMAT
# -----------------------------------------------------------------------------

hba1c_long <- df %>%
  filter(!is.na(new_onset_dm)) %>%
  mutate(
    dm_group = factor(
      case_when(
        new_onset_dm == 1 ~ "New-Onset DM",
        new_onset_dm == 0 ~ "No DM"
      ),
      levels = c("No DM", "New-Onset DM")
    )
  ) %>%
  select(mayo_id, dm_group,
         hb_a1c_pre_op,
         hb_a1c_y1,  hb_a1c_y2,  hb_a1c_y3,
         hb_a1c_y4,  hb_a1c_y5,  hb_a1c_y6,
         hb_a1c_y7,  hb_a1c_y8,  hb_a1c_y9,
         hb_a1c_y10) %>%
  pivot_longer(
    cols      = starts_with("hb_a1c"),
    names_to  = "timepoint",
    values_to = "hba1c"
  ) %>%
  mutate(
    time_num = case_when(
      timepoint == "hb_a1c_pre_op" ~ 0,
      timepoint == "hb_a1c_y1"     ~ 1,
      timepoint == "hb_a1c_y2"     ~ 2,
      timepoint == "hb_a1c_y3"     ~ 3,
      timepoint == "hb_a1c_y4"     ~ 4,
      timepoint == "hb_a1c_y5"     ~ 5,
      timepoint == "hb_a1c_y6"     ~ 6,
      timepoint == "hb_a1c_y7"     ~ 7,
      timepoint == "hb_a1c_y8"     ~ 8,
      timepoint == "hb_a1c_y9"     ~ 9,
      timepoint == "hb_a1c_y10"    ~ 10
    ),
    time_label = factor(
      case_when(
        timepoint == "hb_a1c_pre_op" ~ "Pre-op",
        TRUE ~ paste0("Y", time_num)
      ),
      levels = c("Pre-op", paste0("Y", 1:10))
    )
  ) %>%
  filter(!is.na(hba1c))

# -----------------------------------------------------------------------------
# 2. SUMMARY STATISTICS
# -----------------------------------------------------------------------------

plot_summary <- hba1c_long %>%
  group_by(dm_group, time_num, time_label) %>%
  summarise(
    median_hba1c = median(hba1c, na.rm = TRUE),
    q25          = quantile(hba1c, 0.25, na.rm = TRUE),
    q75          = quantile(hba1c, 0.75, na.rm = TRUE),
    n            = n(),
    .groups      = "drop"
  )

# N-at-risk table (wide format for the at-risk panel)
n_table <- plot_summary %>%
  select(dm_group, time_num, time_label, n) %>%
  mutate(dm_group = as.character(dm_group))

# -----------------------------------------------------------------------------
# 3. MAIN TRAJECTORY FIGURE
# -----------------------------------------------------------------------------

cols <- c("New-Onset DM" = "#C0392B", "No DM" = "#2471A3")

fig_main <- ggplot(plot_summary,
                   aes(x     = time_num,
                       y     = median_hba1c,
                       color = dm_group,
                       fill  = dm_group,
                       group = dm_group)) +

  # IQR ribbon
  geom_ribbon(aes(ymin = q25, ymax = q75),
              alpha = 0.12, color = NA) +

  # Median line
  geom_line(linewidth = 0.8) +

  # Median points
  geom_point(size = 2.8, shape = 21,
             aes(fill = dm_group),
             color = "white", stroke = 1.2) +

  # ADA DM threshold
  geom_hline(yintercept = 6.5, linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  annotate("text", x = -0.3, y = 6.62,
           label = "DM threshold (6.5%)",
           hjust = 0, size = 2.6,
           color = "grey40", fontface = "italic",
           family = "sans") +

  # Prediabetes threshold
  geom_hline(yintercept = 5.7, linetype = "dotted",
             color = "grey60", linewidth = 0.5) +
  annotate("text", x = -0.3, y = 5.82,
           label = "Prediabetes threshold (5.7%)",
           hjust = 0, size = 2.6,
           color = "grey50", fontface = "italic",
           family = "sans") +

  # Scales
  scale_x_continuous(
    breaks = 0:10,
    labels = c("Pre-op", paste0("Y", 1:10)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(4.8, 10.0),
    breaks = seq(5, 10, 0.5),
    name   = "HbA1c (%)"
  ) +
  scale_color_manual(values = cols,
                     name   = NULL) +
  scale_fill_manual(values  = cols,
                    name    = NULL) +

  labs(x = NULL) +   # x label goes on the n-table panel

  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.text.x        = element_blank(),   # hidden — shown on n-table
    axis.text.y        = element_text(color = "black", size = 8),
    axis.title.y       = element_text(color = "black", size = 9,
                                      margin = margin(r = 6)),
    legend.position    = c(0.13, 0.90),
    legend.background  = element_blank(),
    legend.key         = element_blank(),
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.4, "cm"),
    panel.grid.major.y = element_line(color = "grey93", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    plot.margin        = margin(t = 8, r = 8, b = 0, l = 8)
  )

# -----------------------------------------------------------------------------
# 4. N-AT-RISK TABLE PANEL
# -----------------------------------------------------------------------------

# Reshape for plotting as text
n_plot_data <- n_table %>%
  mutate(
    y_pos = case_when(
      dm_group == "New-Onset DM" ~ 1,
      dm_group == "No DM"        ~ 0
    ),
    y_label = case_when(
      dm_group == "New-Onset DM" ~ "New-Onset DM",
      dm_group == "No DM"        ~ "No DM"
    )
  )

fig_ntable <- ggplot(n_plot_data,
                     aes(x = time_num, y = y_pos,
                         label = n, color = dm_group)) +

  geom_text(size = 2.8, family = "sans", fontface = "plain") +

  # Row labels on the left
  annotate("text", x = -0.6, y = 1,
           label = "New-Onset DM",
           hjust = 1, size = 2.8,
           color = cols["New-Onset DM"],
           family = "sans", fontface = "bold") +
  annotate("text", x = -0.6, y = 0,
           label = "No DM",
           hjust = 1, size = 2.8,
           color = cols["No DM"],
           family = "sans", fontface = "bold") +

  scale_x_continuous(
    breaks = 0:10,
    labels = c("Pre-op", paste0("Y", 1:10)),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(limits = c(-0.6, 1.6)) +
  scale_color_manual(values = cols) +

  labs(x = "Time Since Pancreatectomy", y = NULL) +

  theme_void(base_family = "sans") +
  theme(
    axis.text.x     = element_text(color = "black", size = 8,
                                   margin = margin(t = 3)),
    axis.title.x    = element_text(color = "black", size = 9,
                                   margin = margin(t = 4)),
    legend.position = "none",
    plot.margin     = margin(t = 2, r = 8, b = 8, l = 8)
  )

# -----------------------------------------------------------------------------
# 5. COMBINE PANELS
# -----------------------------------------------------------------------------

fig_combined <- fig_main / fig_ntable +
  plot_layout(heights = c(5, 1))

# -----------------------------------------------------------------------------
# 6. EXPORT
# -----------------------------------------------------------------------------

path_png <- file.path(out_dir, "Figure1_HbA1c_Trajectory.png")
ggsave(path_png, plot = fig_combined,
       width = 7, height = 5, dpi = 600, bg = "white")

path_pdf <- file.path(out_dir, "Figure1_HbA1c_Trajectory.pdf")
ggsave(path_pdf, plot = fig_combined,
       width = 7, height = 5, device = cairo_pdf, bg = "white")

cat("\u2713 Figure 1 (PNG) exported to:\n  ", path_png, "\n")
cat("\u2713 Figure 1 (PDF) exported to:\n  ", path_pdf, "\n\n")

# -----------------------------------------------------------------------------
# 7. FIGURE LEGEND TEXT
# -----------------------------------------------------------------------------

cat("=== Suggested figure legend ===\n")
cat(paste0(
  "Figure 1. HbA1c trajectory following pancreatectomy stratified by ",
  "new-onset diabetes status. Median HbA1c values (solid lines) with ",
  "interquartile range (shaded ribbons) are shown at each annual time point ",
  "for patients who developed new-onset diabetes mellitus (red; n = ",
  sum(df$new_onset_dm == 1, na.rm = TRUE), ") and those who did not ",
  "(blue; n = ", sum(df$new_onset_dm == 0, na.rm = TRUE), "). ",
  "Dashed and dotted reference lines indicate the American Diabetes Association ",
  "diagnostic thresholds for diabetes mellitus (>=6.5%) and prediabetes (>=5.7%), ",
  "respectively. Numbers at risk are shown below the figure at each time point. ",
  "HbA1c = glycated hemoglobin; DM = diabetes mellitus; ",
  "IQR = interquartile range; Y = year post-pancreatectomy."
), "\n")