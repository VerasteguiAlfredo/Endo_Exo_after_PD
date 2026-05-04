# =============================================================================
# Figure 4: HbA1c Trajectory — Spaghetti + Smooth
# Panel A: Full cohort median trajectory
# Panel B: Stratified by DM status
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

df <- readRDS("C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/data/datamerged_endo_exo_PD_clean.rds")

fig_out <- "C:/Users/M320532/Desktop/Research/SONC/Endo Exo after PD/Endo_Exo_after_PD/Endo_Exo_Results/Figures"
dir.create(fig_out, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. PREPARE DATA
# -----------------------------------------------------------------------------

df_plot <- df %>%
  filter(!is.na(new_onset_dm)) %>%
  filter(is.na(hb_a1c_pre_op) | hb_a1c_pre_op < 6.5) %>%
  mutate(
    dm_group = factor(
      case_when(
        new_onset_dm == 1 ~ "New-Onset DM",
        new_onset_dm == 0 ~ "No DM"
      ),
      levels = c("No DM", "New-Onset DM")
    )
  )

# Reshape to long — post-op only (Y1-Y10)
hba1c_long <- df_plot %>%
  select(mayo_id, dm_group,
         hb_a1c_y1:hb_a1c_y10) %>%
  pivot_longer(
    cols      = hb_a1c_y1:hb_a1c_y10,
    names_to  = "timepoint",
    values_to = "hba1c"
  ) %>%
  mutate(
    year = as.numeric(gsub("hb_a1c_y", "", timepoint))
  ) %>%
  filter(!is.na(hba1c))

# -----------------------------------------------------------------------------
# 2. SUMMARY STATISTICS
# -----------------------------------------------------------------------------

# Panel A — full cohort
summary_overall <- hba1c_long %>%
  group_by(year) %>%
  summarise(
    median = median(hba1c),
    q25    = quantile(hba1c, 0.25),
    q75    = quantile(hba1c, 0.75),
    n      = n(),
    .groups = "drop"
  )

# Panel B — stratified
summary_strat <- hba1c_long %>%
  group_by(dm_group, year) %>%
  summarise(
    median = median(hba1c),
    q25    = quantile(hba1c, 0.25),
    q75    = quantile(hba1c, 0.75),
    n      = n(),
    .groups = "drop"
  )

cat("=== Panel A summary ===\n")
print(summary_overall)
cat("\n=== Panel B summary ===\n")
print(summary_strat)

# -----------------------------------------------------------------------------
# 3. SPAGHETTI DATA
# Subsample patients with >= 3 time points for cleaner spaghetti
# -----------------------------------------------------------------------------

pts_sufficient <- hba1c_long %>%
  count(mayo_id) %>%
  filter(n >= 3) %>%
  pull(mayo_id)

# Subsample for visual clarity — max 60 patients per group
set.seed(42)
spaghetti_ids <- hba1c_long %>%
  filter(mayo_id %in% pts_sufficient) %>%
  distinct(mayo_id, dm_group) %>%
  group_by(dm_group) %>%
  slice_sample(n = 60, replace = FALSE) %>%
  pull(mayo_id)

spaghetti_data <- hba1c_long %>%
  filter(mayo_id %in% spaghetti_ids)

spaghetti_overall <- hba1c_long %>%
  filter(mayo_id %in% sample(pts_sufficient,
                              min(80, length(pts_sufficient)),
                              replace = FALSE))

# -----------------------------------------------------------------------------
# 4. SHARED THEME
# -----------------------------------------------------------------------------

theme_fig <- theme_classic(base_size = 10, base_family = "sans") +
  theme(
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.4),
    axis.text          = element_text(color = "black", size = 8),
    axis.title         = element_text(color = "black", size = 9),
    panel.grid.major.y = element_line(color = "grey93", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    legend.background  = element_blank(),
    legend.key         = element_blank(),
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.35, "cm"),
    plot.subtitle      = element_text(size = 8, color = "grey40",
                                      margin = margin(b = 4)),
    plot.margin        = margin(t = 8, r = 8, b = 8, l = 8)
  )

# Shared axes
x_scale <- scale_x_continuous(
  breaks = 1:10,
  labels = paste0("Y", 1:10),
  expand = expansion(mult = c(0.03, 0.03))
)

y_scale <- scale_y_continuous(
  limits = c(4.0, 14.0),
  breaks = seq(4.0, 14.0, 1.0),
  name   = "HbA1c (%)"
)

# Reference lines
ref_dm <- geom_hline(yintercept = 6.5, linetype = "dashed",
                     color = "grey50", linewidth = 0.45)
ref_pre <- geom_hline(yintercept = 5.7, linetype = "dotted",
                      color = "grey65", linewidth = 0.40)

ref_label_dm <- annotate("text", x = 1.1, y = 6.62,
                          label = "DM threshold (6.5%)",
                          hjust = 0, size = 2.4, color = "grey40",
                          fontface = "italic", family = "sans")
ref_label_pre <- annotate("text", x = 1.1, y = 5.82,
                           label = "Prediabetes threshold (5.7%)",
                           hjust = 0, size = 2.4, color = "grey55",
                           fontface = "italic", family = "sans")

# -----------------------------------------------------------------------------
# 5. PANEL A — Full cohort
# -----------------------------------------------------------------------------

fig_4a <- ggplot() +

  # Spaghetti — individual trajectories (light, background)
  geom_line(data    = spaghetti_overall,
            mapping = aes(x = year, y = hba1c, group = mayo_id),
            color   = "grey70", alpha = 0.25, linewidth = 0.3) +

  # IQR ribbon
  geom_ribbon(data    = summary_overall,
              mapping = aes(x = year, ymin = q25, ymax = q75),
              fill    = "#2471A3", alpha = 0.15) +

  # Median line
  geom_line(data    = summary_overall,
            mapping = aes(x = year, y = median),
            color   = "#2471A3", linewidth = 1.0) +

  # Median points
  geom_point(data    = summary_overall,
             mapping = aes(x = year, y = median),
             color   = "white", fill = "#2471A3",
             shape = 21, size = 2.8, stroke = 1.2) +

  # N labels above ribbon
  geom_text(data    = summary_overall,
            mapping = aes(x = year, y = q75 + 0.35,
                          label = paste0("n=", n)),
            size = 2.3, color = "grey40", family = "sans") +

  ref_dm + ref_pre + ref_label_dm + ref_label_pre +
  x_scale + y_scale +

  labs(
    x        = "Time Since Pancreatectomy",
    subtitle = "A  |  Full cohort"
  ) +

  theme_fig +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------
# 6. PANEL B — Stratified by DM status
# -----------------------------------------------------------------------------

cols_dm <- c("New-Onset DM" = "#C0392B", "No DM" = "#2471A3")

fig_4b <- ggplot() +

  # Spaghetti — per group
  geom_line(data    = spaghetti_data,
            mapping = aes(x = year, y = hba1c,
                          group = mayo_id, color = dm_group),
            alpha   = 0.15, linewidth = 0.3) +

  # IQR ribbons
  geom_ribbon(data    = summary_strat,
              mapping = aes(x = year, ymin = q25, ymax = q75,
                            fill = dm_group),
              alpha   = 0.12) +

  # Median lines
  geom_line(data    = summary_strat,
            mapping = aes(x = year, y = median, color = dm_group),
            linewidth = 1.0) +

  # Median points
  geom_point(data    = summary_strat,
             mapping = aes(x = year, y = median,
                           color = dm_group, fill = dm_group),
             shape = 21, size = 2.8,
             color = "white", stroke = 1.2) +

  ref_dm + ref_pre + ref_label_dm + ref_label_pre +
  x_scale + y_scale +

  scale_color_manual(values = cols_dm, name = NULL) +
  scale_fill_manual(values  = cols_dm, name = NULL) +

  labs(
    x        = "Time Since Pancreatectomy",
    subtitle = "B  |  Stratified by new-onset diabetes status"
  ) +

  theme_fig +
  theme(
    legend.position = c(0.15, 0.90),
    axis.title.y    = element_blank(),
    axis.text.y     = element_blank(),
    axis.ticks.y    = element_blank(),
    axis.line.y     = element_blank()
  )

# -----------------------------------------------------------------------------
# 7. N TABLE PANELS
# -----------------------------------------------------------------------------

# Panel A n-table
n_table_A <- summary_overall %>%
  mutate(y_pos = 0)

fig_4a_n <- ggplot(n_table_A,
                   aes(x = year, y = y_pos, label = n)) +
  geom_text(size = 2.5, family = "sans", color = "grey30") +
  annotate("text", x = 0.6, y = 0,
           label = "Overall", hjust = 1, size = 2.5,
           color = "grey30", family = "sans", fontface = "bold") +
  scale_x_continuous(breaks = 1:10,
                     labels = paste0("Y", 1:10),
                     expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(limits = c(-0.6, 0.6)) +
  labs(x = NULL, y = NULL) +
  theme_void(base_family = "sans") +
  theme(
    axis.text.x = element_blank(),
    plot.margin = margin(t = 0, r = 8, b = 4, l = 8)
  )

# Panel B n-table
n_table_B <- summary_strat %>%
  mutate(
    y_pos = case_when(
      dm_group == "New-Onset DM" ~ 1,
      dm_group == "No DM"        ~ 0
    )
  )

fig_4b_n <- ggplot(n_table_B,
                   aes(x = year, y = y_pos,
                       label = n, color = dm_group)) +
  geom_text(size = 2.5, family = "sans") +
  annotate("text", x = 0.6, y = 1,
           label = "New-Onset DM", hjust = 1, size = 2.5,
           color = cols_dm["New-Onset DM"],
           family = "sans", fontface = "bold") +
  annotate("text", x = 0.6, y = 0,
           label = "No DM", hjust = 1, size = 2.5,
           color = cols_dm["No DM"],
           family = "sans", fontface = "bold") +
  scale_x_continuous(breaks = 1:10,
                     labels = paste0("Y", 1:10),
                     expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(limits = c(-0.6, 1.6)) +
  scale_color_manual(values = cols_dm) +
  labs(x = NULL, y = NULL) +
  theme_void(base_family = "sans") +
  theme(
    axis.text.x     = element_blank(),
    legend.position = "none",
    plot.margin     = margin(t = 0, r = 8, b = 4, l = 8)
  )

# -----------------------------------------------------------------------------
# 8. SHARED X-AXIS LABEL STRIP
# -----------------------------------------------------------------------------

x_strip <- ggplot(data.frame(x = 1:10, y = 0),
                  aes(x = x, y = y)) +
  geom_blank() +
  scale_x_continuous(breaks = 1:10,
                     labels = paste0("Y", 1:10),
                     expand = expansion(mult = c(0.03, 0.03))) +
  labs(x = "Year Post-Pancreatectomy") +
  theme_void(base_family = "sans") +
  theme(
    axis.text.x  = element_text(color = "black", size = 8,
                                margin = margin(t = 2)),
    axis.title.x = element_text(color = "black", size = 9,
                                margin = margin(t = 3)),
    plot.margin  = margin(t = 0, r = 8, b = 6, l = 8)
  )

# -----------------------------------------------------------------------------
# 9. ASSEMBLE
# -----------------------------------------------------------------------------

fig_4_combined <- (fig_4a | fig_4b) /
                  (fig_4a_n | fig_4b_n) /
                  (x_strip | x_strip) +
  plot_layout(heights = c(6, 0.8, 0.5))

# -----------------------------------------------------------------------------
# 10. EXPORT
# -----------------------------------------------------------------------------

path_png <- file.path(fig_out, "Figure4_HbA1c_Trajectory_Spaghetti.png")
path_pdf <- file.path(fig_out, "Figure4_HbA1c_Trajectory_Spaghetti.pdf")

ggsave(path_png, plot = fig_4_combined,
       width = 10, height = 6, dpi = 600, bg = "white")
ggsave(path_pdf, plot = fig_4_combined,
       width = 10, height = 6, device = cairo_pdf, bg = "white")

cat("\u2713 Figure 4 PNG:", path_png, "\n")
cat("\u2713 Figure 4 PDF:", path_pdf, "\n\n")

# -----------------------------------------------------------------------------
# 11. FIGURE LEGEND
# -----------------------------------------------------------------------------

cat("=== Suggested figure legend ===\n")
cat(paste0(
  "Figure 4. HbA1c trajectory following pancreatectomy. ",
  "Individual patient trajectories (spaghetti lines) are shown for a ",
  "random subsample of patients with three or more post-operative HbA1c ",
  "measurements. Bold lines represent cohort median values at each annual ",
  "time point; shaded areas represent the interquartile range. ",
  "(A) Full analytic cohort. ",
  "(B) Stratified by new-onset diabetes mellitus status (red, New-Onset DM; ",
  "blue, No DM). ",
  "Dashed and dotted reference lines indicate the American Diabetes Association ",
  "diagnostic thresholds for diabetes mellitus (\u22656.5%) and prediabetes ",
  "(\u22655.7%), respectively. ",
  "Numbers of patients with available HbA1c measurements at each time point ",
  "are shown below each panel. ",
  "HbA1c = glycated hemoglobin; DM = diabetes mellitus; Y = year post-pancreatectomy."
), "\n")