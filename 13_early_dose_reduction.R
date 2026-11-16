# =============================================================================
# Slide 2. Renal dose reduction in the first days and outcome.
# (a) Regulatory-label subgroup data, cIAI, baseline CrCl 30-50 mL/min, MITT:
#     ceftolozane/tazobactam label (FDA 2022): 11/23 (47.8 %) vs meropenem 9/13 (69.2 %);
#       CrCl > 50: 312/366 (85.2 %) vs 355/404 (87.9 %)
#     ceftazidime/avibactam label (FDA 2025): 14/31 (45 %) vs meropenem 26/35 (74 %);
#       label states these patients received a 33 % lower daily dose than now recommended
# (b) Aldardeer 2024 (prospective cohort, n = 224): early vs late renal dose adjustment,
#     in-hospital death HR 0.59 (95 % CI 0.36-0.97) favouring LATE adjustment.
#     Horstink 2025 scoping review: 11 studies, 1,407 patients, high risk of bias.
# All values from the cited label text / abstracts; no simulation.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 17) theme_minimal(base_size = bs) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", color = col_navy, size = bs * 0.95), plot.subtitle = element_text(color = col_dark, size = bs * 0.75), plot.margin = margin(8, 10, 8, 8))

lab <- tribble(~drug, ~arm, ~n, ~N,
  "Ceftolozane/\ntazobactam\nCrCl 30-50", "New agent", 11, 23,
  "Ceftolozane/\ntazobactam\nCrCl 30-50", "Meropenem", 9, 13,
  "Ceftazidime/\navibactam\nCrCl 30-50",  "New agent", 14, 31,
  "Ceftazidime/\navibactam\nCrCl 30-50",  "Meropenem", 26, 35,
  "Ceftolozane/\ntazobactam\nCrCl > 50 (reference)", "New agent", 312, 366,
  "Ceftolozane/\ntazobactam\nCrCl > 50 (reference)", "Meropenem", 355, 404) |>
  mutate(pct = 100 * n / N, drug = factor(drug, unique(drug)[c(3, 1, 2)]),
         arm = factor(arm, c("New agent", "Meropenem")),
         lab = sprintf("%.0f%%\n(%d/%d)", pct, n, N))

pa <- ggplot(lab, aes(drug, pct, fill = arm)) +
  geom_col(position = position_dodge(0.7), width = 0.62) +
  geom_text(aes(label = lab), position = position_dodge(0.7), vjust = -0.25, size = 4.2, lineheight = 0.9, color = col_navy) +
  scale_fill_manual(values = c("New agent" = col_blue, "Meropenem" = col_grey), name = NULL,
                    labels = c("Renally adjusted new agent", "Meropenem comparator")) +
  scale_y_continuous(limits = c(0, 108), breaks = seq(0, 100, 25), expand = c(0, 0), labels = \(x) paste0(x, "%")) +
  labs(x = NULL, y = "Clinical cure, cIAI (MITT)",
       title = "Label precautions: cure fell at CrCl 30-50 mL/min",
       subtitle = "FDA prescribing information, section 5.1, both agents") +
  base_theme(16) + theme(legend.position = "bottom", panel.grid.major.x = element_blank(),
                         axis.text.x = element_text(size = 12, lineheight = 0.88))

fo <- tibble(study = "Aldardeer 2024\nlate vs early adjustment\nn = 224", hr = 0.588, lo = 0.355, hi = 0.974)
pb <- ggplot(fo, aes(hr, study)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = col_dark) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.15, orientation = "y", linewidth = 0.9, color = col_blue) +
  geom_point(size = 5, color = col_blue) +
  annotate("text", x = 0.588, y = 1.42, label = "HR 0.59 (95% CI 0.36-0.97)", size = 4.6, color = col_navy, fontface = "bold") +
  annotate("text", x = 0.3, y = 0.55, label = "favours late adjustment", hjust = 0, size = 4, color = col_dark) +
  annotate("text", x = 1.05, y = 0.55, label = "favours early", hjust = 0, size = 4, color = col_dark) +
  scale_x_log10(limits = c(0.28, 1.6), breaks = c(0.3, 0.5, 1, 1.5)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 0.7))) +
  labs(x = "Hazard ratio, in-hospital death (log scale)", y = NULL,
       title = "Cohort: later adjustment,\nlower in-hospital mortality",
       subtitle = "Scoping review: 11 studies,\n1,407 patients, high risk of bias") +
  base_theme(16) + theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 12.5, lineheight = 0.9, color = col_dark))

p <- pa + pb + plot_layout(widths = c(1.5, 1))
ggsave("figures/early_dose_reduction.png", p, width = 13, height = 6, dpi = 200, bg = "white")
