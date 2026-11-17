# =============================================================================
# Cirrhosis GFR slide. Bias of eGFR equations against measured GFR in cirrhosis,
# restricted to patients with measured GFR < 60 mL/min/1.73 m2.
# Source: Singapura 2021, Liver Transpl 27:1538-1552, doi 10.1002/lt.26216.
# Systematic review/meta-analysis: 25 studies, n = 4565 (52.0 years, 37.0% women),
# 18 equations. The <60 subset: 6 studies, 12 equations, 467 patients, measured
# GFR range 22.6-56.8 mL/min/1.73 m2.
# Values below are the clinical-unit (mL/min/1.73 m2) biases reported in the
# abstract; a positive value means the equation OVERESTIMATES GFR:
#   All equations pooled          +21.7 (95% CI 17.7-25.7)
#   MDRD 6                        +15.7 (10.6-20.8)
#   Royal Free Hospital (RFH)     +15.0 (5.5-24.6)
#   GRAIL                         +12.6 (7.2-18.0)   [evaluated in 1 study only]
#   CKD-EPI cystatin C (CEC)      +10.3 (2.1-18.4)
# Context row (all levels of GFR, patients with ascites): +8.3 (-3.1 to 19.7).
# Across all levels of GFR, CKD-EPI creatinine-cystatin C (CESC) was least biased
# (-3.6; -17.4 to 10.3) but imprecise. No simulation; no value is interpolated.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"

d <- tribble(~eq, ~detail, ~est, ~lo, ~hi, ~grp,
  "All equations pooled",      "12 equations, 6 studies, 467 patients", 21.7, 17.7, 25.7, "pooled",
  "MDRD 6",                     "precise, but the most biased equation", 15.7, 10.6, 20.8, "cr",
  "Royal Free Hospital",        "developed in cirrhosis",               15.0,  5.5, 24.6, "cr",
  "GRAIL",                      "developed in cirrhosis; 1 study only", 12.6,  7.2, 18.0, "cr",
  "CKD-EPI cystatin C",         "least biased; needs cystatin C",       10.3,  2.1, 18.4, "cys",
  "All equations, ascites", "any level of GFR; shown for contrast",    8.3, -3.1, 19.7, "ctx") |>
  mutate(eq = paste0(eq, "\n", detail), eq = factor(eq, rev(eq)),
         lab = sprintf("%+.1f (%.1f to %.1f)", est, lo, hi),
         lab = ifelse(est == 8.3, "+8.3 (-3.1 to 19.7)", lab))

p <- ggplot(d, aes(est, eq, color = grp)) +
  annotate("rect", xmin = 0, xmax = 30, ymin = 0.4, ymax = 6.6, fill = col_red, alpha = 0.05) +
  geom_vline(xintercept = 0, linetype = "dashed", color = col_dark) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.18, linewidth = 1, orientation = "y") +
  geom_point(aes(shape = grp), size = 5.5, fill = "white") +
  geom_text(aes(x = 28.5, label = lab), hjust = 0, size = 4.4, color = col_navy, show.legend = FALSE) +
  annotate("text", x = 27, y = 0.5, hjust = 1, size = 4.1, color = col_red,
           label = "equation reads HIGHER than the true GFR") +
  scale_color_manual(values = c(pooled = col_red, cr = col_navy, cys = col_blue, ctx = col_grey), guide = "none") +
  scale_shape_manual(values = c(pooled = 18, cr = 16, cys = 16, ctx = 15), guide = "none") +
  scale_x_continuous(limits = c(-5, 41), breaks = seq(0, 30, 10),
                     labels = \(x) ifelse(x > 0, paste0("+", x), as.character(x))) +
  scale_y_discrete(expand = expansion(add = c(0.75, 0.6))) +
  coord_cartesian(clip = "off") +
  labs(x = "Bias vs measured GFR (mL/min/1.73 m\u00b2); positive = overestimates kidney function", y = NULL,
       title = "When true GFR is below 60, every equation overestimates it - by 10 to 22 mL/min",
       subtitle = "Cirrhosis; meta-analysis of 25 studies, 4,565 patients (Singapura 2021). Subgroup: measured GFR 22.6-56.8",
       caption = "Consequence at the bedside: a patient whose true GFR is 30 is reported as 40 to 52 depending on the equation,\nand the dose of a renally cleared, narrow-index drug (aminoglycoside, vancomycin) is set from the higher number.") +
  theme_minimal(base_size = 17) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 12, color = col_navy, lineheight = 1.15, hjust = 1),
        plot.title = element_text(face = "bold", color = col_navy, size = 16.5),
        plot.subtitle = element_text(color = col_dark, size = 12.5),
        plot.caption = element_text(hjust = 0, size = 12, color = col_red, lineheight = 1.05, margin = margin(t = 10)),
        plot.margin = margin(8, 10, 8, 8))
ggsave("figures/cirrhosis_gfr_bias.png", p, width = 13.2, height = 6.4, dpi = 200, bg = "white")
