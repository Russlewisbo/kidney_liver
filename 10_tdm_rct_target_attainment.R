# =============================================================================
# PK/PD target attainment by trial arm in the three randomised trials of
# TDM- or model-guided antibiotic dosing in ICU patients (published 2022).
# Published percentages only (abstracts); no simulation.
#   TARGET   Hagel 2022, Intensive Care Med      piperacillin/tazobactam, n = 249
#            target attainment 37.3 % (TDM) vs 14.6 % (no TDM); mean SOFA 7.9 vs 8.2, P = 0.39
#   DOLPHIN  Ewoldt 2022, Intensive Care Med     beta-lactams + ciprofloxacin, n = 388
#            day 3: 59.5 % (MIPD) vs 60.4 % (standard); ICU stay IRR 1.16 (0.96-1.41)
#   Right Dose, Right Now  Roggeveen 2022, Crit Care   n = 252 randomised (stopped early)
#            ciprofloxacin 69 % vs 3 %; "for the other antibiotics [vancomycin, meropenem,
#            ceftriaxone] AutoKinetics dosing did not improve target attainment" (no % in abstract)
# Target definitions and sampling days differ between trials: compare arms
# WITHIN a trial, not bars BETWEEN trials.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2)})

col_int <- "#1f77b4"; col_std <- "#9aa3ad"
d <- tribble(
  ~trial,                                              ~arm,        ~pct,
  "TARGET\npiperacillin/tazobactam",                   "Standard",  14.6,
  "TARGET\npiperacillin/tazobactam",                   "Guided",    37.3,
  "DOLPHIN\nbeta-lactams + ciprofloxacin, day 3",      "Standard",  60.4,
  "DOLPHIN\nbeta-lactams + ciprofloxacin, day 3",      "Guided",    59.5,
  "Right Dose, Right Now\nciprofloxacin only",         "Standard",   3,
  "Right Dose, Right Now\nciprofloxacin only",         "Guided",    69
) |>
  mutate(trial = factor(trial, unique(trial)),
         arm   = factor(arm, c("Standard", "Guided")))

outc <- tibble(
  trial = factor(levels(d$trial), levels(d$trial)),
  lab = c("Mean SOFA 7.9 vs 8.2\nP = 0.39",
          "ICU stay IRR 1.16\n(95% CI 0.96-1.41)",
          "Clinical endpoints:\nno difference")
)

p <- ggplot(d, aes(arm, pct, fill = arm)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%s%%", format(pct, nsmall = 0, drop0trailing = TRUE))),
            vjust = -0.5, size = 5.6, fontface = "bold", color = "#2c3e50") +
  geom_label(data = outc, aes(x = 1.5, y = 96, label = lab), inherit.aes = FALSE,
             size = 4.5, lineheight = 0.95, fill = "#f7f9fc", color = "#1f3a5f",
             linewidth = 0.3, label.padding = unit(0.45, "lines")) +
  facet_wrap(~trial, nrow = 1) +
  scale_fill_manual(values = c(Standard = col_std, Guided = col_int), guide = "none") +
  scale_x_discrete(labels = c(Standard = "Standard\ndosing", Guided = "TDM / model-\nguided dosing")) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 80, 20), expand = c(0, 0),
                     labels = \(x) paste0(x, "%")) +
  labs(x = NULL, y = "Patients attaining the\ntrial's PK/PD target") +
  theme_minimal(base_size = 17) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        strip.text = element_text(face = "bold", size = 14.5, color = "#1f3a5f", lineheight = 0.95),
        axis.title.y = element_text(face = "bold", hjust = 0.35),
        panel.spacing = unit(1.6, "lines"), plot.margin = margin(8, 10, 8, 8))

ggsave("figures/tdm_rct_target_attainment.png", p, width = 12.5, height = 5.8, dpi = 200, bg = "white")
