# =============================================================================
# Section 3 slide. Do patients on highly protein-bound anti-infectives do worse
# when albumin is low? All clinical evidence is observational. Values are taken
# from the published abstracts / full texts cited; nothing is simulated.
# Panel A (effect estimates with 95% CI, "worse outcome with hypoalbuminaemia"):
#   Zusman 2015 (CMI 21:54; prospective, n = 279): 30-day mortality OR for
#     hypoalbuminaemia 4.6 (2.1-10.1) in 173 ertapenem patients vs 1.2 (0.5-2.7)
#     in 106 imipenem/meropenem patients (P = 0.02 for the difference).
#   Baalbaki 2022 (J Pharm Technol 38:326; n = 124, Enterobacterales bacteraemia,
#     ceftriaxone): 90-day clinical failure aOR 4.03 (1.12-14.50).
#   Shah 2025 (JAC 80:1823; n = 100, MSSA bacteraemia, ertapenem + ASP/cefazolin):
#     time to negative cultures HR 0.45 (0.27-0.72); shown as 1/HR = 2.22 (1.39-3.70).
#   Shah 2026 (JAC 81(2); n = 285, MSSA bacteraemia, cefazolin or ASP, albumin
#     <= 3.0 g/dL): HR 0.60 (0.44-0.81); shown as 1/HR = 1.67 (1.23-2.27).
# Panel B (crude or weighted percentages, hypoalbuminaemia vs normal albumin):
#   Steere 2023 (OFID 10:ofad102; n = 260, ceftriaxone): failure 12.3 vs 7.7 %
#     (P = .21); ICU subgroup 24.4 vs 7.3 % (P = .07).
#   Arensman Hannan 2024 (AAC 68:e01663-23; n = 137, obesity, ceftriaxone 2 g q12h):
#     clinical success 77.8 vs 91.2 % (P = 0.038) -> failure 22.2 vs 8.8 %.
#   Gavaghan 2022 (OFID 9:S2, abstract; n = 146, ertapenem): 30-day mortality
#     19.7 vs 9.6 % (P = 0.09).
#   Chung 2026 (OFID 13:S1, abstract; n = 53, ICU ESBL bacteraemia, ertapenem):
#     treatment failure 53.1 vs 42.9 % (P = 0.46).
#   VanDorf 2024 (Ann Pharmacother 58:690; n = 54, ICU ESBL bacteraemia, matched on
#     albumin): ertapenem vs meropenem clinical failure 50.0 vs 38.9 % (P = 0.436);
#     escalation 33.3 vs 2.8 % (P = 0.002)  [drug comparison, not albumin comparison]
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 17) theme_minimal(base_size = bs) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", color = col_navy, size = bs * 0.95), plot.subtitle = element_text(color = col_dark, size = bs * 0.75), plot.margin = margin(8, 10, 8, 8))

fo <- tribble(~label, ~drug, ~est, ~lo, ~hi, ~note, ~ctrl,
  "Ertapenem (85-95% bound)\nZusman 2015, n = 173, 30-day death",                 "Ertapenem",     4.60, 2.10, 10.10, "OR 4.6 (2.1-10.1)",  FALSE,
  "Imipenem/meropenem (2-20% bound)\nsame cohort, n = 106, 30-day death",         "Low-binding",   1.20, 0.50,  2.70, "OR 1.2 (0.5-2.7)",   TRUE,
  "Ceftriaxone (85-95% bound)\nBaalbaki 2022, n = 124, 90-day failure",           "Ceftriaxone",   4.03, 1.12, 14.50, "aOR 4.03 (1.12-14.5)", FALSE,
  "Ertapenem + anti-staph. agent, MSSA\nShah 2025, n = 100, slower sterilisation", "Ertapenem",     1/0.45, 1/0.72, 1/0.27, "1/HR 2.2 (1.4-3.7)", FALSE,
  "Cefazolin or ASP, MSSA, albumin <= 3.0\nShah 2026, n = 285, slower sterilisation", "ASP/cefazolin", 1/0.60, 1/0.81, 1/0.44, "1/HR 1.7 (1.2-2.3)", FALSE) |>
  mutate(label = factor(label, rev(label)))
pa <- ggplot(fo, aes(est, label)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = col_dark) +
  geom_errorbar(aes(xmin = lo, xmax = hi, color = ctrl), width = 0.2, linewidth = 0.9, orientation = "y") +
  geom_point(aes(color = ctrl, shape = ctrl), size = 5, fill = "white") +
  geom_text(aes(x = 16, label = note), hjust = 0, size = 4.1, color = col_navy) +
  annotate("text", x = 0.98, y = 0.42, label = "better with low albumin", hjust = 1, size = 3.7, color = col_dark) +
  annotate("text", x = 1.02, y = 0.42, label = "worse with low albumin", hjust = 0, size = 3.7, color = col_dark) +
  scale_x_log10(limits = c(0.4, 70), breaks = c(0.5, 1, 2, 4, 8, 16)) +
  scale_color_manual(values = c(`FALSE` = col_red, `TRUE` = col_blue), guide = "none") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 15), guide = "none") +
  scale_y_discrete(expand = expansion(add = c(0.9, 0.5))) +
  coord_cartesian(clip = "off") +
  labs(x = "Odds or hazard of a worse outcome with hypoalbuminaemia (log scale)", y = NULL,
       title = "Adjusted estimates: the signal is there, and so is the confounder",
       subtitle = "Blue square: the only low-binding control group, same cohort as the ertapenem row") +
  base_theme(15) + theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 11.5, lineheight = 0.9, color = col_navy, hjust = 1),
                         plot.margin = margin(8, 80, 8, 8))

pc <- tribble(~study, ~grp, ~pct,
  "Ceftriaxone\nSteere 2023, n = 260\nP = .21",                 "Low albumin", 12.3, "Ceftriaxone\nSteere 2023, n = 260\nP = .21",                 "Normal", 7.7,
  "Ceftriaxone, ICU\nSteere 2023\nP = .07",            "Low albumin", 24.4, "Ceftriaxone, ICU\nSteere 2023\nP = .07",            "Normal", 7.3,
  "Ceftriaxone, obesity\nArensman 2024, n = 137\nP = .038", "Low albumin", 22.2, "Ceftriaxone, obesity\nArensman 2024, n = 137\nP = .038", "Normal", 8.8,
  "Ertapenem\nGavaghan 2022, n = 146\nP = .09",             "Low albumin", 19.7, "Ertapenem\nGavaghan 2022, n = 146\nP = .09",             "Normal", 9.6,
  "Ertapenem, ESBL, ICU\nChung 2026, n = 53\nP = .46", "Low albumin", 53.1, "Ertapenem, ESBL, ICU\nChung 2026, n = 53\nP = .46", "Normal", 42.9) |>
  mutate(study = factor(study, unique(study)), grp = factor(grp, c("Normal", "Low albumin")))
pb <- ggplot(pc, aes(study, pct, fill = grp)) +
  geom_col(position = position_dodge(0.7), width = 0.62) +
  geom_text(aes(label = paste0(pct, "%")), position = position_dodge(0.7), vjust = -0.4, size = 4, color = col_navy, fontface = "bold") +
  scale_fill_manual(values = c("Normal" = col_grey, "Low albumin" = col_red), name = NULL, labels = c("Normal albumin", "Albumin < 2.5 g/dL")) +
  scale_y_continuous(limits = c(0, 62), breaks = seq(0, 60, 20), expand = c(0, 0), labels = \(x) paste0(x, "%")) +
  labs(x = NULL, y = "Treatment failure or death", title = "Cohorts without adjusted estimates: consistent direction, small numbers",
       subtitle = "Outcome = treatment failure, except Gavaghan (30-day death); retrospective, two are conference abstracts") +
  base_theme(15) + theme(legend.position = "bottom", panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 11.5, lineheight = 0.9))
p <- free(pa) / free(pb) + plot_layout(heights = c(1, 1.05))
ggsave("figures/hypoalbuminaemia_outcomes.png", p, width = 13.5, height = 9.2, dpi = 200, bg = "white")
