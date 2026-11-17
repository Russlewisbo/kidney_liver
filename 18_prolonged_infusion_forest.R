# =============================================================================
# Slide 12. Prolonged vs intermittent beta-lactam infusion: randomised evidence.
#   MERCY     Monti 2023, JAMA           n = 607   28-day mortality  RR 0.96 (0.81-1.13)
#   BLING III Dulhunty 2024, JAMA        n = 7,031 90-day mortality  OR 0.91 (0.81-1.01), P = 0.08
#   Meta-analysis Abdul-Aziz 2024, JAMA  18 RCTs, n = 9,108, 90-day mortality RR 0.86 (95 % CrI 0.72-0.98)
#   Focused update Hong 2026, Pharmacotherapy  28 RCTs, mortality RR 0.91 (0.85-0.97)
# Inset (50% fT>MIC both strata; 100% fT>MIC high CL): mechanistic simulation at MIC 2 mg/L from 11_infusion_benefit_by_clearance.R
# (structural model Bastida 2020; variability and strata assumed) - clearly labelled.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 17) theme_minimal(base_size = bs) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", color = col_navy, size = bs * 0.95), plot.subtitle = element_text(color = col_dark, size = bs * 0.75), plot.margin = margin(8, 10, 8, 8))

fo <- tribble(~study, ~detail, ~measure, ~est, ~lo, ~hi,
  "MERCY (2023)\nn = 607, 28-day mortality",                 "", "RR", 0.96, 0.81, 1.13,
  "BLING III (2024)\nn = 7,031, 90-day mortality",           "", "OR", 0.91, 0.81, 1.01,
  "Meta-analysis, 18 RCTs (2024)\nn = 9,108, 90-day mortality", "", "RR", 0.86, 0.72, 0.98,
  "Focused update, 28 RCTs (2026)\nmortality",               "", "RR", 0.91, 0.85, 0.97) |>
  mutate(study = factor(study, rev(study)), type = ifelse(grepl("RCTs", study), "Pooled", "Single trial"),
         lab = sprintf("%s %.2f (%.2f-%.2f)", measure, est, lo, hi))
pa <- ggplot(fo, aes(est, study, color = type)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = col_dark) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.18, orientation = "y", linewidth = 1) +
  geom_point(aes(shape = type), size = 5.5, fill = "white") +
  geom_text(aes(x = 1.17, label = lab), hjust = 0, size = 4.6, color = col_navy, show.legend = FALSE) +
  annotate("text", x = 0.98, y = 0.45, label = "favours prolonged infusion", hjust = 1, size = 4, color = col_dark) +
  annotate("text", x = 1.02, y = 0.45, label = "favours bolus", hjust = 0, size = 4, color = col_dark) +
  scale_x_log10(limits = c(0.62, 1.75), breaks = c(0.7, 0.8, 0.9, 1, 1.1)) +
  scale_color_manual(values = c("Pooled" = col_blue, "Single trial" = col_navy), guide = "none") +
  scale_shape_manual(values = c("Pooled" = 18, "Single trial" = 15), guide = "none") +
  scale_y_discrete(expand = expansion(add = c(0.8, 0.5))) +
  coord_cartesian(clip = "off") +
  labs(x = "Mortality, prolonged vs intermittent infusion (log scale)", y = NULL,
       title = "Single trials neutral; pooled estimates significant") +
  base_theme(16) + theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 12.5, color = col_navy, lineheight = 0.9, hjust = 1))

sim <- read.csv("figures/infusion_benefit_by_clearance.csv") |>
  mutate(cl = sub("\n.*", "", stratum)) |>
  filter((target == "50% fT>MIC") | (target == "100% fT>MIC" & cl == "High CL")) |>
  mutate(group = factor(paste0(cl, "\n", target),
                        c("Typical CL\n50% fT>MIC", "High CL\n50% fT>MIC", "High CL\n100% fT>MIC")),
         short = recode(gsub("\n", " ", regimen),
                        "1 g q8h 0.5 h" = "q8h bolus", "1 g q8h 3 h" = "q8h, 3-h",
                        "0.75 g q6h 3 h" = "q6h, 3-h", "3 g/24 h continuous" = "continuous"),
         short = factor(short, c("q8h bolus", "q8h, 3-h", "q6h, 3-h", "continuous")))
pb <- ggplot(sim, aes(group, PTA, fill = short)) +
  geom_col(position = position_dodge(0.8), width = 0.72) +
  scale_fill_manual(values = c(col_grey, col_blue, "#5fa8d3", col_navy), name = NULL) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 50), expand = c(0, 0), labels = \(x) paste0(x, "%")) +
  labs(x = NULL, y = "Target attainment, MIC 2 mg/L",
       title = "Why heterogeneous? (simulation)",
       subtitle = "All arms 3 g/day; the arms separate only\nat the strict 100% fT>MIC target") +
  base_theme(14) + theme(legend.position = "bottom", legend.text = element_text(size = 9.5),
                         legend.key.size = unit(0.8, "lines"), plot.margin = margin(8, 22, 8, 4),
                         panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 9.5, lineheight = 0.9))

p <- pa + pb + plot_layout(widths = c(1.8, 1))
ggsave("figures/prolonged_infusion_forest.png", p, width = 13.5, height = 5.8, dpi = 200, bg = "white")
