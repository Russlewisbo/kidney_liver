# =============================================================================
# RRT slide. Meropenem dosing nomogram, redrawn from Roberts 2025
# (Intensive Care Med 51:1628-1640, doi 10.1007/s00134-025-08067-w), Table 2.
# SMARRT: 300 patients, 22 ICUs, 12 countries; population PK + Monte Carlo.
# Every dose below is transcribed from the published Table 2 (open access,
# CC BY). Merged cells in the original are expanded here to the intensities
# they cover; no value is interpolated.
#   Standard target: steady-state unbound >= 2 mg/L  (100% fT>MIC, 4xMIC Enterobacterales)
#   Higher target:   steady-state unbound >= 4-8 mg/L (empirical / 4xMIC P. aeruginosa)
#   Toxicity threshold used by the authors: 45 mg/L.
#   All regimens are continuous infusion after a 1 g loading dose over 30 min.
# Context (same paper): median prescribed daily dose in the cohort was 3 g;
#   for the commonest patient (anuric, CRRT 1.5-2 L/h) 2 g/day CI suffices even
#   for the higher target; 18.5% of meropenem patients had trough > 20 mg/L and
#   1.5% > 45 mg/L. Trough below the breakpoint in only 1.3%.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(tidyr)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"

d <- tribble(
  ~modality,                 ~intensity, ~urine,       ~target,   ~dose,              ~mid,
  # ---- Continuous RRT ----
  "Continuous RRT",          "1.5 L/h",  "Oligoanuria", "Standard", "1-1.5 g",  1.25,
  "Continuous RRT",          "2.5 L/h",  "Oligoanuria", "Standard", "1-1.5 g",  1.25,
  "Continuous RRT",          "3.5 L/h",  "Oligoanuria", "Standard", "1-1.5 g",  1.25,
  "Continuous RRT",          "1.5 L/h",  "Oligoanuria", "Higher",   "1.5-2 g",  1.75,
  "Continuous RRT",          "2.5 L/h",  "Oligoanuria", "Higher",   "2 g",      2.00,
  "Continuous RRT",          "3.5 L/h",  "Oligoanuria", "Higher",   "2 g",      2.00,
  "Continuous RRT",          "1.5 L/h",  "UO >= 500",   "Standard", "1-1.5 g",  1.25,
  "Continuous RRT",          "2.5 L/h",  "UO >= 500",   "Standard", "1-1.5 g",  1.25,
  "Continuous RRT",          "3.5 L/h",  "UO >= 500",   "Standard", "1-1.5 g",  1.25,
  "Continuous RRT",          "1.5 L/h",  "UO >= 500",   "Higher",   "3 g",      3.00,
  "Continuous RRT",          "2.5 L/h",  "UO >= 500",   "Higher",   "3 g",      3.00,
  "Continuous RRT",          "3.5 L/h",  "UO >= 500",   "Higher",   "3 g",      3.00,
  # ---- Short SLED (~6 h) ----
  "Short SLED (~6 h)",       "9 L/h",    "Oligoanuria", "Standard", "1-1.5 g",  1.25,
  "Short SLED (~6 h)",       "12 L/h",   "Oligoanuria", "Standard", "1-1.5 g",  1.25,
  "Short SLED (~6 h)",       "15 L/h",   "Oligoanuria", "Standard", "1-1.5 g",  1.25,
  "Short SLED (~6 h)",       "9 L/h",    "Oligoanuria", "Higher",   "2 g",      2.00,
  "Short SLED (~6 h)",       "12 L/h",   "Oligoanuria", "Higher",   "2 g",      2.00,
  "Short SLED (~6 h)",       "15 L/h",   "Oligoanuria", "Higher",   "3 g",      3.00,
  "Short SLED (~6 h)",       "9 L/h",    "UO >= 500",   "Standard", "1-1.5 g",  1.25,
  "Short SLED (~6 h)",       "12 L/h",   "UO >= 500",   "Standard", "1-1.5 g",  1.25,
  "Short SLED (~6 h)",       "15 L/h",   "UO >= 500",   "Standard", "1-1.5 g",  1.25,
  "Short SLED (~6 h)",       "9 L/h",    "UO >= 500",   "Higher",   "3 g",      3.00,
  "Short SLED (~6 h)",       "12 L/h",   "UO >= 500",   "Higher",   "3 g",      3.00,
  "Short SLED (~6 h)",       "15 L/h",   "UO >= 500",   "Higher",   "3 g",      3.00,
  # ---- Intermediate SLED (~8 h) ----
  "Intermediate SLED (~8 h)","9 L/h",    "Oligoanuria", "Standard", "1-1.5 g",  1.25,
  "Intermediate SLED (~8 h)","12 L/h",   "Oligoanuria", "Standard", "1-1.5 g",  1.25,
  "Intermediate SLED (~8 h)","15 L/h",   "Oligoanuria", "Standard", "1-1.5 g",  1.25,
  "Intermediate SLED (~8 h)","9 L/h",    "Oligoanuria", "Higher",   "2 g",      2.00,
  "Intermediate SLED (~8 h)","12 L/h",   "Oligoanuria", "Higher",   "3 g",      3.00,
  "Intermediate SLED (~8 h)","15 L/h",   "Oligoanuria", "Higher",   "3 g",      3.00,
  "Intermediate SLED (~8 h)","9 L/h",    "UO >= 500",   "Standard", "1-1.5 g",  1.25,
  "Intermediate SLED (~8 h)","12 L/h",   "UO >= 500",   "Standard", "1-1.5 g",  1.25,
  "Intermediate SLED (~8 h)","15 L/h",   "UO >= 500",   "Standard", "1-1.5 g",  1.25,
  "Intermediate SLED (~8 h)","9 L/h",    "UO >= 500",   "Higher",   "3 g",      3.00,
  "Intermediate SLED (~8 h)","12 L/h",   "UO >= 500",   "Higher",   "3 g",      3.00,
  "Intermediate SLED (~8 h)","15 L/h",   "UO >= 500",   "Higher",   "3 g",      3.00,
  # ---- Long SLED (~12 h) ----
  "Long SLED (~12 h)",       "9 L/h",    "Oligoanuria", "Standard", "1-1.5 g",  1.25,
  "Long SLED (~12 h)",       "12 L/h",   "Oligoanuria", "Standard", "1-1.5 g",  1.25,
  "Long SLED (~12 h)",       "15 L/h",   "Oligoanuria", "Standard", "1-1.5 g",  1.25,
  "Long SLED (~12 h)",       "9 L/h",    "Oligoanuria", "Higher",   "3 g",      3.00,
  "Long SLED (~12 h)",       "12 L/h",   "Oligoanuria", "Higher",   "3 g",      3.00,
  "Long SLED (~12 h)",       "15 L/h",   "Oligoanuria", "Higher",   "3 g",      3.00,
  "Long SLED (~12 h)",       "9 L/h",    "UO >= 500",   "Standard", "1-1.5 g",  1.25,
  "Long SLED (~12 h)",       "12 L/h",   "UO >= 500",   "Standard", "1-1.5 g",  1.25,
  "Long SLED (~12 h)",       "15 L/h",   "UO >= 500",   "Standard", "1-1.5 g",  1.25,
  "Long SLED (~12 h)",       "9 L/h",    "UO >= 500",   "Higher",   "3 g",      3.00,
  "Long SLED (~12 h)",       "12 L/h",   "UO >= 500",   "Higher",   "3 g",      3.00,
  "Long SLED (~12 h)",       "15 L/h",   "UO >= 500",   "Higher",   "3-4 g",    3.50)

stopifnot(nrow(d) == 48)
mods <- c("Continuous\nRRT", "Short SLED\n(~6 h)", "Intermediate\nSLED (~8 h)", "Long SLED\n(~12 h)")
d <- d |>
  mutate(modality = factor(recode(modality, "Continuous RRT" = "Continuous\nRRT", "Short SLED (~6 h)" = "Short SLED\n(~6 h)", "Intermediate SLED (~8 h)" = "Intermediate\nSLED (~8 h)", "Long SLED (~12 h)" = "Long SLED\n(~12 h)"), mods),
         col = factor(paste0(urine, "\n", target),
                      c("Oligoanuria\nStandard", "Oligoanuria\nHigher", "UO >= 500\nStandard", "UO >= 500\nHigher")))

d <- d |> mutate(intensity = factor(intensity, rev(c("1.5 L/h","2.5 L/h","3.5 L/h","9 L/h","12 L/h","15 L/h"))))

p <- ggplot(d, aes(col, intensity, fill = mid)) +
  geom_tile(color = "white", linewidth = 1.1) +
  geom_text(aes(label = dose, color = mid > 2.4), fontface = "bold", size = 5.2) +
  facet_grid(modality ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_gradient(low = "#dce9f5", high = col_navy, guide = "none") +
  scale_color_manual(values = c(`TRUE` = "white", `FALSE` = col_navy), guide = "none") +
  scale_x_discrete(position = "top",
                   labels = c("Oligoanuria\nstandard target\n(>= 2 mg/L)", "Oligoanuria\nhigher target\n(>= 4-8 mg/L)",
                              "Urine >= 500 mL/24 h\nstandard target\n(>= 2 mg/L)", "Urine >= 500 mL/24 h\nhigher target\n(>= 4-8 mg/L)")) +
  labs(x = NULL, y = NULL,
       title = "Meropenem daily dose by continuous infusion, after a 1 g loading dose",
       subtitle = "Redrawn from Roberts 2025, Table 2 (SMARRT: 300 patients, 22 ICUs, 12 countries)",
       caption = "Median dose actually prescribed in this cohort: 3 g/day. For the commonest patient (anuric, CRRT 1.5-2 L/h)\n2 g/day covers even the higher target; 18.5% of patients had troughs above 20 mg/L.") +
  theme_minimal(base_size = 17) +
  theme(panel.grid = element_blank(), panel.spacing.y = unit(0.45, "lines"),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold", color = col_navy, size = 13, lineheight = 0.9),
        axis.text.x.top = element_text(size = 12.5, lineheight = 0.95, color = col_navy, face = "bold"),
        axis.text.y = element_text(size = 13, color = col_dark),
        plot.title = element_text(face = "bold", color = col_navy, size = 16.5),
        plot.subtitle = element_text(color = col_dark, size = 12.5),
        plot.caption = element_text(hjust = 0, size = 12, color = col_red, lineheight = 1.0, margin = margin(t = 12)),
        plot.margin = margin(8, 10, 8, 8))
ggsave("figures/rrt_meropenem_nomogram.png", p, width = 13, height = 6.8, dpi = 300, bg = "white")
