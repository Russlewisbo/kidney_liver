# =============================================================================
# Slide 6. Cefepime trough and neurotoxicity (Boschung-Pasquier 2020, CMI).
#   n = 319 patients with trough measured; neurotoxicity in 23.2 %.
#   Trough without neurotoxicity: median 6.3 mg/L (IQR 4.1-8.6)
#   Trough with neurotoxicity:    median 21.6 mg/L (IQR 17.0-28.6)
#   No neurotoxicity below 7.7 mg/L; neurotoxicity in every patient above 38.1 mg/L.
# Median and IQR drawn as reported; individual points are NOT shown because
# patient-level data are not public.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 17) theme_minimal(base_size = bs) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", color = col_navy, size = bs * 0.95), plot.subtitle = element_text(color = col_dark, size = bs * 0.75), plot.margin = margin(8, 10, 8, 8))

d <- tibble(grp = c("No neurotoxicity\n(n ~ 245)", "Neurotoxicity\n(n ~ 74; 23.2%)"),
            med = c(6.3, 21.6), lo = c(4.1, 17.0), hi = c(8.6, 28.6)) |>
  mutate(grp = factor(grp, grp))
p <- ggplot(d, aes(y = grp)) +
  annotate("rect", xmin = 0, xmax = 7.7, ymin = 0.4, ymax = 2.6, fill = col_blue, alpha = 0.07) +
  annotate("rect", xmin = 38.1, xmax = 45, ymin = 0.4, ymax = 2.6, fill = col_red, alpha = 0.08) +
  geom_vline(xintercept = c(7.7, 38.1), linetype = "dashed", color = col_dark) +
  geom_segment(aes(x = lo, xend = hi, yend = grp), linewidth = 9, color = c(col_blue, col_red), lineend = "butt") +
  geom_point(aes(x = med), size = 6, shape = 21, fill = "white", color = c(col_blue, col_red), stroke = 1.6) +
  geom_text(aes(x = hi + 0.8, label = sprintf("median %.1f mg/L (IQR %.1f-%.1f)", med, lo, hi)), hjust = 0, size = 4.8, color = col_navy) +
  annotate("text", x = 8.2, y = 2.72, label = "no neurotoxicity\nbelow 7.7 mg/L", size = 4.2, color = col_dark, lineheight = 0.9, vjust = 0, hjust = 0) +
  annotate("text", x = 37.6, y = 2.72, label = "neurotoxicity in all\nabove 38.1 mg/L", size = 4.2, color = col_dark, lineheight = 0.9, vjust = 0, hjust = 1) +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 40, 10), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.5, 1.0))) +
  labs(x = "Cefepime trough concentration (mg/L)", y = NULL,
       title = "319 patients with a measured cefepime trough",
       subtitle = "Those with neurotoxicity had lower kidney function (median eGFR 35 vs 82 mL/min/1.73 m\u00b2)",
       caption = "Time course (Payne 2017, 135 cases): onset a median of 4 days (IQR 2-6) after starting cefepime - late enough to be blamed on\nthe sepsis - and improvement a median of 2 days (IQR 1-3) after stopping it. One in four cases occurred on renally appropriate doses.") +
  base_theme(17) + theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 15, lineheight = 0.9, color = col_dark),
                         plot.caption = element_text(hjust = 0, size = 11.5, color = col_red, lineheight = 1.1, margin = margin(t = 10)))
ggsave("figures/cefepime_neurotoxicity.png", p, width = 13, height = 5.9, dpi = 300, bg = "white")
