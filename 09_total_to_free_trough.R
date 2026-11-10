# =============================================================================
# Same total ceftriaxone trough, different free trough: what a fixed unbound
# fraction hides. Pure arithmetic on published summary statistics:
#   fixed fu = 10 %  (mid-point of published binding 83-96 %, tabulated in Wong 2013)
#   ICU fu   = 33.0 % median, IQR 20.2-44.5 % (Schleibinger 2015, BJCP, n = 20)
#   Targets: unbound trough > 1 and > 4 mg/L (targets used by Gijsen 2021)
# Simplification: fu is treated as concentration-independent within a patient;
# ceftriaxone binding is saturable, so true fu rises further at high totals.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2)})

col_meas <- "#1f77b4"; col_ref <- "#4a4a4a"; col_tgt <- "#a83232"
df <- tibble(total = seq(0, 60, by = 0.5)) |>
  mutate(fixed = 0.10 * total, med = 0.330 * total,
         lo = 0.202 * total, hi = 0.445 * total)
ex <- 20   # worked example: total trough 20 mg/L

p <- ggplot(df, aes(total)) +
  geom_hline(yintercept = c(1, 4), linetype = "dashed", color = col_tgt, linewidth = 0.7) +
  annotate("text", x = 60, y = 1.75, label = "target 1 mg/L", hjust = 1, color = col_tgt, size = 4.6) +
  annotate("text", x = 0.8, y = 4.75, label = "target 4 mg/L", hjust = 0, color = col_tgt, size = 4.6) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = col_meas, alpha = 0.22) +
  geom_line(aes(y = med), color = col_meas, linewidth = 1.5) +
  geom_line(aes(y = fixed), color = col_ref, linewidth = 1.5) +
  annotate("segment", x = ex, xend = ex, y = 0.10 * ex, yend = 0.330 * ex,
           linetype = "dotted", color = "grey30", linewidth = 0.8) +
  annotate("point", x = ex, y = 0.10 * ex, color = col_ref, size = 4) +
  annotate("point", x = ex, y = 0.330 * ex, color = col_meas, size = 4) +
  annotate("text", x = 1.2, y = 25.2, hjust = 0, size = 5.0, color = "#2c3e50", fontface = "bold",
           label = sprintf("Example: total trough %d mg/L", ex)) +
  annotate("text", x = 1.2, y = 23.0, hjust = 0, size = 5.0, color = col_meas,
           label = sprintf("measured binding: free %.1f mg/L (IQR %.1f-%.1f)", 0.330 * ex, 0.202 * ex, 0.445 * ex)) +
  annotate("text", x = 1.2, y = 20.8, hjust = 0, size = 5.0, color = col_ref,
           label = sprintf("assumed binding: free %.1f mg/L", 0.10 * ex)) +
  annotate("text", x = 52, y = 0.330 * 52 + 3.6, hjust = 1, size = 4.9, color = col_meas, fontface = "bold",
           label = "measured in ICU patients\n33% unbound (IQR 20-45%)", lineheight = 0.95) +
  annotate("text", x = 60, y = 7.6, hjust = 1, size = 4.9, color = col_ref, fontface = "bold",
           label = "assumed 10% unbound") +
  scale_x_continuous(breaks = seq(0, 60, 10), expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(breaks = seq(0, 25, 5), limits = c(0, 27), expand = c(0, 0)) +
  labs(x = "Measured total ceftriaxone trough (mg/L)",
       y = "Free ceftriaxone trough (mg/L)") +
  theme_minimal(base_size = 17) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold"), plot.margin = margin(10, 14, 8, 8))

ggsave("figures/total_to_free_trough.png", p, width = 10.5, height = 5.6, dpi = 200, bg = "white")
