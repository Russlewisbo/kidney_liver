# =============================================================================
# Bland-Altman: Cockcroft-Gault estimated vs 8-h measured CrCl in ICU patients
# Simulates the typical pattern: bias near zero in mid-range,
# systematic underestimation in ARC, wide limits of agreement throughout.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

set.seed(2026)

n <- 150

# Simulate "true" measured CrCl (mL/min) — wide ICU distribution incl. ARC
measured <- pmax(5, rnorm(n, mean = 95, sd = 55))

# CG-estimated: systematic underestimation when measured is high,
# overestimation at low measured values, with substantial random noise.
# Mimic Baptista et al., Udy et al. reports.
bias_systematic <- 25 - 0.35 * measured     # nonzero structure
noise           <- rnorm(n, 0, 18)
estimated       <- pmax(5, measured + bias_systematic + noise)

df <- tibble(
  measured  = measured,
  estimated = estimated,
  mean_cl   = (measured + estimated) / 2,
  diff      = estimated - measured
)

mean_diff <- mean(df$diff)
sd_diff   <- sd(df$diff)
loa_low   <- mean_diff - 1.96 * sd_diff
loa_high  <- mean_diff + 1.96 * sd_diff

p <- ggplot(df, aes(x = mean_cl, y = diff)) +
  annotate("rect", xmin = 130, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#fde7e7", alpha = 0.6) +
  annotate("text", x = 132, y = 75, label = "Augmented renal\nclearance zone",
           hjust = 0, color = "#a83232", size = 4.2, lineheight = 0.95) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_hline(yintercept = mean_diff, color = "#1f77b4", linewidth = 1) +
  geom_hline(yintercept = c(loa_low, loa_high),
             color = "#1f77b4", linetype = "dashed", linewidth = 0.8) +
  geom_point(alpha = 0.55, size = 2.2, color = "#333333") +
  annotate("text", x = 5, y = mean_diff + 4,
           label = sprintf("Bias = %+.1f mL/min", mean_diff),
           hjust = 0, color = "#1f77b4", size = 4.2, fontface = "bold") +
  annotate("text", x = 5, y = loa_high - 5,
           label = sprintf("Upper LoA = %+.1f", loa_high),
           hjust = 0, color = "#1f77b4", size = 3.8) +
  annotate("text", x = 5, y = loa_low + 5,
           label = sprintf("Lower LoA = %.1f", loa_low),
           hjust = 0, color = "#1f77b4", size = 3.8) +
  labs(
    x = "Mean of estimated and measured CrCl (mL/min)",
    y = "Cockcroft-Gault − measured CrCl (mL/min)"
  ) +
  theme_minimal(base_size = 16, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave("figures/bland_altman_crcl.png", p,
       width = 10, height = 5.5, dpi = 200, bg = "white")
message("Saved figures/bland_altman_crcl.png")
