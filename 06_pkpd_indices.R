# =============================================================================
# The three PK/PD indices on one concentration-time curve
# One-compartment IV bolus, q8h dosing at (near) steady state, simulated.
# Illustrative parameters only.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

k    <- 0.4    # elimination rate constant (1/h)
c0   <- 20     # increment per dose (mg/L)
tau  <- 8      # dosing interval (h)
mic  <- 4      # mg/L
doses <- seq(0, 40, by = tau)

conc <- function(t) {
  sapply(t, \(ti) sum(ifelse(ti >= doses, c0 * exp(-k * (ti - doses)), 0)))
}

# Last full interval shown (steady state, 5th dose)
t0 <- 32; t1 <- 40
tt <- seq(0, t1, by = 0.05)
df <- tibble(t = tt, c = conc(tt))

cmax <- conc(t0 + 1e-6)
t_above <- t0 + log(cmax / mic) / k   # time at which curve crosses MIC
pct_above <- 100 * (t_above - t0) / tau
auc <- df |> filter(t >= t0) |> summarise(a = sum(c) * 0.05) |> pull(a)

message(sprintf("Cmax = %.1f, Cmax/MIC = %.1f, fT>MIC = %.0f%%, AUC(tau)/MIC = %.0f",
                cmax, cmax / mic, pct_above, auc / mic))

cols <- c(cmax = "#a83232", auc = "#2e7d46", time = "#1f77b4")

p <- ggplot(df, aes(t, c)) +
  # Shade only the area above the MIC line
  geom_ribbon(data = filter(df, t >= t0), aes(ymin = mic, ymax = pmax(c, mic)),
              fill = cols[["auc"]], alpha = 0.18) +
  geom_line(linewidth = 1.1, color = "#333333") +
  geom_hline(yintercept = mic, linetype = "dashed", color = "grey40") +
  annotate("text", x = 0.3, y = mic + 1.3, label = "MIC", hjust = 0,
           color = "grey30", size = 4.8, fontface = "bold") +
  # Cmax/MIC: vertical arrow from MIC to the peak
  annotate("segment", x = t0 - 0.9, xend = t0 - 0.9, y = mic, yend = cmax,
           color = cols[["cmax"]], linewidth = 1,
           arrow = arrow(ends = "both", length = unit(0.12, "in"))) +
  annotate("text", x = t0 - 1.5, y = (mic + cmax) / 2,
           label = "Cmax / MIC", hjust = 1, color = cols[["cmax"]],
           size = 5.2, fontface = "bold") +
  # fT>MIC: horizontal bracket along the MIC line
  annotate("segment", x = t0, xend = t_above, y = mic - 1.6, yend = mic - 1.6,
           color = cols[["time"]], linewidth = 1,
           arrow = arrow(ends = "both", length = unit(0.12, "in"))) +
  annotate("text", x = (t0 + t_above) / 2, y = mic - 3.3,
           label = "fT > MIC", color = cols[["time"]],
           size = 5.2, fontface = "bold") +
  # fAUC/MIC: shaded area label
  annotate("text", x = t1 - 0.6, y = 11, label = "fAUC / MIC",
           hjust = 1, color = cols[["auc"]], size = 5.2, fontface = "bold") +
  scale_x_continuous(breaks = doses, expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = c(-1, NA), expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(xlim = c(0, t1)) +
  labs(
    x = "Time (h)",
    y = "Free drug concentration (mg/L)",
    subtitle = "Simulated one-compartment profile, q8h bolus dosing at steady state"
  ) +
  theme_minimal(base_size = 16, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave("figures/pkpd_indices.png", p,
       width = 11.2, height = 5.6, dpi = 200, bg = "white")
message("Saved figures/pkpd_indices.png")
