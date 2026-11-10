# =============================================================================
# Label-recommended dosing vs measured exposure in ICU patients (DALI-style)
# Simulated vancomycin: dose chosen from a label table on Cockcroft-Gault CrCl,
# exposure driven by true (measured) CrCl plus between-patient variability.
# Illustrative only - not real patient data.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

set.seed(2026)

n <- 200

# Same CrCl structure as 02_bland_altman_crcl.R
measured  <- pmax(5, rnorm(n, mean = 95, sd = 55))
estimated <- pmax(5, measured + (25 - 0.35 * measured) + rnorm(n, 0, 18))

# Label table: daily dose (mg) by CG-estimated CrCl
label_dose <- function(crcl) {
  case_when(
    crcl >= 90 ~ 3000,
    crcl >= 50 ~ 2000,
    crcl >= 30 ~ 1500,
    TRUE       ~ 1000
  )
}

target <- c(400, 600)  # AUC24 target band (mg*h/L)

df <- tibble(measured, estimated) |>
  mutate(
    dose = label_dose(estimated),
    # Vancomycin CL (L/h) from measured CrCl, log-normal residual variability
    cl   = (0.045 * measured + 0.6) * exp(rnorm(n, 0, 0.25)),
    auc  = dose / cl,
    status = case_when(
      auc < target[1] ~ "Below target",
      auc > target[2] ~ "Above target",
      TRUE            ~ "Within target"
    ),
    status = factor(status, c("Below target", "Within target", "Above target"))
  )

pct <- df |> count(status) |> mutate(p = 100 * n / sum(n))
message(paste(sprintf("%s: %.0f%%", pct$status, pct$p), collapse = "; "))

p <- ggplot(df, aes(x = match(dose, c(1000, 1500, 2000, 3000)), y = auc)) +
  annotate("rect", xmin = 0.4, xmax = 4.6, ymin = target[1], ymax = target[2],
           fill = "#e3f1e6", alpha = 0.8) +
  geom_hline(yintercept = target, color = "#2e7d46", linetype = "dashed") +
  geom_jitter(aes(color = status), width = 0.18, alpha = 0.65, size = 2.2,
              show.legend = TRUE) +
  scale_color_manual(values = c("Below target" = "#1f77b4",
                                "Within target" = "#333333",
                                "Above target" = "#a83232"),
                     name = NULL) +
  scale_x_continuous(breaks = 1:4, labels = c("1000", "1500", "2000", "3000"),
                     limits = c(0.4, 4.6), expand = expansion(0)) +
  coord_cartesian(ylim = c(0, 1500)) +
  labs(
    x = "Label-recommended daily dose (mg), chosen from CG-estimated CrCl",
    y = "Measured AUC24 (mg·h/L)",
    subtitle = "Shaded band: target AUC24 400-600 mg·h/L (simulated data)"
  ) +
  theme_minimal(base_size = 16, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    plot.subtitle = element_text(color = "#2e7d46")
  )

ggsave("figures/empirical_dosing.png", p,
       width = 11.2, height = 5.6, dpi = 200, bg = "white")
message("Saved figures/empirical_dosing.png")
