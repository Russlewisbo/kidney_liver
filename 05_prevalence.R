# =============================================================================
# Prevalence of kidney and liver dysfunction in septic ICU and oncology patients
# Simulated cohorts: true prevalences are illustrative assumptions, not
# published estimates. Replace with KDIGO / EASL-CLIF figures when available.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

set.seed(2026)

n <- 600

truth <- tribble(
  ~population,            ~condition,                        ~p,
  "Septic ICU",           "AKI (any KDIGO stage)",           0.57,
  "Septic ICU",           "Severe AKI (KDIGO 2-3)",          0.28,
  "Septic ICU",           "Acute or chronic liver failure",  0.18,
  "Oncology / HSCT",      "AKI (any KDIGO stage)",           0.35,
  "Oncology / HSCT",      "Severe AKI (KDIGO 2-3)",          0.15,
  "Oncology / HSCT",      "Acute or chronic liver failure",  0.22
)

df <- truth |>
  mutate(
    x     = rbinom(n(), n, p),
    prev  = 100 * x / n,
    # Wilson 95% CI
    lo    = 100 * sapply(x, \(k) prop.test(k, n)$conf.int[1]),
    hi    = 100 * sapply(x, \(k) prop.test(k, n)$conf.int[2]),
    condition  = factor(condition, unique(condition)),
    population = factor(population, c("Septic ICU", "Oncology / HSCT"))
  )

p <- ggplot(df, aes(x = condition, y = prev, fill = population)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                position = position_dodge(width = 0.75),
                width = 0.2, color = "#333333") +
  geom_text(aes(y = hi + 2, label = sprintf("%.0f%%", prev)),
            position = position_dodge(width = 0.75),
            size = 5, fontface = "bold", color = "#333333") +
  scale_fill_manual(values = c("Septic ICU" = "#1f77b4",
                               "Oncology / HSCT" = "#9aa5b1"),
                    name = NULL) +
  scale_y_continuous(limits = c(0, 65), expand = expansion(mult = c(0, 0))) +
  labs(
    x = NULL,
    y = "Prevalence (% of patients)",
    subtitle = sprintf("Simulated cohorts, n = %d per group; bars show 95%% CI", n)
  ) +
  theme_minimal(base_size = 16, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave("figures/prevalence.png", p,
       width = 11.2, height = 4.9, dpi = 200, bg = "white")
message("Saved figures/prevalence.png")
