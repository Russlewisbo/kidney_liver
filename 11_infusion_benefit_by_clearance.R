# =============================================================================
# Where does prolonged infusion pay off? Meropenem 1 g q8h given as a 0.5-h
# infusion, a 3-h infusion, or 3 g/24 h continuously, in three clearance strata.
# MECHANISTIC SIMULATION (not patient data).
#   Structural model: one compartment, typical CL 8.35 L/h, V 28.2 L
#                     (Bastida 2020, JAC: decompensated cirrhosis, n = 54)
#   ASSUMED by me:    clearance strata = 0.35x, 1x, 2x typical CL;
#                     30 % log-normal between-patient variability on CL and V;
#                     unbound = total (meropenem binding is low);
#                     same daily dose in every stratum (isolates the infusion effect)
#   Target:           100 % fT>MIC at steady state (trough or Css above MIC)
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(ggplot2)})
set.seed(2026)

n <- 5000; CL_tv <- 8.35; V_tv <- 28.2; cv <- 0.30
dose <- 1000; tau <- 8
mic <- 2^(-2:5)

trough_ss <- function(CL, V, tinf) {             # steady-state trough, intermittent infusion
  k <- CL / V
  cmax <- (dose / tinf / CL) * (1 - exp(-k * tinf)) / (1 - exp(-k * tau))
  cmax * exp(-k * (tau - tinf))
}

strata <- tibble(stratum = c("Low clearance\n(2.9 L/h; kidney failure)",
                             "Typical clearance\n(8.4 L/h)",
                             "High clearance\n(16.7 L/h; augmented)"),
                 mult = c(0.35, 1, 2))

res <- strata |>
  rowwise() |>
  reframe(stratum, {
    CL <- CL_tv * mult * exp(rnorm(n, 0, cv)); V <- V_tv * exp(rnorm(n, 0, cv))
    cmin <- list(`0.5-h infusion` = trough_ss(CL, V, 0.5),
                 `3-h infusion`   = trough_ss(CL, V, 3),
                 `Continuous`     = (3 * dose / 24) / CL)
    bind_rows(lapply(names(cmin), \(r) tibble(regimen = r, MIC = mic,
                                              PTA = sapply(mic, \(m) mean(cmin[[r]] > m)))))
  }) |>
  mutate(stratum = factor(stratum, strata$stratum),
         regimen = factor(regimen, c("0.5-h infusion", "3-h infusion", "Continuous")))

write.csv(res, "figures/infusion_benefit_by_clearance.csv", row.names = FALSE)

cols <- c(`0.5-h infusion` = "#7f7f7f", `3-h infusion` = "#1f77b4", `Continuous` = "#1f3a5f")
p <- ggplot(res, aes(MIC, PTA, color = regimen, linetype = regimen)) +
  annotate("rect", xmin = 2, xmax = 8, ymin = -Inf, ymax = Inf, fill = "#fde7e7", alpha = 0.6) +
  geom_hline(yintercept = 0.9, linetype = "dotted", color = "grey40") +
  geom_line(linewidth = 1.4) + geom_point(size = 2.6) +
  geom_text(data = tibble(stratum = factor(strata$stratum[1], strata$stratum), MIC = 4, PTA = 0.06,
                          lab = "MIC 2-8 mg/L"), aes(MIC, PTA, label = lab),
            inherit.aes = FALSE, color = "#a83232", size = 4.4) +
  geom_text(data = tibble(stratum = factor(strata$stratum[1], strata$stratum), MIC = 0.25, PTA = 0.855,
                          lab = "90%"), aes(MIC, PTA, label = lab),
            inherit.aes = FALSE, color = "grey35", size = 4.2, hjust = 0.2) +
  facet_wrap(~stratum, nrow = 1) +
  scale_x_continuous(trans = "log2", breaks = mic, labels = c("0.25", "0.5", "1", "2", "4", "8", "16", "32")) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.25), limits = c(0, 1.02)) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = c("solid", "solid", "22")) +
  labs(x = "MIC (mg/L)", y = "Probability of 100% fT>MIC", color = NULL, linetype = NULL) +
  theme_minimal(base_size = 17) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        legend.text = element_text(size = 15), legend.key.width = unit(2.4, "lines"),
        strip.text = element_text(face = "bold", size = 14.5, color = "#1f3a5f", lineheight = 0.95),
        axis.title = element_text(face = "bold"), panel.spacing = unit(1.4, "lines"))

ggsave("figures/infusion_benefit_by_clearance.png", p, width = 12.5, height = 5.8, dpi = 200, bg = "white")
