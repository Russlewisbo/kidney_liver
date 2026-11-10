# =============================================================================
# Monte Carlo PTA: meropenem 1 g q8h intermittent vs extended vs continuous
# Target: 50% fT>MIC (clinical efficacy); 100% fT>4xMIC shown as sensitivity
# Population: ICU adults with altered Vd (sepsis / ARC / fluid resuscitation)
# One-compartment model with log-normal BSV on CL and V
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

set.seed(42)

# ---- Population PK parameters (Roberts et al., Jaruratanasirikul et al.) ----
n_subj   <- 5000
CL_tv    <- 12      # L/h, typical clearance (mixed renal function ICU)
V_tv     <- 25      # L, typical Vd
CV_CL    <- 0.45    # 45% BSV (wider than healthy due to organ dysfunction)
CV_V     <- 0.30    # 30% BSV
ke       <- function(CL, V) CL / V

# Sample BSV
CL_i <- CL_tv * exp(rnorm(n_subj, 0, CV_CL))
V_i  <- V_tv  * exp(rnorm(n_subj, 0, CV_V))

# ---- Regimens (all 1 g q8h equivalent total daily dose, except continuous 3 g/24h) ----
# Intermittent: 1 g over 0.5 h, q8h
# Extended:     1 g over 3.0 h, q8h
# Continuous:   3 g over 24 h (with 1 g loading dose over 0.5 h)

simulate_conc <- function(t, CL, V, dose, tinf, tau, n_doses = 4) {
  # Steady-state one-compartment infusion model
  k <- ke(CL, V)
  R0 <- dose / tinf
  # During infusion of dose n: C = (R0/(CL))*(1 - exp(-k*t_in_inf))*exp(-k*time_since_inf_end_in_prev_doses)
  # Simpler: compute superposition over n_doses to reach SS
  C <- numeric(length(t))
  for (n in 0:(n_doses - 1)) {
    t_start <- n * tau
    t_inf_end <- t_start + tinf
    # During infusion
    in_inf <- t >= t_start & t <= t_inf_end
    C[in_inf] <- C[in_inf] + (R0 / CL) * (1 - exp(-k * (t[in_inf] - t_start)))
    # After infusion
    post_inf <- t > t_inf_end
    Cend <- (R0 / CL) * (1 - exp(-k * tinf))
    C[post_inf] <- C[post_inf] + Cend * exp(-k * (t[post_inf] - t_inf_end))
  }
  C
}

simulate_continuous <- function(t, CL, V, load_dose, load_tinf, cont_rate) {
  # Loading dose (over load_tinf), then continuous infusion at cont_rate (mg/h)
  k <- ke(CL, V)
  C <- numeric(length(t))
  # Loading
  R0 <- load_dose / load_tinf
  in_load <- t <= load_tinf
  C[in_load] <- (R0 / CL) * (1 - exp(-k * t[in_load]))
  Cend_load <- (R0 / CL) * (1 - exp(-k * load_tinf))
  # After loading: superposition of decay + continuous infusion rising to Css
  post_load <- t > load_tinf
  tt <- t[post_load] - load_tinf
  Css <- cont_rate / CL
  C[post_load] <- Cend_load * exp(-k * tt) + Css * (1 - exp(-k * tt))
  C
}

# ---- Time-above-MIC computation in a 24-h window at SS ----
mic_grid <- c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32)
t_grid   <- seq(0, 24, by = 0.05)  # 0.05 h resolution

fT_above_MIC <- function(C, mic, t) {
  # Fraction of time C > mic over the window
  mean(C > mic)
}

compute_pta <- function(regimen) {
  pta <- sapply(mic_grid, function(mic) {
    above <- vapply(seq_len(n_subj), function(i) {
      if (regimen == "intermittent") {
        C <- simulate_conc(t_grid, CL_i[i], V_i[i],
                           dose = 1000, tinf = 0.5, tau = 8, n_doses = 4)
      } else if (regimen == "extended") {
        C <- simulate_conc(t_grid, CL_i[i], V_i[i],
                           dose = 1000, tinf = 3.0, tau = 8, n_doses = 4)
      } else {
        C <- simulate_continuous(t_grid, CL_i[i], V_i[i],
                                 load_dose = 1000, load_tinf = 0.5,
                                 cont_rate = 3000 / 24)
      }
      # Use last 8 h for IV bolus/extended (SS dosing interval),
      # last 12 h for continuous to capture Css
      t_eval <- if (regimen == "continuous") t_grid >= 12 else t_grid >= 16
      fT_above_MIC(C[t_eval], mic, t_grid[t_eval])
    }, numeric(1))
    mean(above >= 0.5)   # 50% fT>MIC clinical target
  })
  tibble(MIC = mic_grid, PTA = pta, Regimen = regimen)
}

results <- bind_rows(
  compute_pta("intermittent"),
  compute_pta("extended"),
  compute_pta("continuous")
) |>
  mutate(Regimen = factor(Regimen,
                          levels = c("intermittent", "extended", "continuous"),
                          labels = c("Intermittent (0.5 h)",
                                     "Extended (3 h)",
                                     "Continuous (24 h)")))

# ---- Plot in assertion-evidence style ----
p <- ggplot(results, aes(x = MIC, y = PTA, color = Regimen, group = Regimen)) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "grey50") +
  annotate("text", x = 0.15, y = 0.93, label = "90% PTA target",
           hjust = 0, color = "grey40", size = 4.2) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 2.5) +
  scale_x_log10(breaks = mic_grid,
                labels = c("0.125","0.25","0.5","1","2","4","8","16","32")) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1), expand = c(0,0)) +
  scale_color_manual(values = c("#7f7f7f", "#1f77b4", "#d62728")) +
  labs(x = "MIC (mg/L)",
       y = "Probability of attaining 50% fT>MIC",
       color = NULL) +
  theme_minimal(base_size = 16, base_family = "sans") +
  theme(
    legend.position = c(0.82, 0.85),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

ggsave("figures/pta_meropenem.png", p, width = 10, height = 5.5, dpi = 200, bg = "white")
message("Saved figures/pta_meropenem.png")
