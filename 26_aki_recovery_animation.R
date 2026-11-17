# =============================================================================
# Stepped build for the hypothetical patient introduced on slide 2 and resolved
# on the closing slide. THE POINT: serum creatinine is a lagging, production-
# limited indicator. It keeps the renal-dosing table on q12h long after the
# kidney has recovered, and meropenem 1 g q12h then fails the PK/PD target.
#
# 58-year-old, alcohol-related cirrhosis (Child-Pugh B, albumin 22 g/L),
# septic shock from secondary peritonitis. Creatinine 2.1 mg/dL (baseline 0.8),
# no urine for 6 h. The table proposes meropenem 1 g q12h.
#
# MECHANISTIC SIMULATION OF ONE PATIENT - not data, no variability.
# Three coupled models, all deterministic:
#  (1) Kidney. Measured creatinine clearance rises 20 -> 190 mL/min, logistic,
#      half-recovery at 22 h, most of it between hours 16 and 28 - the abrupt
#      return of urine output when septic shock reverses. ASSUMED BY ME. The 190
#      plateau is augmented renal
#      clearance (CrCl >= 130), reported in ~4 of 10 ICU patients - the deck's
#      own ARC slide - and favoured by the hyperdynamic circulation of cirrhosis.
#  (2) Creatinine. One compartment, V = 42 L (0.6 x 70 kg), constant production
#      P = 50 mg/h, so that a clearance of 100 mL/min returns her stated baseline
#      of 0.8 mg/dL. dCr/dt = (P - CLcr(t)*Cr)/V. Laboratory creatinine is
#      sampled once a day (0, 24, 48 h), as in practice.
#      Cockcroft-Gault for a 58-year-old 70 kg man = 79.7 / Cr(mg/dL).
#  (3) Meropenem. One compartment, V = 28.2 L (Bastida 2020, JAC; decompensated
#      cirrhosis, n = 54). CL(t) = 2.4 L/h non-renal + 0.085 L/h per mL/min of
#      measured creatinine clearance; the slope is calibrated so that CrCl 70
#      returns the Bastida typical CL of 8.35 L/h.
#      Binding ~2%, so unbound = total. MIC 2 mg/L (EUCAST breakpoint).
#      dA/dt = R(t) - (CL(t)/V) A, explicit Euler, dt = 0.001 h.
#
# Regimens: TABLE 1 g q12h over 30 min, fixed on the admission creatinine.
#           FIX   2 g load over 30 min, then 1 g q8h over 3 h (3 g/day).
#           A third, 2 g q8h over 3 h, is computed for the annotation only.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 17) theme_minimal(base_size = bs) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = col_navy, size = bs * 0.95),
        plot.subtitle = element_text(color = col_dark, size = bs * 0.75),
        plot.margin = margin(6, 10, 4, 8))

V <- 28.2; CL_nr <- 2.4; slope <- (8.35 - CL_nr)/70; MIC <- 2; MIC4 <- 8
plateau <- 190
crcl <- function(t) 20 + (plateau - 20)/(1 + exp(-(t - 22)/2.5))
CLt  <- function(t) CL_nr + slope * crcl(t)
CG   <- function(cr_mgdl) (140 - 58) * 70 / (72 * cr_mgdl)

dt <- 0.001; tg <- seq(0, 48, by = dt)
# ---- creatinine kinetics ----------------------------------------------------
# One pool, but sized to THIS patient rather than to a textbook 70 kg man.
# Creatinine distributes in total body water: 0.6 x 70 kg = 42 L lean, and this
# patient has been resuscitated for septic shock and has ascites, so the pool is
# taken as 55 L. Production is constant at 50 mg/h, set so that a clearance of
# 100 mL/min returns her stated baseline of 0.8 mg/dL.
# The consequence, and the point of the slide: the plasma level can only approach
# its new steady state with a half-life of V/CL x ln2, which is 11 h while the
# clearance is still 60 mL/min and 2.3 h once it reaches 190. The creatinine
# therefore reports a change in GFR late, and reports it later the sicker the
# kidney still is. Daily sampling (0, 24, 48 h) does the rest.
Vcr <- 55; Pcr <- 50
Cr <- numeric(length(tg)); Cr[1] <- 21      # mg/L = 2.1 mg/dL
for (i in 2:length(tg)) Cr[i] <- Cr[i-1] + dt * (Pcr - 0.06 * crcl(tg[i-1]) * Cr[i-1]) / Vcr
cat(sprintf("Creatinine pool half-life: %.1f h at CrCl 60, %.1f h at the %.0f plateau\n",
            log(2)*Vcr/(0.06*60), log(2)*Vcr/(0.06*plateau), plateau))

# ---- meropenem --------------------------------------------------------------
rate_fun <- function(t, starts, dose, tinf) {
  r <- numeric(length(t)); for (s in starts) r <- r + ifelse(t >= s & t < s + tinf, dose/tinf, 0); r
}
sim <- function(rate) {
  A <- numeric(length(tg)); CLv <- CLt(tg)
  for (i in 2:length(tg)) A[i] <- A[i-1] + dt * (rate[i-1] - CLv[i-1]/V * A[i-1])
  A/V
}
tab_starts <- seq(0, 47.9, by = 12); fix_starts <- seq(8, 47.9, by = 8)
C_tab <- sim(rate_fun(tg, tab_starts, 1000, 0.5))
C_fix <- sim(rate_fun(tg, 0, 2000, 0.5) + rate_fun(tg, fix_starts, 1000, 3))
C_hi  <- sim(rate_fun(tg, 0, 2000, 0.5) + rate_fun(tg, fix_starts, 2000, 3))
C_q8b <- sim(rate_fun(tg, seq(0, 47.9, by = 8), 1000, 0.5))
d <- tibble(t = tg, table_dose = C_tab, fix = C_fix, hidose = C_hi, CrCl = crcl(tg), Cr = Cr/10)

# ---- summary ----------------------------------------------------------------
fa <- function(C, lo, hi, thr) mean(C[tg >= lo & tg <= hi] > thr)
iv <- tibble(lo = tab_starts, hi = tab_starts + 12) |>
  mutate(lo_sc = ifelse(lo == 0, 0.5, lo),
         ft  = sapply(seq_along(lo), \(i) fa(C_tab, lo_sc[i], hi[i], MIC)),
         ft4 = sapply(seq_along(lo), \(i) fa(C_tab, lo_sc[i], hi[i], MIC4)))
last24 <- function(C, thr) fa(C, 36, 48, thr)
labs_daily <- tibble(t = c(0, 24, 48), cr = Cr[c(1, which(tg == 24), length(tg))]/10) |>
  mutate(cg = CG(cr))
cat(sprintf("CrCl 0/24/48 h: %.0f / %.0f / %.0f mL/min; CL %.1f -> %.1f L/h (t1/2 %.2f -> %.2f h)\n",
            crcl(0), crcl(24), crcl(48), CLt(0), CLt(48), log(2)*V/CLt(0), log(2)*V/CLt(48)))
print(labs_daily |> mutate(across(everything(), \(x) round(x, 2))))
print(iv |> mutate(across(c(ft, ft4), \(x) round(100*x, 1))))
cat(sprintf("Final 12-h interval, fT>MIC: 1 g q12h %.0f%% | 1 g q8h 30-min %.0f%% | 1 g q8h 3-h %.0f%% | 2 g q8h 3-h %.0f%%\n",
            100*last24(C_tab, MIC), 100*last24(C_q8b, MIC), 100*last24(C_fix, MIC), 100*last24(C_hi, MIC)))
cat(sprintf("Final 12-h interval, fT>4xMIC: 1 g q12h %.0f%% | 1 g q8h 3-h %.0f%%\n",
            100*last24(C_tab, MIC4), 100*last24(C_fix, MIC4)))
post <- tg >= 0.5
h_below <- sum(C_tab[post] <= MIC)*dt; first_below <- tg[post][which(C_tab[post] <= MIC)[1]]
cat(sprintf("1 g q12h: %.1f h below MIC of 48, first crossing %.1f h\n", h_below, first_below))
write.csv(d |> filter(abs(t %% 0.02) < 1e-9), "figures/aki_recovery_sim.csv", row.names = FALSE)

# ---- frames -----------------------------------------------------------------
lab_int <- iv |> mutate(mid = lo + 6, lab = paste0(round(100*ft), "%"),
                        col = ifelse(ft > 0.999, col_navy, col_red))
sc <- 60   # creatinine (mg/dL) -> CrCl axis

make_frame <- function(tmax, shade = FALSE, comp = FALSE, summary = FALSE, hi = FALSE) {
  dd <- d |> filter(t <= tmax, t >= 0.01)
  p1 <- ggplot(dd, aes(t)) +
    annotate("rect", xmin = 0, xmax = 48, ymin = 0.25, ymax = MIC, fill = "#fbeeee") +
    geom_hline(yintercept = MIC, color = col_red, linewidth = 0.8) +
    geom_hline(yintercept = MIC4, color = col_grey, linewidth = 0.6, linetype = "dashed") +
    annotate("text", x = 47.6, y = MIC*0.78, label = "MIC 2 mg/L", hjust = 1, size = 4.6, color = col_red, fontface = "bold") +
    annotate("text", x = 47.6, y = MIC4*1.20, label = "4 x MIC", hjust = 1, size = 4.2, color = col_grey)
  if (shade) {
    bel <- dd |> filter(table_dose <= MIC, t > 1)
    if (nrow(bel)) p1 <- p1 + geom_ribbon(data = bel, aes(x = t, ymin = pmax(table_dose, 0.25), ymax = MIC),
                                          fill = col_red, alpha = 0.30)
  }
  p1 <- p1 + geom_line(aes(y = table_dose), color = col_red, linewidth = 1.25) +
    scale_y_log10(limits = c(0.25, 190), breaks = c(0.5, 1, 2, 5, 10, 20, 50, 100),
                  labels = c("0.5", "1", "2", "5", "10", "20", "50", "100")) +
    scale_x_continuous(limits = c(0, 48), breaks = seq(0, 48, 12), labels = \(x) paste0(x, " h"), expand = c(0.01, 0)) +
    labs(x = NULL, y = "Meropenem (mg/L)",
         title = if (summary)
           sprintf("The dose never changed: fT>MIC falls from %.0f%% to %.0f%% of the interval as her kidneys recover",
                   100*iv$ft[1], 100*iv$ft[4]) else "Unbound meropenem concentration",
         subtitle = "Numbers above each 12-hour interval: fraction of that interval above MIC on the table dose") +
    base_theme(17) +
    theme(axis.text.x = element_blank(),
          plot.title = element_text(face = "bold", size = 16.2, color = if (summary) col_red else col_navy))
  done <- lab_int |> filter(hi <= tmax + 1e-6)
  if (nrow(done)) p1 <- p1 + geom_text(data = done, aes(x = mid, y = 165, label = lab, color = col),
                                       size = 5.4, fontface = "bold", inherit.aes = FALSE) + scale_color_identity()
  p1 <- p1 + annotate("label", x = 0.3, y = 0.33, hjust = 0, size = 4.6, fontface = "bold", color = col_red,
                      fill = "white", label.r = unit(0, "pt"),
                      label = "Renal-dosing table: 1 g q12h, fixed on the admission creatinine (2 g/day)")

  klab <- labs_daily |> filter(t <= tmax + 1e-6)
  p2 <- ggplot(dd, aes(t)) +
    geom_line(aes(y = CrCl), color = col_navy, linewidth = 1.3) +
    geom_line(aes(y = Cr*sc), color = col_red, linewidth = 1.0, linetype = "22") +
    geom_point(data = klab, aes(x = t, y = cr*sc), color = col_red, size = 4.2, inherit.aes = FALSE) +
    scale_x_continuous(limits = c(0, 48), breaks = seq(0, 48, 12), labels = \(x) paste0(x, " h"), expand = c(0.01, 0)) +
    scale_y_continuous(limits = c(0, 215), breaks = c(0, 60, 120, 180),
                       sec.axis = sec_axis(~./sc, name = "Creatinine (mg/dL)", breaks = c(0, 1, 2, 3))) +
    labs(x = "Time from the first dose", y = "CrCl (mL/min)",
         title = "Measured creatinine clearance (navy) and serum creatinine (red, dashed; dots = daily laboratory value)",
         subtitle = if (tmax >= 23.9)
           sprintf("Hour 24, the only creatinine anyone sees: %.1f mg/dL, Cockcroft-Gault %.0f mL/min, so the table keeps 1 g q12h.\nHer measured clearance that morning is already %.0f mL/min, and rising.",
                   labs_daily$cr[2], labs_daily$cg[2], crcl(24)) else " \n ") +
    base_theme(17) +
    theme(plot.title = element_text(size = 13.5, face = "bold", color = col_navy),
          plot.subtitle = element_text(size = 12.4, color = col_navy),
          axis.title.y.right = element_text(color = col_red), axis.text.y.right = element_text(color = col_red))
  if (nrow(klab)) {
    off <- c(22, -26, 22)[seq_len(nrow(klab))]; hj <- c(0, 0, 1)[seq_len(nrow(klab))]
    p2 <- p2 + geom_text(data = klab |> mutate(off = off, hj = hj),
                         aes(x = t + ifelse(hj == 0, 0.8, -0.8), y = cr*sc + off,
                             label = sprintf("%.1f mg/dL", cr), hjust = hj),
                         inherit.aes = FALSE, size = 4.2, color = col_red, fontface = "bold")
  }
  (p1 / p2) + plot_layout(heights = c(2.0, 1.3))
}

frames <- list(list(tmax = 12), list(tmax = 24), list(tmax = 36), list(tmax = 48),
               list(tmax = 48, shade = TRUE), list(tmax = 48, shade = TRUE, summary = TRUE))
for (i in seq_along(frames)) {
  a <- frames[[i]]
  ggsave(sprintf("figures/aki_sim_%02d.png", i),
         make_frame(a$tmax, isTRUE(a$shade), isTRUE(a$comp), isTRUE(a$summary), isTRUE(a$hi)),
         width = 12.5, height = 6.6, dpi = 300, bg = "white")
}
cat("frames written\n")
