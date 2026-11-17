# =============================================================================
# Does continuous infusion really beat a 3-4 h extended infusion?
# MECHANISTIC SIMULATION (not patient data). This figure exists to show that the
# answer depends almost entirely on the TARGET DEFINITION and the DOSING INTERVAL,
# not on "continuous vs extended" as such.
#   Structural model: one compartment, typical CL 8.35 L/h, V 28.2 L
#                     (Bastida 2020, JAC: decompensated cirrhosis, n = 54)
#   ASSUMED by me:    30% log-normal between-patient variability on CL and V;
#                     unbound = total (meropenem binding is low); MIC 2 mg/L;
#                     high-clearance stratum = 2x typical CL (t1/2 1.17 h vs 2.34 h)
#   n = 20,000 virtual patients per cell.
# Regimens compared at MATCHED daily dose where possible.
# Key point for the talk: at 50% fT>MIC every prolonged regimen is equivalent;
# the apparent superiority of continuous infusion appears only at the stringent
# 100% fT>MIC (and more so at 4xMIC) target, and shrinks when the interval is
# shortened from q8h to q6h - i.e. it is a trough phenomenon, not an infusion-mode
# phenomenon. Published comparisons of extended vs continuous infusion that use
# 50-60% fT>MIC, q6h intervals, or higher doses find little or no difference.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(ggplot2)})
set.seed(2026)
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"

n <- 20000; CL_tv <- 8.35; V_tv <- 28.2; cv <- 0.30; MIC <- 2

fT <- function(CL, V, dose, tau, tinf, Ct) {   # fraction of interval above Ct, steady state
  k <- CL/V; R0 <- dose/tinf
  cmin <- (R0/CL)*(1-exp(-k*tinf))/(1-exp(-k*tau))*exp(-k*(tau-tinf))
  cmax <- (R0/CL)*(1-exp(-k*tinf)) + cmin*exp(-k*tinf)
  out <- numeric(length(CL)); out[cmin >= Ct] <- 1
  idx <- (cmin < Ct) & (cmax > Ct)
  if (any(idx)) {
    kk <- k[idx]; A <- R0/CL[idx]; arg <- (Ct - A)/(cmin[idx] - A)
    t1 <- pmin(pmax(ifelse(arg > 0 & arg < 1, -log(arg)/kk, 0), 0), tinf)
    t2 <- pmin(pmax(log(cmax[idx]/Ct)/kk, 0), tau - tinf)
    out[idx] <- ((tinf - t1) + t2)/tau
  }
  out
}

# All arms deliver the SAME 3 g/day, so the comparison isolates infusion mode and
# dosing interval from dose. (A q6h arm at 1 g would give 4 g/day and confound the two.)
regs <- tribble(~regimen, ~dose, ~tau, ~tinf, ~daily,
  "1 g q8h\n0.5 h",       1000,  8, 0.5, 3,
  "1 g q8h\n3 h",         1000,  8, 3,   3,
  "0.75 g q6h\n3 h",       750,  6, 3,   3,
  "3 g/24 h\ncontinuous",   NA, NA, NA,  3)
targets <- tribble(~target, ~frac, ~mult, "50% fT>MIC", 0.5, 1, "100% fT>MIC", 1.0, 1, "100% fT>4xMIC", 1.0, 4)
strata <- tibble(stratum = c("Typical CL\n8.4 L/h", "High CL\n16.7 L/h"), mult = c(1, 2))

res <- list()
for (s in seq_len(nrow(strata))) {
  CL <- CL_tv*strata$mult[s]*exp(rnorm(n,0,cv)); V <- V_tv*exp(rnorm(n,0,cv))
  for (r in seq_len(nrow(regs))) for (t in seq_len(nrow(targets))) {
    Ct <- MIC*targets$mult[t]
    pta <- if (is.na(regs$tau[r])) mean(((regs$daily[r]*1000/24)/CL) > Ct)
           else mean(fT(CL, V, regs$dose[r], regs$tau[r], regs$tinf[r], Ct) >= targets$frac[t] - 1e-9)
    res[[length(res)+1]] <- tibble(stratum = strata$stratum[s], regimen = regs$regimen[r],
                                   target = targets$target[t], PTA = 100*pta)
  }
}
d <- bind_rows(res) |>
  mutate(stratum = factor(stratum, strata$stratum),
         regimen = factor(regimen, regs$regimen),
         target  = factor(target, targets$target))
write.csv(d, "figures/infusion_benefit_by_clearance.csv", row.names = FALSE)

p <- ggplot(d, aes(regimen, PTA, fill = regimen)) +
  geom_hline(yintercept = 90, linetype = "dotted", color = col_dark) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.0f", PTA)), vjust = -0.35, size = 4.1, fontface = "bold", color = col_navy) +
  facet_grid(stratum ~ target, switch = "y") +
  scale_fill_manual(values = c(col_grey, col_blue, "#5fa8d3", col_navy), guide = "none") +
  scale_y_continuous(limits = c(0, 118), breaks = seq(0, 100, 50), expand = c(0, 0), labels = \(x) paste0(x, "%")) +
  labs(x = NULL, y = "Probability of target attainment (MIC 2 mg/L)",
       title = "Extended and continuous infusion separate only under the strictest target",
       subtitle = "Meropenem, every arm 3 g/day; 20,000 simulated patients per cell; model from Bastida 2020, variability assumed",
       caption = "At 50% fT>MIC all three prolonged regimens are equivalent. The gap at 100% fT>MIC is a TROUGH effect, not an infusion-mode effect:\nat the same daily dose, shortening the interval from q8h to q6h closes much of it. Published extended-vs-continuous comparisons\nthat use 50-60% fT>MIC, or q6h dosing, accordingly find little or no difference.") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        strip.text.x = element_text(face = "bold", color = col_navy, size = 13),
        strip.text.y.left = element_text(face = "bold", color = col_navy, size = 12, angle = 0, lineheight = 0.9),
        strip.placement = "outside",
        axis.text.x = element_text(size = 10.5, lineheight = 0.9, color = col_dark),
        plot.title = element_text(face = "bold", color = col_navy, size = 16.5),
        plot.subtitle = element_text(color = col_dark, size = 12),
        plot.caption = element_text(hjust = 0, size = 10.5, color = col_red, lineheight = 1.15, margin = margin(t = 10)),
        panel.spacing = unit(1.1, "lines"), plot.margin = margin(8, 10, 8, 8))
ggsave("figures/infusion_benefit_by_clearance.png", p, width = 13, height = 7.2, dpi = 200, bg = "white")
