# =============================================================================
# Section 3. WHY hypoalbuminaemia hurts once-daily highly bound beta-lactams
# and not q6h or continuous dosing.
#
# MECHANISTIC SIMULATION - not data. The structural premise is Benet & Hoener
# 2002: for a restrictively cleared, low-extraction drug the UNBOUND intrinsic
# clearance CLu is a property of the eliminating organ and does not change when
# binding changes. Everything else follows:
#     CL = fu x CLu            total clearance rises as fu rises
#     V  = Vb + fu x Vt        volume rises as fu rises
#     k  = CL/V                so the unbound curve decays FASTER
#     AUCu = Dose / CLu        average unbound exposure is INDEPENDENT of fu
# The average is therefore preserved and the peak-to-trough swing widens, so
# the loss falls entirely on the trough - which only matters when the dosing
# interval is long relative to the half-life.
#
# Exemplar: ertapenem 1 g daily (highly bound, renally and restrictively
# cleared, the drug IDSA 2024 advises against in hypoalbuminaemic critically
# ill patients). ASSUMED BY ME: Vb 4 L, Vt 80 L, CLu 27.7 L/h, chosen so that
# fu = 0.05 reproduces the textbook half-life of about 4 h; fu 0.05 -> 0.15 is
# the shift from normal albumin to ICU hypoalbuminaemia (binding 95% -> 85%),
# within the range on the unbound-fraction slide. MIC 0.5 mg/L (EUCAST
# ertapenem, Enterobacterales). Target 40% fT>MIC (classical carbapenem target).
# Regimens are MATCHED at 1 g/day so the contrast isolates the interval.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(ggplot2); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 17) theme_minimal(base_size = bs) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = col_navy, size = bs * 0.95),
        plot.subtitle = element_text(color = col_dark, size = bs * 0.75),
        plot.margin = margin(6, 10, 4, 8))

Vb <- 4; Vt <- 80; CLu <- 27.7; MIC <- 0.5; DAILY <- 1000
fu_n <- 0.05; fu_l <- 0.15
dt <- 0.002; tg <- seq(0, 120, by = dt); last <- tg >= 96        # steady state, final 24 h

simulate <- function(fu, dose, tau, tinf) {
  V <- Vb + fu*Vt; CL <- fu*CLu; k <- CL/V
  starts <- seq(0, 119.9, by = tau)
  R <- numeric(length(tg)); for (s in starts) R <- R + ifelse(tg >= s & tg < s + tinf, dose/tinf, 0)
  A <- numeric(length(tg))
  for (i in 2:length(tg)) A[i] <- A[i-1] + dt*(R[i-1] - k*A[i-1])
  list(cu = fu*A/V, k = k, V = V, CL = CL)
}
summ <- function(s) {
  cu <- s$cu[last]
  c(ft = mean(cu > MIC), cmin = min(cu), cmax = max(cu),
    aucu = sum(cu)*dt, cavg = mean(cu), thalf = log(2)/s$k)
}

# ---- Panel A: 1 g q24h, normal vs low albumin -------------------------------
A_n <- simulate(fu_n, 1000, 24, 0.5); A_l <- simulate(fu_l, 1000, 24, 0.5)
sn <- summ(A_n); sl <- summ(A_l)
cat("1 g q24h\n")
cat(sprintf("  fu %.2f: t1/2 %.1f h, CL %.2f L/h, V %.1f L | AUCu(24h) %.1f mg.h/L, Cu,avg %.2f, Cu,min %.3f, fT>MIC %.0f%%\n",
            fu_n, sn["thalf"], A_n$CL, A_n$V, sn["aucu"], sn["cavg"], sn["cmin"], 100*sn["ft"]))
cat(sprintf("  fu %.2f: t1/2 %.1f h, CL %.2f L/h, V %.1f L | AUCu(24h) %.1f mg.h/L, Cu,avg %.2f, Cu,min %.3f, fT>MIC %.0f%%\n",
            fu_l, sl["thalf"], A_l$CL, A_l$V, sl["aucu"], sl["cavg"], sl["cmin"], 100*sl["ft"]))
cat(sprintf("  AUCu ratio low/normal = %.3f (theory: 1.000); trough ratio = %.2f\n",
            sl["aucu"]/sn["aucu"], sl["cmin"]/sn["cmin"]))

# ---- Panel B: same 1 g/day, five intervals, both albumin states -------------
regs <- tribble(~lab, ~dose, ~tau, ~tinf,
  "1 g q24h",            1000,   24,  0.5,
  "500 mg q12h",          500,   12,  0.5,
  "333 mg q8h",        1000/3,    8,  0.5,
  "250 mg q6h",           250,    6,  0.5,
  "1 g/24 h continuous", 1000,   24, 24)
g <- regs |> rowwise() |>
  mutate(n_min = summ(simulate(fu_n, dose, tau, tinf))["cmin"],
         l_min = summ(simulate(fu_l, dose, tau, tinf))["cmin"],
         n_ft  = 100*summ(simulate(fu_n, dose, tau, tinf))["ft"],
         l_ft  = 100*summ(simulate(fu_l, dose, tau, tinf))["ft"]) |> ungroup() |>
  mutate(ratio = l_min/n_min, lab = factor(lab, rev(regs$lab)))
print(as.data.frame(g |> mutate(across(where(is.numeric), \(x) round(x, 3)))), row.names = FALSE)
write.csv(g, "figures/binding_trough_regimens.csv", row.names = FALSE)
cat(sprintf("Average unbound concentration, every regimen and both albumin states: %.2f mg/L\n", sn["cavg"]))

# ---- figure -----------------------------------------------------------------
tw <- tibble(t = tg[last] - 96, normal = A_n$cu[last], low = A_l$cu[last]) |>
  pivot_longer(-t, names_to = "alb", values_to = "cu")
pA <- ggplot(tw, aes(t, cu, color = alb)) +
  annotate("rect", xmin = 0, xmax = 24, ymin = 0.004, ymax = MIC, fill = "#fbeeee") +
  geom_hline(yintercept = sn["cavg"], linetype = "22", color = col_navy, linewidth = 0.9) +
  geom_hline(yintercept = MIC, color = col_red, linewidth = 0.8) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = c(normal = col_blue, low = col_red), guide = "none") +
  scale_y_log10(limits = c(0.004, 30), breaks = c(0.01, 0.1, 0.5, 1, 5, 10),
                labels = c("0.01", "0.1", "0.5", "1", "5", "10")) +
  scale_x_continuous(breaks = seq(0, 24, 6), labels = \(x) paste0(x, " h"), expand = c(0.01, 0)) +
  annotate("text", x = 23.5, y = sn["cavg"]*1.5, hjust = 1, size = 4.5, color = col_navy, fontface = "bold",
           label = sprintf("Average unbound %.2f mg/L - identical", sn["cavg"])) +
  annotate("text", x = 23.5, y = MIC*0.72, hjust = 1, size = 4.3, color = col_red, fontface = "bold",
           label = "MIC 0.5 mg/L") +
  annotate("text", x = 0.4, y = 0.095, hjust = 0, size = 4.5, color = col_blue, fontface = "bold",
           label = sprintf("Albumin normal, unbound 5%%:  trough %.2f, fT>MIC %.0f%%", sn["cmin"], 100*sn["ft"])) +
  annotate("text", x = 0.4, y = 0.025, hjust = 0, size = 4.5, color = col_red, fontface = "bold",
           label = sprintf("Albumin low, unbound 15%%:  trough %.2f, fT>MIC %.0f%%", sl["cmin"], 100*sl["ft"])) +
  labs(x = "Time through one dosing interval, steady state", y = "Unbound ertapenem (mg/L)",
       title = "Ertapenem 1 g once daily",
       subtitle = "Binding falls, clearance and volume both rise: the average holds, the trough does not") +
  base_theme(17)

pB <- ggplot(g, aes(y = lab)) +
  annotate("rect", xmin = 0.004, xmax = MIC, ymin = 0.4, ymax = 5.75, fill = "#fbeeee") +
  geom_vline(xintercept = MIC, color = col_red, linewidth = 0.8) +
  geom_vline(xintercept = sn["cavg"], linetype = "22", color = col_navy, linewidth = 0.9) +
  geom_segment(aes(x = l_min, xend = n_min, yend = lab), color = col_grey, linewidth = 1.6) +
  geom_point(aes(x = n_min), color = col_blue, size = 6.2) +
  geom_point(aes(x = l_min), color = col_red, size = 4.0) +
  geom_text(aes(x = 3.4, label = sprintf("%.0f%% to %.0f%%", n_ft, l_ft)),
            hjust = 0, size = 4.4, color = col_dark) +
  annotate("text", x = 3.4, y = 5.62, hjust = 0, size = 4.2, color = col_dark, fontface = "bold", label = "fT>MIC") +
  scale_x_log10(limits = c(0.012, 26), breaks = c(0.02, 0.1, 0.5, 1.5),
                labels = c("0.02", "0.1", "0.5", "1.5")) +
  labs(x = "Unbound trough (mg/L, log scale)", y = NULL,
       title = "Same 1 g a day, five ways to give it",
       subtitle = "Blue = albumin normal, red = albumin low") +
  base_theme(17) + theme(panel.grid.major.y = element_blank(),
                         axis.text.y = element_text(size = 14, color = col_navy))

p <- pA + pB + plot_layout(widths = c(1.05, 1))
ggsave("figures/binding_trough_mechanism.png", p, width = 13.6, height = 6.0, dpi = 300, bg = "white")
cat("figure written\n")
