# =============================================================================
# Section 2 slide. Creatinine-based equations vs measured 8-h creatinine clearance
# in 54 ICU patients with normal serum creatinine, 644 paired samples
# (Baptista 2014, J Nephrol 27:403-410).
# Left:  medians by 12 nested cut-off subgroups of measured CLcr, digitised from
#        the published Fig. 3 at 300 dpi (bar-top detection; resolution ~0.3 mL/min;
#        values in figures/baptista2014_fig3_digitised.csv). Plotted as estimated
#        vs measured median against the identity line.
# Right: Bland-Altman bias and limits of agreement (% of the pair average) as
#        PRINTED on the published Fig. 2: CG 0.8 (-81.1 to +82.7); MDRD 6.8
#        (-72.9 to +86.4); CKD-EPI 17.6 (-49.3 to +84.4).
# Abstract: overall means 135.5 (8h-CLcr) vs 135.7 (CG); R = 0.2, 0.19, 0.34.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(tidyr); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 17) theme_minimal(base_size = bs) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", color = col_navy, size = bs * 0.95), plot.subtitle = element_text(color = col_dark, size = bs * 0.75), plot.margin = margin(8, 10, 8, 8))

d <- read.csv("figures/baptista2014_fig3_digitised.csv") |>
  pivot_longer(c(CG, MDRD, CKDEPI), names_to = "eq", values_to = "est") |>
  mutate(eq = factor(recode(eq, CKDEPI = "CKD-EPI"), c("CG", "MDRD", "CKD-EPI")))
p1 <- ggplot(d, aes(CLCr, est, color = eq)) +
  annotate("rect", xmin = 60, xmax = 120, ymin = 60, ymax = 220, fill = col_red, alpha = 0.05) +
  annotate("rect", xmin = 120, xmax = 220, ymin = 60, ymax = 220, fill = col_blue, alpha = 0.05) +
  geom_abline(slope = 1, intercept = 0, color = col_dark, linetype = "dashed") +
  geom_line(linewidth = 1) + geom_point(size = 2.8) +
  annotate("text", x = 62, y = 212, label = "measured < 120:\nequations OVER-estimate", hjust = 0, vjust = 1, size = 4.2, color = col_red, lineheight = 0.9) +
  annotate("text", x = 218, y = 66, label = "measured > 120:\nequations UNDER-estimate", hjust = 1, vjust = 0, size = 4.2, color = col_blue, lineheight = 0.9) +
  annotate("text", x = 200, y = 205, label = "identity", angle = 38, size = 3.8, color = col_dark) +
  scale_color_manual(values = c("CG" = col_navy, "MDRD" = col_blue, "CKD-EPI" = col_grey), name = NULL) +
  scale_x_continuous(limits = c(60, 220), breaks = seq(75, 200, 25)) +
  scale_y_continuous(limits = c(60, 220), breaks = seq(75, 200, 25)) +
  coord_equal() +
  labs(x = "Measured 8-h creatinine clearance, median (mL/min/1.73 m\u00b2)",
       y = "Estimated GFR, median (mL/min/1.73 m\u00b2)",
       title = "Medians by cut-off of measured clearance",
       subtitle = "644 paired samples, 54 ICU patients with normal creatinine; redrawn from Fig. 3") +
  base_theme(15) + theme(legend.position = "bottom", legend.text = element_text(size = 12.5))

ba <- tibble(eq = factor(c("CG", "MDRD", "CKD-EPI"), rev(c("CG", "MDRD", "CKD-EPI"))),
             bias = c(0.8, 6.8, 17.6), lo = c(-81.1, -72.9, -49.3), hi = c(82.7, 86.4, 84.4))
p2 <- ggplot(ba, aes(y = eq)) +
  geom_vline(xintercept = 0, color = col_dark) +
  geom_segment(aes(x = lo, xend = hi, yend = eq), linewidth = 7, color = col_grey, lineend = "round") +
  geom_point(aes(x = bias), size = 5, shape = 21, fill = col_navy, color = "white", stroke = 1.2) +
  geom_text(aes(x = lo, label = sprintf("%.1f", lo)), hjust = 1.35, size = 4.2, color = col_dark) +
  geom_text(aes(x = hi, label = sprintf("+%.1f", hi)), hjust = -0.35, size = 4.2, color = col_dark) +
  geom_text(aes(x = bias, label = sprintf("bias %+.1f%%", bias)), vjust = -1.4, size = 4.2, color = col_navy, fontface = "bold") +
  scale_x_continuous(limits = c(-115, 115), breaks = seq(-100, 100, 50), labels = \(x) paste0(x, "%")) +
  scale_y_discrete(expand = expansion(add = c(0.6, 0.8))) +
  labs(x = "Estimate minus measured, % of pair average", y = NULL,
       title = "Bias near zero, limits of agreement \u00b180%",
       subtitle = "Bland-Altman values as printed in Fig. 2; mean 135.5 vs 135.7 (CG), R = 0.2") +
  base_theme(15) + theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 14, face = "bold", color = col_navy))
p <- p1 + p2 + plot_layout(widths = c(1.05, 1))
ggsave("figures/gfr_estimates_baptista.png", p, width = 13.5, height = 6.2, dpi = 200, bg = "white")
