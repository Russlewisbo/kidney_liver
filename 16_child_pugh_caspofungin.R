# =============================================================================
# Caspofungin slide. Why the Child-Pugh score misleads in the ICU, and what the
# label's Child-Pugh B dose reduction costs. Published values only; no simulation
# of our own (the right panel reports the authors' own Monte Carlo output).
#
# LEFT  - Kurland 2019, AAC 63:e02466-18 (prospective, 46 ICU patients on
#         caspofungin; Child-Pugh A 5, B 40, C 1). At steady state (day 5),
#         correlation with AUC(0-24): Child-Pugh score r = 0.03 (P = 0.84);
#         plasma albumin r = 0.45 (P = 0.005) - i.e. LOW albumin went with LOW
#         exposure. Authors' conclusion, verbatim: "The caspofungin dose should
#         not be reduced in critically ill patients in the absence of cirrhosis,
#         and we advise against the use of the C-P score in patients with trauma-
#         or sepsis-induced liver injury."
#
# RIGHT - Martial 2016, Clin Pharmacokinet 55:723-33 (popPK from 21 ICU patients
#         + Monte Carlo). Median AUC(0-24) on day 14 and target attainment at
#         MIC 0.125 ug/mL for five regimens (loading/maintenance, mg):
#           I   70/50 or 70/70 by weight (licensed standard)  AUC 105, PTA 47%
#           II  70/35 (licensed Child-Pugh B reduction)       AUC  65, PTA 14%
#           III 100/50                                        AUC  93, PTA 36%
#           IV  100/70                                        AUC 130, PTA 69%
#           V   100/100                                       AUC 186, PTA 94%
#         At MIC 0.03 ug/mL attainment was 100% for every regimen.
#         Authors' conclusion: the maintenance dose should not be reduced in
#         non-cirrhotic ICU patients when Child-Pugh B is driven by
#         hypoalbuminaemia, as it results in significantly lower exposure.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 16) theme_minimal(base_size = bs) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", color = col_navy, size = bs*0.98), plot.subtitle = element_text(color = col_dark, size = bs*0.78), plot.margin = margin(8, 10, 8, 8))

r <- tibble(var = factor(c("Child-Pugh score", "Serum albumin"), c("Child-Pugh score", "Serum albumin")),
            r = c(0.03, 0.45), lab = c("r = 0.03\nP = 0.84", "r = 0.45\nP = 0.005"))
p1 <- ggplot(r, aes(var, r)) +
  geom_hline(yintercept = 0, color = col_dark) +
  geom_col(fill = c(col_grey, col_blue), width = 0.55) +
  geom_text(aes(label = lab), vjust = -0.3, size = 4.7, lineheight = 0.9, color = col_navy, fontface = "bold") +
  scale_y_continuous(limits = c(-0.03, 0.68), breaks = seq(0, 0.6, 0.2), expand = c(0, 0)) +
  labs(x = NULL, y = "Correlation with caspofungin AUC(0-24)",
       title = "The score does not predict exposure; albumin does",
       subtitle = "46 ICU patients, 40 of them Child-Pugh B, so the label would\nreduce the dose in most. Low albumin went with LOW exposure.") +
  base_theme(16) + theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 14, color = col_navy))

m <- tribble(~reg, ~auc, ~pta, ~kind,
  "70/50-70\nlicensed\nstandard",   105, 47, "std",
  "70/35\nlabel's\nChild-Pugh B",    65, 14, "cut",
  "100/50",                         93, 36, "alt",
  "100/70",                        130, 69, "alt",
  "100/100",                       186, 94, "alt") |>
  mutate(reg = factor(reg, reg))
p2 <- ggplot(m, aes(reg, pta, fill = kind)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = paste0(pta, "%")), vjust = -0.35, size = 5, fontface = "bold", color = col_navy) +
  geom_text(aes(y = 3, label = paste0("AUC ", auc)), size = 3.9, color = "white", fontface = "bold", vjust = 0) +
  scale_fill_manual(values = c(std = col_grey, cut = col_red, alt = col_blue), guide = "none") +
  scale_y_continuous(limits = c(0, 112), breaks = seq(0, 100, 25), expand = c(0, 0), labels = \(x) paste0(x, "%")) +
  labs(x = "Loading / maintenance dose (mg)", y = "Target attainment at MIC 0.125 \u00b5g/mL",
       title = "The label's Child-Pugh B reduction loses a third of the exposure",
       subtitle = "AUC falls 105 to 65 mg\u00b7h/L and attainment 47% to 14%. Simulated day-14\nAUC(0-24) printed inside each bar; at MIC 0.03 every regimen attained 100%.") +
  base_theme(16) + theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 10.5, lineheight = 0.95))

p <- p1 + p2 + plot_layout(widths = c(1, 1.45))
ggsave("figures/child_pugh_caspofungin.png", p, width = 13.4, height = 5.9, dpi = 200, bg = "white")
