# =============================================================================
# Slide 9. Day-1 meropenem target attainment, ACLF vs no liver failure, on CVVHD
# (Grensemann 2020; n = 19, 8 with ACLF; Monte Carlo from the fitted model).
#   Target: 100 % fT > 4 x ECOFF (8 mg/L, Pseudomonas aeruginosa), day 1
#   1 g q8h, short infusion:                ACLF 18 %   no liver failure 48 %
#   2 g loading dose + 1 g q8h prolonged:   ACLF 94 %   no liver failure 91 %
#   Peripheral volume 38.6 L (ACLF) vs 19.7 L.
# Published abstract values; simulation is the authors', not ours.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 17) theme_minimal(base_size = bs) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", color = col_navy, size = bs * 0.95), plot.subtitle = element_text(color = col_dark, size = bs * 0.75), plot.margin = margin(8, 10, 8, 8))

d <- tribble(~regimen, ~grp, ~pta,
  "1 g q8h\nshort infusion",              "ACLF",             18,
  "1 g q8h\nshort infusion",              "No liver failure", 48,
  "2 g loading dose +\n1 g q8h prolonged", "ACLF",             94,
  "2 g loading dose +\n1 g q8h prolonged", "No liver failure", 91) |>
  mutate(regimen = factor(regimen, unique(regimen)), grp = factor(grp, c("No liver failure", "ACLF")))
p <- ggplot(d, aes(regimen, pta, fill = grp)) +
  geom_col(position = position_dodge(0.7), width = 0.62) +
  geom_hline(yintercept = 90, linetype = "dotted", color = col_dark) +
  annotate("text", x = 0.5, y = 92.5, label = "90%", hjust = 0, size = 4, color = col_dark) +
  geom_text(aes(label = paste0(pta, "%")), position = position_dodge(0.7), vjust = -0.4, size = 5.6, fontface = "bold", color = col_navy) +
  scale_fill_manual(values = c("No liver failure" = col_grey, "ACLF" = col_blue), name = NULL,
                    labels = c("No liver failure (peripheral V 19.7 L)", "Acute-on-chronic liver failure (V 38.6 L)")) +
  scale_y_continuous(limits = c(0, 108), breaks = seq(0, 100, 25), expand = c(0, 0), labels = \(x) paste0(x, "%")) +
  labs(x = NULL, y = "Day-1 probability of target attainment\n(100% fT > 8 mg/L)",
       title = "A loading dose closes the day-1 gap created by the larger distribution volume",
       subtitle = "Meropenem on continuous haemodialysis; 19 patients, 8 with ACLF (Grensemann 2020)") +
  base_theme(17) + theme(legend.position = "bottom", panel.grid.major.x = element_blank(),
                         axis.text.x = element_text(size = 15, lineheight = 0.9))
ggsave("figures/aclf_day1_pta.png", p, width = 12.5, height = 5.6, dpi = 200, bg = "white")
