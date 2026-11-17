# =============================================================================
# Section 3 slide. Can a total concentration be corrected for albumin, as taught
# for phenytoin (Sheiner-Tozer)? Published summary statistics only.
# Left:  Wong 2013 (AAC 57:6165; ICU patients): bias of unbound concentration
#        predicted from PUBLISHED protein-binding values vs measured unbound:
#        ceftriaxone +83.3 %, flucloxacillin +56.8 %, benzylpenicillin +25 %
#        (over-prediction), meropenem -12.1 % (under-prediction). Measured total
#        and unbound were linearly related for all drugs EXCEPT ceftriaxone and
#        flucloxacillin (saturable binding).
# Right: phenytoin, the textbook case. Cheng/Kiang/Ensom 2016 (CJHP 69(4)):
#        only albumin (R2 = 0.09) and total phenytoin (R2 = 0.53) correlated with
#        free phenytoin; all equations performed poorly in critical care.
#        Parikh & MacLaren 2018 (J Crit Care 44:95): four correction equations in
#        a medical ICU, R2 < 0.4 for all; biased and imprecise.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 17) theme_minimal(base_size = bs) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", color = col_navy, size = bs * 0.95), plot.subtitle = element_text(color = col_dark, size = bs * 0.75), plot.margin = margin(8, 10, 8, 8))

w <- tibble(drug = c("Ceftriaxone", "Flucloxacillin", "Benzylpenicillin", "Meropenem"),
            bias = c(83.3, 56.8, 25, -12.1),
            binding = c("saturable", "saturable", "linear", "linear")) |>
  mutate(drug = factor(drug, rev(drug)), lab = sprintf("%+.1f%%", bias))
p1 <- ggplot(w, aes(bias, drug, fill = binding)) +
  geom_vline(xintercept = 0, color = col_dark) +
  geom_col(width = 0.62) +
  geom_text(aes(label = lab, hjust = ifelse(bias > 0, -0.15, 1.15)), size = 5, fontface = "bold", color = col_navy) +
  scale_fill_manual(values = c(saturable = col_red, linear = col_blue), name = "Binding in ICU plasma",
                    labels = c(linear = "linear (total predicts unbound)", saturable = "saturable (it does not)")) +
  scale_x_continuous(limits = c(-30, 105), breaks = seq(-25, 100, 25), labels = \(x) paste0(x, "%")) +
  labs(x = "Bias of unbound concentration predicted from\npublished protein binding (% of measured)", y = NULL,
       title = "Beta-lactams: published binding mispredicts free drug",
       subtitle = "ICU patients, measured vs predicted unbound (Wong 2013)") +
  base_theme(16) + theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_text(size = 12), legend.text = element_text(size = 12),
                         panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 14.5, color = col_navy, face = "bold"))

ph <- tibble(pred = factor(c("Serum albumin\nalone", "Total phenytoin\nalone", "Four correction\nequations (ICU)"),
                           c("Serum albumin\nalone", "Total phenytoin\nalone", "Four correction\nequations (ICU)")),
             r2 = c(0.09, 0.53, 0.4), kind = c("obs", "obs", "ceiling"),
             lab = c("R\u00b2 = 0.09", "R\u00b2 = 0.53", "all R\u00b2 < 0.4\nbiased, imprecise"))
p2 <- ggplot(ph, aes(pred, r2)) +
  geom_col(data = filter(ph, kind == "obs"), fill = col_grey, width = 0.6) +
  geom_col(data = filter(ph, kind == "ceiling"), fill = NA, color = col_red, linetype = "dashed", linewidth = 0.9, width = 0.6) +
  geom_segment(data = filter(ph, kind == "ceiling"), aes(x = 2.7, xend = 3.3, y = 0.4, yend = 0.4), color = col_red, linewidth = 1.2) +
  geom_text(aes(label = lab), vjust = -0.35, size = 4.6, lineheight = 0.9, fontface = "bold", color = col_navy) +
  scale_y_continuous(limits = c(0, 0.75), breaks = seq(0, 0.6, 0.2), expand = c(0, 0)) +
  labs(x = NULL, y = "R\u00b2 vs measured free phenytoin",
       title = "Phenytoin, the textbook case,\nfails the same test in the ICU",
       subtitle = "Cheng 2016; Parikh 2018") +
  base_theme(16) + theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 12.5, lineheight = 0.9))
p <- p1 + p2 + plot_layout(widths = c(1.3, 1))
ggsave("figures/albumin_correction.png", p, width = 13, height = 5.8, dpi = 300, bg = "white")
