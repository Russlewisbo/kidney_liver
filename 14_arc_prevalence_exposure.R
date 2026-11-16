# =============================================================================
# Slide 4. Augmented renal clearance: prevalence and consequence.
#   Hefny 2022 (systematic review, 70 studies): pooled ARC prevalence 39 %.
#   Udy 2012 (n = 52 paired troughs/CrCl): trough >= MIC in 58 % overall;
#     CrCl >= 130 mL/min/1.73 m2: trough < MIC in 82 %, < 4x MIC in 72 %.
# Published summary statistics only.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 17) theme_minimal(base_size = bs) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", color = col_navy, size = bs * 0.95), plot.subtitle = element_text(color = col_dark, size = bs * 0.75), plot.margin = margin(8, 10, 8, 8))

icons <- tibble(id = 1:100, x = (id - 1) %% 10, y = 9 - (id - 1) %/% 10,
                arc = ifelse(id <= 39, "Augmented renal clearance", "Not augmented"))
p1 <- ggplot(icons, aes(x, y, fill = arc)) +
  geom_tile(width = 0.82, height = 0.82) +
  scale_fill_manual(values = c("Augmented renal clearance" = col_blue, "Not augmented" = "#dfe3e8"), name = NULL) +
  coord_equal() + theme_void(base_size = 17) +
  labs(title = "39 of 100 ICU patients have ARC", subtitle = "Pooled prevalence, 70 studies (Hefny 2022)\nCrCl >= 130 mL/min/1.73 m\u00b2") +
  theme(legend.position = "bottom", legend.text = element_text(size = 13.5),
        plot.title = element_text(face = "bold", color = col_navy, size = 16),
        plot.subtitle = element_text(color = col_dark, size = 12.5), plot.margin = margin(8, 14, 8, 8))

exp <- tribble(~grp, ~metric, ~pct,
  "All patients",       "Trough < MIC",      42,
  "CrCl >= 130",        "Trough < MIC",      82,
  "CrCl >= 130",        "Trough < 4 x MIC",  72) |>
  mutate(grp = factor(grp, c("All patients", "CrCl >= 130")), metric = factor(metric, c("Trough < MIC", "Trough < 4 x MIC")))
p2 <- ggplot(exp, aes(grp, pct, fill = metric)) +
  geom_col(position = position_dodge2(width = 0.7, preserve = "single"), width = 0.62) +
  geom_text(aes(label = paste0(pct, "%")), position = position_dodge2(width = 0.7, preserve = "single"), vjust = -0.4, size = 5.2, fontface = "bold", color = col_navy) +
  scale_fill_manual(values = c("Trough < MIC" = col_blue, "Trough < 4 x MIC" = col_navy), name = NULL) +
  scale_x_discrete(labels = c("All patients\n(n = 52)", "Measured CrCl\n>= 130 mL/min/1.73 m\u00b2")) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), labels = \(x) paste0(x, "%")) +
  labs(x = NULL, y = "Beta-lactam troughs below target", title = "Consequence: sub-MIC troughs when clearance is high",
       subtitle = "Udy 2012, ICU patients on standard doses") +
  base_theme(17) + theme(legend.position = "bottom", panel.grid.major.x = element_blank(),
                         axis.text.x = element_text(size = 13.5, lineheight = 0.9))
p <- p1 + p2 + plot_layout(widths = c(1, 1.25))
ggsave("figures/arc_prevalence_exposure.png", p, width = 12.5, height = 5.6, dpi = 200, bg = "white")
