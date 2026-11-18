# =============================================================================
# Slide 14 (take-home). One timeline, 0-96 h: dose for the infection first,
# then for measured organ function. Author's synthesis - not data.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 17) theme_minimal(base_size = bs) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", color = col_navy, size = bs * 0.95), plot.subtitle = element_text(color = col_dark, size = bs * 0.75), plot.margin = margin(8, 10, 8, 8))

bands <- tibble(xmin = c(0, 48), xmax = c(48, 96), lab = c("0-48 h: dose for the INFECTION", "After 48 h: dose for MEASURED organ function"),
                fill = c(col_blue, col_navy))
events <- tribble(~x, ~y, ~lab, ~hj,
  0,  2.55, "Full loading dose\n(set by volume, not by kidney)", 0,
  2,  1.75, "Unreduced maintenance dose of a wide-index beta-lactam\nprolonged infusion after the load", 0,
  48, 2.55, "Re-measure, do not estimate:\nurinary CrCl or cystatin C;\nurine output + RRT dose and hours", 0,
  48, 1.75, "Then adjust the maintenance dose\nup (ARC) or down (persistent AKI)", 0,
  72, 2.55, "Re-check whenever organ function\nor RRT settings change", 0)
p <- ggplot() +
  geom_rect(data = bands, aes(xmin = xmin, xmax = xmax, ymin = 3.0, ymax = 3.5, fill = fill)) +
  geom_text(data = bands, aes(x = (xmin + xmax) / 2, y = 3.25, label = lab), color = "white", fontface = "bold", size = 5.2) +
  scale_fill_identity() +
  annotate("segment", x = 0, xend = 96, y = 1.2, yend = 1.2, linewidth = 1.2, color = col_dark, arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  annotate("point", x = c(0, 48, 72), y = 1.2, size = 5, color = c(col_blue, col_navy, col_navy)) +
  annotate("segment", x = c(0, 48, 72), xend = c(0, 48, 72), y = 1.2, yend = 2.95, linetype = "dotted", color = col_dark) +
  geom_text(data = events, aes(x + 0.8, y, label = lab, hjust = hj), size = 4.3, lineheight = 0.92, color = col_navy, vjust = 1) +
  annotate("rect", xmin = -1, xmax = 97, ymin = 0.12, ymax = 0.9, fill = "#fbeeee", color = col_red, linewidth = 0.5) +
  annotate("text", x = 48, y = 0.5, size = 4.4, color = col_red, fontface = "bold",
           lineheight = 0.9, label = "Exception: narrow-index or highly bound drugs (vancomycin, aminoglycosides, polymyxins; flucloxacillin, ceftriaxone)\nare individualised from the FIRST dose - measure, do not assume") +
  scale_x_continuous(breaks = c(0, 24, 48, 72, 96), labels = \(x) paste0(x, " h"), limits = c(-1, 97), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.1, 3.55), expand = c(0, 0)) +
  labs(x = "Time from first antibiotic dose", y = NULL) +
  theme_minimal(base_size = 17) +
  theme(panel.grid = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size = 14, color = col_dark, face = "bold"),
        axis.title.x = element_text(size = 14, color = col_dark), plot.margin = margin(8, 10, 8, 8))
ggsave("figures/dosing_timeline.png", p, width = 13, height = 5.6, dpi = 300, bg = "white")
