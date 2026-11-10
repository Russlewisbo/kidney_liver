# =============================================================================
# Free vs protein-bound drug: only unbound drug crosses the capillary wall
# Schematic with randomly placed (simulated) molecules. Illustrative only.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

set.seed(2026)

# Plasma compartment: x in [0, 4.6]; tissue (site of infection): x in [5.4, 10]
n_alb  <- 9      # albumin molecules, each with one bound drug molecule
n_free <- 7      # free drug in plasma
n_tis  <- 6      # free drug that has reached tissue

alb <- tibble(x = runif(n_alb, 0.5, 4.1), y = runif(n_alb, 0.8, 4.2)) |>
  mutate(bx = x + 0.28, by = y + 0.22)

free_plasma <- tibble(x = runif(n_free, 0.4, 4.2), y = runif(n_free, 0.6, 4.4))
free_tissue <- tibble(x = runif(n_tis, 5.8, 8.2), y = runif(n_tis, 0.6, 4.4))

bact <- tibble(x = 9.0, y = 2.5)

col_drug <- "#1f77b4"
col_alb  <- "#b7bec7"

p <- ggplot() +
  annotate("rect", xmin = 0, xmax = 4.6, ymin = 0, ymax = 5,
           fill = "#fdf1f1", color = "grey70") +
  annotate("rect", xmin = 5.4, xmax = 10, ymin = 0, ymax = 5,
           fill = "#f1f6fb", color = "grey70") +
  annotate("segment", x = 5, xend = 5, y = 0, yend = 5,
           linetype = "dashed", color = "grey40", linewidth = 1) +
  annotate("text", x = 2.3, y = 5.45, label = "Plasma", fontface = "bold",
           size = 6, color = "#a83232") +
  annotate("text", x = 7.7, y = 5.45, label = "Site of infection (tissue)",
           fontface = "bold", size = 6, color = "#1f77b4") +
  annotate("text", x = 5, y = -0.45, label = "capillary wall", size = 4.6,
           color = "grey35") +
  geom_point(data = alb, aes(x, y), shape = 21, size = 15, fill = col_alb,
             color = "grey45", stroke = 0.8) +
  geom_point(data = alb, aes(bx, by), shape = 21, size = 4.5, fill = col_drug,
             color = "white", stroke = 0.6) +
  geom_point(data = free_plasma, aes(x, y), shape = 21, size = 4.5,
             fill = col_drug, color = "white", stroke = 0.6) +
  geom_point(data = free_tissue, aes(x, y), shape = 21, size = 4.5,
             fill = col_drug, color = "white", stroke = 0.6) +
  # Free drug crossing the wall
  annotate("segment", x = 4.65, xend = 5.35, y = 2.5, yend = 2.5,
           arrow = arrow(length = unit(0.14, "in")), color = col_drug,
           linewidth = 1.2) +
  annotate("label", x = 5, y = 3.1, label = "free only", size = 4.6,
           color = col_drug, fontface = "bold", fill = "white", linewidth = 0) +
  # Bacterium (target) in tissue
  geom_point(data = bact, aes(x, y), shape = 21, size = 26, fill = "#e8d9b0",
             color = "#8a6d1f", stroke = 1) +
  annotate("text", x = 9.0, y = 2.5, label = "target\norganism", size = 3.8,
           color = "#5a4710", lineheight = 0.9) +
  # Legend
  annotate("point", x = 0.5, y = -1.2, shape = 21, size = 4.5, fill = col_drug,
           color = "white") +
  annotate("text", x = 0.8, y = -1.2, label = "Free (unbound) drug", hjust = 0, size = 4.6) +
  annotate("point", x = 3.6, y = -1.2, shape = 21, size = 8, fill = col_alb,
           color = "grey45") +
  annotate("text", x = 4.0, y = -1.2, label = "Albumin (with bound drug)", hjust = 0, size = 4.6) +
  coord_cartesian(xlim = c(-0.1, 10.1), ylim = c(-1.7, 5.9), expand = FALSE) +
  theme_void(base_family = "sans")

ggsave("figures/free_drug.png", p, width = 11.2, height = 5.6, dpi = 200, bg = "white")
message("Saved figures/free_drug.png")
