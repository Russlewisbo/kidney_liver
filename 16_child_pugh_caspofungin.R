# =============================================================================
# Slide 7 (liver). Child-Pugh class and caspofungin exposure in ICU patients
# (Kurland 2019, AAC; n = 46).  Child-Pugh A 5, B 40, C 1.
#   Correlation with AUC: Child-Pugh score r = 0.03 (P = 0.84); albumin r = 0.45 (P = 0.005)
# Published summary statistics only.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 17) theme_minimal(base_size = bs) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", color = col_navy, size = bs * 0.95), plot.subtitle = element_text(color = col_dark, size = bs * 0.75), plot.margin = margin(8, 10, 8, 8))

cp <- tibble(cls = factor(c("A", "B", "C"), c("A", "B", "C")), n = c(5, 40, 1),
             lab = c("5", "40\n(label says: reduce dose)", "1"))
p1 <- ggplot(cp, aes(cls, n)) +
  geom_col(fill = c(col_grey, col_blue, col_grey), width = 0.62) +
  geom_text(aes(label = lab), vjust = -0.3, size = 4.8, lineheight = 0.9, color = col_navy, fontface = "bold") +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  labs(x = "Child-Pugh class", y = "ICU patients (n = 46)", title = "Most ICU patients score Child-Pugh B",
       subtitle = "Through low albumin, not cirrhosis") +
  base_theme(17) + theme(panel.grid.major.x = element_blank())

r <- tibble(var = factor(c("Child-Pugh score", "Serum albumin"), c("Child-Pugh score", "Serum albumin")),
            r = c(0.03, 0.45), lab = c("r = 0.03\nP = 0.84", "r = 0.45\nP = 0.005"))
p2 <- ggplot(r, aes(var, r)) +
  geom_hline(yintercept = 0, color = col_dark) +
  geom_col(fill = c(col_grey, col_blue), width = 0.55) +
  geom_text(aes(label = lab), vjust = -0.3, size = 4.8, lineheight = 0.9, color = col_navy, fontface = "bold") +
  scale_y_continuous(limits = c(-0.05, 0.62), breaks = seq(0, 0.6, 0.2), expand = c(0, 0)) +
  labs(x = NULL, y = "Correlation with caspofungin AUC", title = "Score does not predict exposure; albumin does",
       subtitle = "Low albumin went with LOW exposure") +
  base_theme(17) + theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 15))
p <- p1 + p2
ggsave("figures/child_pugh_caspofungin.png", p, width = 12.5, height = 5.4, dpi = 200, bg = "white")
