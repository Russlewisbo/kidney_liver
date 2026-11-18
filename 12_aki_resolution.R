# =============================================================================
# Slide 1. Admission AKI in infected patients resolves quickly (Crass 2019, CID).
# Published percentages only (abstract): AKI on admission in 27.1 % (pneumonia),
# 19.5 % (intra-abdominal), 20.0 % (urinary), 9.7 % (skin/soft tissue);
# of admission AKI, 57.2 % had resolved by 48 h. Icon array = 100 patients.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 17) theme_minimal(base_size = bs) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", color = col_navy, size = bs * 0.95), plot.subtitle = element_text(color = col_dark, size = bs * 0.75), plot.margin = margin(8, 10, 8, 8))

# Left: icon array, 100 patients with AKI at the time infection is diagnosed
n_res <- 57   # 57.2 % rounded to whole patients
icons <- tibble(id = 1:100, x = (id - 1) %% 10, y = 9 - (id - 1) %/% 10,
                status = ifelse(id <= n_res, "Resolved by 48 h", "Persistent at 48 h"))
p1 <- ggplot(icons, aes(x, y, fill = status)) +
  geom_tile(width = 0.82, height = 0.82, color = NA) +
  scale_fill_manual(values = c("Resolved by 48 h" = col_blue, "Persistent at 48 h" = col_grey), name = NULL) +
  coord_equal() + theme_void(base_size = 17) +
  labs(title = "100 patients with AKI at diagnosis",
       subtitle = "57.2% have resolved by 48 h") +
  theme(legend.position = "bottom", legend.text = element_text(size = 14),
        plot.title = element_text(face = "bold", color = col_navy, size = 16, hjust = 0.5),
        plot.subtitle = element_text(color = col_blue, face = "bold", size = 15, hjust = 0.5),
        plot.margin = margin(8, 14, 8, 8))

# Right: how common is AKI at presentation, by infection type
aki <- tribble(~site, ~pct,
  "Pneumonia", 27.1, "Urinary tract", 20.0, "Intra-abdominal", 19.5, "Skin and soft tissue", 9.7) |>
  mutate(site = factor(site, rev(site)))
p2 <- ggplot(aki, aes(pct, site)) +
  geom_col(fill = col_blue, width = 0.62) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.15, size = 5.2, color = col_navy, fontface = "bold") +
  scale_x_continuous(limits = c(0, 34), expand = c(0, 0), labels = \(x) paste0(x, "%")) +
  labs(x = "Patients with AKI on admission", y = NULL,
       title = "AKI is present at diagnosis in 1 of 4 pneumonias") +
  base_theme(17) + theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 15, color = col_dark))

p <- p2 + p1 + plot_layout(widths = c(1.15, 1))
ggsave("figures/aki_resolution.png", p, width = 12.5, height = 5.6, dpi = 300, bg = "white")
