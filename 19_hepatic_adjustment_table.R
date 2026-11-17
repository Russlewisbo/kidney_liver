# =============================================================================
# New liver slide. Which antimicrobials carry a hepatic dose adjustment?
# Table graphic. Sources: Halilovic & Heintz 2014 (AJHP; 22 antibiotics with
# hepatic or mixed clearance reviewed), Verbeeck 2008 (EJCP), Comce 2024 (JAC
# systematic review: ceftriaxone, gentamicin, meropenem, vancomycin in cirrhosis),
# Kurland 2019 (caspofungin ICU), and the US prescribing information of each drug.
# CHECK each label cell against the current SmPC/PI before presenting.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(patchwork)})
col_blue <- "#1f77b4"; col_grey <- "#9aa3ad"; col_navy <- "#1f3a5f"; col_dark <- "#4a4a4a"; col_red <- "#b03a3a"
base_theme <- function(bs = 17) theme_minimal(base_size = bs) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", color = col_navy, size = bs * 0.95), plot.subtitle = element_text(color = col_dark, size = bs * 0.75), plot.margin = margin(8, 10, 8, 8))

library(tidyr)
tab <- tribble(~drug, ~handling, ~label, ~note,
  "Tigecycline",     "Biliary / hepatic",          "Child-Pugh C: 100 mg load, then 25 mg q12h",        "Class C only; hepatotoxicity",
  "Metronidazole",   "Hepatic oxidation",           "Child-Pugh C: reduce dose by 50%",                  "A and B: no change",
  "Voriconazole",    "CYP2C19 / 3A4",               "Child-Pugh A-B: full load, half maintenance",       "No data for class C; TDM",
  "Caspofungin",     "Hepatic (hydrolysis)", "Child-Pugh B: 35 mg/day",                        "ICU: score does not predict AUC (next slide)",
  "Rifampicin",      "Hepatic, biliary",            "No quantitative adjustment",                        "Monitor for hepatotoxicity",
  "Ceftriaxone",     "40% biliary, 60% renal",      "None unless hepatic + renal failure (max 2 g/day)", "Albumin, not Child-Pugh, drives free fraction",
  "Other beta-lactams,\naminoglycosides, vancomycin", "Renal", "None for liver disease", "Kidney function, volume, albumin set the dose") |>
  mutate(row = rev(seq_len(n())), hep = ifelse(label %in% c("None for liver disease", "No quantitative adjustment"), "none", "adj"))
long <- tab |> select(row, hep, drug, handling, label, note) |>
  pivot_longer(c(drug, handling, label, note), names_to = "col", values_to = "txt") |>
  mutate(x = c(drug = 0, handling = 3.0, label = 5.1, note = 9.7)[col],
         face = ifelse(col == "drug", "bold", "plain"),
         colr = case_when(col == "label" & hep == "adj" ~ col_blue, col == "drug" ~ col_navy, TRUE ~ col_dark))
hdrs <- tibble(x = c(0, 3.0, 5.1, 9.7), txt = c("Drug", "Elimination", "Label adjustment for liver disease", "Comment"))
p <- ggplot() +
  geom_rect(data = tab, aes(xmin = -0.1, xmax = 13.8, ymin = row - 0.45, ymax = row + 0.45, fill = hep), alpha = 0.5) +
  scale_fill_manual(values = c(adj = "#eef4fb", none = "#f3f3f3"), guide = "none") +
  geom_text(data = long, aes(x, row, label = txt, fontface = face), color = long$colr, hjust = 0, size = 4.1, lineheight = 0.85) +
  geom_text(data = hdrs, aes(x, max(tab$row) + 1.05, label = txt), hjust = 0, size = 4.6, fontface = "bold", color = col_navy) +
  annotate("segment", x = -0.1, xend = 13.8, y = max(tab$row) + 0.6, yend = max(tab$row) + 0.6, color = col_navy, linewidth = 0.6) +
  scale_x_continuous(limits = c(-0.1, 13.8), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.4, max(tab$row) + 1.5), expand = c(0, 0)) +
  theme_void() + theme(plot.margin = margin(6, 8, 6, 8))
ggsave("figures/hepatic_adjustment_table.png", p, width = 14, height = 5.2, dpi = 300, bg = "white")
