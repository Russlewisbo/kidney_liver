# =============================================================================
# Assumed vs measured unbound fraction (fu) of highly bound beta-lactams in
# ICU patients. Every number is a published summary statistic (no simulation).
#   Ceftriaxone   reference: published binding 83-96 % -> fu 4-17 % (values tabulated in Wong 2013, AAC)
#                 ICU: median 33.0 %, IQR 20.2-44.5 %        (Schleibinger 2015, BJCP)
#   Flucloxacillin reference: fu 5 % assumed in routine TDM   (Wallenburg 2021; Moser 2021)
#                 ICU: range 7.0-71.7 %                       (Wallenburg 2021, JAC)
#                 S. aureus bacteraemia: range 1.1-64.7 %     (Moser 2021, JAC)
#   Ertapenem     healthy-volunteer plasma, same assay: ~20 % (Liebchen 2014, JAC)
#                 ICU: range 30.9-53.6 %                      (Liebchen 2014, JAC)
# NOTE: the bars mix IQR and range because that is what each paper reports;
# the statistic is printed on each bar.
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(ggplot2)})

col_meas <- "#1f77b4"; col_ref <- "#4a4a4a"

meas <- tribble(
  ~drug,            ~cohort,                                   ~lo,  ~hi,  ~mid, ~stat,
  "Ceftriaxone",    "ICU (Schleibinger 2015)",                 20.2, 44.5, 33.0, "median, IQR",
  "Flucloxacillin", "ICU (Wallenburg 2021)",                    7.0, 71.7, NA,   "range",
  "Flucloxacillin", "S. aureus bacteraemia (Moser 2021)",       1.1, 64.7, NA,   "range",
  "Ertapenem",      "ICU (Liebchen 2014)",                     30.9, 53.6, NA,   "range"
) |>
  mutate(row = c(4, 2.8, 2.2, 1),
         lab = sprintf("%s: %s", cohort, stat))

ref <- tribble(
  ~drug,            ~lo, ~hi,  ~row, ~lab,
  "Ceftriaxone",     4,  17,   4.45, "literature value: 4-17%",
  "Flucloxacillin",  5,   5,   3.25, "assumed in TDM: 5%",
  "Ertapenem",      20,  20,   1.45, "healthy volunteers, same assay: ~20%"
)

drug_lab <- tibble(drug = c("Ceftriaxone", "Flucloxacillin", "Ertapenem"),
                   row  = c(4.2, 2.75, 1.2))

p <- ggplot() +
  geom_segment(data = meas, aes(x = lo, xend = hi, y = row, yend = row),
               linewidth = 7, color = col_meas, lineend = "butt", alpha = 0.85) +
  geom_point(data = filter(meas, !is.na(mid)), aes(x = mid, y = row),
             shape = 21, fill = "white", color = col_meas, size = 4.5, stroke = 1.6) +
  geom_text(data = meas, aes(x = hi + 1.5, y = row, label = lab),
            hjust = 0, size = 4.6, color = col_meas) +
  geom_segment(data = ref, aes(x = lo, xend = hi, y = row, yend = row),
               linewidth = 3, color = col_ref) +
  geom_point(data = filter(ref, lo == hi), aes(x = lo, y = row),
             shape = 18, size = 6, color = col_ref) +
  geom_text(data = ref, aes(x = hi + 1.5, y = row, label = lab),
            hjust = 0, size = 4.6, color = col_ref) +
  geom_text(data = drug_lab, aes(x = -2, y = row, label = drug),
            hjust = 1, fontface = "bold", size = 5.6, color = "#1f3a5f") +
  scale_x_continuous(breaks = seq(0, 80, 20), labels = \(x) paste0(x, "%"),
                     limits = c(-26, 125), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.6, 4.8)) +
  labs(x = "Unbound fraction of drug in plasma", y = NULL) +
  theme_minimal(base_size = 17) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(hjust = 0.34, face = "bold"),
        plot.margin = margin(8, 8, 8, 8))

ggsave("figures/unbound_fraction_range.png", p, width = 12.5, height = 5.6, dpi = 200, bg = "white")
