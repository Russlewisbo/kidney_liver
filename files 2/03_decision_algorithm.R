# =============================================================================
# Decision algorithm for antimicrobial dosing in organ failure
# Three anchoring questions: free fraction -> Vd -> CL -> dosing -> TDM
# Rendered via DiagrammeR (Graphviz dot syntax)
# =============================================================================

suppressPackageStartupMessages({
  library(DiagrammeR)
  library(DiagrammeRsvg)
  library(rsvg)
})

dot <- '
digraph dosing {
  graph [layout = dot, rankdir = TB, splines = ortho, nodesep = 0.45, ranksep = 0.55,
         bgcolor = "white"]
  node  [fontname = "Helvetica", fontsize = 13, style = "filled,rounded",
         shape = box, color = "#2c3e50", penwidth = 1.2]
  edge  [fontname = "Helvetica", fontsize = 11, color = "#2c3e50", penwidth = 1.1]

  start [label = "Patient with organ failure\\nrequiring antimicrobial therapy",
         fillcolor = "#1f3a5f", fontcolor = "white", fontsize = 14]

  q1 [label = "Q1. What is the FREE fraction?\\nIs this drug highly protein-bound\\n(>80%) AND is albumin <25 g/L?",
      fillcolor = "#fff4d6"]

  q1y [label = "Free fraction ↑\\nTotal-drug TDM misleads.\\nMeasure FREE drug if available.",
       fillcolor = "#ffe4b3"]
  q1n [label = "Standard total-drug\\ntargets apply",
       fillcolor = "#e8f4ff"]

  q2 [label = "Q2. Is Vd altered?\\nSepsis · capillary leak · ascites\\n· edema · CRRT circuit",
      fillcolor = "#fff4d6"]

  q2y [label = "Give a FULL loading dose\\n(based on expanded Vd) —\\nnever reduced for organ failure",
       fillcolor = "#d6f0e0"]
  q2n [label = "Standard loading dose",
       fillcolor = "#e8f4ff"]

  q3 [label = "Q3. What is the CLEARANCE?",
      fillcolor = "#fff4d6"]

  cl_renal   [label = "Renally cleared\\n· Measure CrCl (urinary)\\n· Watch for ARC (>130 mL/min)\\n· If CRRT: estimate via\\n  Q_eff × S_c",
              fillcolor = "#e0ecff"]
  cl_hepatic [label = "Hepatically cleared\\n· Child-Pugh ≠ clearance\\n· Assess HRS / cholestasis\\n· Expect wide BSV",
              fillcolor = "#f0e0ff"]
  cl_mixed   [label = "Mixed / unclear\\n· Use population priors\\n· TDM-driven empiric",
              fillcolor = "#fde0e0"]

  pk_pd [label = "Q4. Match maintenance to PK/PD target",
         fillcolor = "#fff4d6"]

  bl [label = "β-lactams (fT>MIC)\\n→ extended or continuous\\n   infusion",
      fillcolor = "#d6f0e0"]
  vanco [label = "Vancomycin · FQ (AUC/MIC)\\n→ Bayesian AUC dosing",
         fillcolor = "#d6f0e0"]
  amg [label = "Aminoglycosides (Cmax/MIC)\\n→ once-daily, extended intervals",
       fillcolor = "#d6f0e0"]

  tdm [label = "Q5. Trigger TDM / MIPD\\nDay 1–2 · re-sample after every\\norgan-function change",
       fillcolor = "#1f3a5f", fontcolor = "white", fontsize = 14]

  start -> q1
  q1 -> q1y [label = "Yes"]
  q1 -> q1n [label = "No"]
  q1y -> q2
  q1n -> q2

  q2 -> q2y [label = "Yes"]
  q2 -> q2n [label = "No"]
  q2y -> q3
  q2n -> q3

  q3 -> cl_renal
  q3 -> cl_hepatic
  q3 -> cl_mixed

  cl_renal -> pk_pd
  cl_hepatic -> pk_pd
  cl_mixed -> pk_pd

  pk_pd -> bl
  pk_pd -> vanco
  pk_pd -> amg

  bl -> tdm
  vanco -> tdm
  amg -> tdm
}
'

g <- grViz(dot)
svg <- export_svg(g)
rsvg_png(charToRaw(svg), file = "figures/decision_algorithm.png", width = 2200)
message("Saved figures/decision_algorithm.png")
