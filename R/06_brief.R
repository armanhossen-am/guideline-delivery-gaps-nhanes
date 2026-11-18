# =============================================================================
# 06_brief.R - one-page policy brief (Typst -> PDF), numbers pulled from outputs
# Output: brief/Policy_Brief_Undiagnosed_Diabetes_Hypertension.pdf
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })
res <- readRDS("output/res_main.rds"); ml <- readRDS("output/res_ml.rds")
htc <- readRDS("output/htn_cascade.rds"); tb <- readRDS("output/tables.rds")
dir.create("brief", showWarnings = FALSE)
r0 <- function(x) formatC(x, format = "f", digits = 0); r1 <- function(x) formatC(x, format = "f", digits = 1)

pdm  <- res$prev[1, ]; ppre <- res$prev[2, ]; ph <- res$prev[3, ]
o_dm <- filter(res$outside, outcome == "Undiagnosed diabetes", rule == "USPSTF 2021")
o_pr <- filter(res$outside, outcome == "Undiagnosed prediabetes", rule == "USPSTF 2021")
old  <- filter(res$reasons, outcome == "Undiagnosed diabetes", reason == "Older than 70 y")
t1   <- as.data.frame(tb$t1)
test_dm <- as.numeric(t1[t1$Characteristic == "Reported glucose test in past 3 y, %", 3])
h_in <- filter(res$htn, outcome == "Undiagnosed hypertension (>=130/80)", grepl("Insured with", m))
ev <- ml$eval |> filter(outcome == "undx_dm")
U <- filter(ev, rule == "USPSTF 2021"); A <- filter(ev, rule == "ADA 2025")
M <- filter(ev, rule == "Risk model, volume matched to USPSTF")
elig_care <- filter(res$elig_care, m == "Usual source of care")

# ---- compact figure ---------------------------------------------------------
three <- function(df, out, rl) {
  x <- df |> filter(outcome == out, rule == rl)
  tibble(row = out,
         seg = c("Outside screening criteria", "Eligible, uninsured or no usual care", "Eligible and in care"),
         p = c(x$p[1], x$p[2], x$p[3] + x$p[4]))
}
fd <- bind_rows(
  three(res$cascade, "Undiagnosed diabetes", "uspstf_dm") |> mutate(row = paste0("Diabetes\n", r1(pdm$N), " million")),
  three(res$cascade, "Undiagnosed prediabetes", "uspstf_dm") |> mutate(row = paste0("Prediabetes\n", r1(ppre$N), " million")),
  three(htc, "Undiagnosed hypertension, >=130/80 mm Hg", "uspstf_htn") |> mutate(row = paste0("Hypertension\n", r1(ph$N), " million"))) |>
  mutate(row = factor(row, levels = rev(unique(row))),
         seg = factor(seg, levels = c("Outside screening criteria", "Eligible, uninsured or no usual care", "Eligible and in care")))
cols <- c("#eb6834", "#2a78d6", "#1baf7a")   # validated reference slots 2, 1, 3
fig <- ggplot(fd, aes(p, row, fill = seg)) +
  geom_col(width = 0.66, color = "white", linewidth = 0.7, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(p >= 6, paste0(r0(p), "%"), "")), position = position_stack(vjust = 0.5, reverse = TRUE),
            size = 3.1, color = ifelse(fd$seg == "Eligible and in care", "#0b0b0b", "white"), family = "Roboto") +
  scale_fill_manual(values = setNames(cols, levels(fd$seg))) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100.5), labels = function(x) paste0(x, "%")) +
  guides(fill = guide_legend(nrow = 3)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 9.5, base_family = "Roboto") +
  theme(legend.position = "top", legend.justification = "left", legend.title = element_blank(),
        legend.text = element_text(color = "#52514e", size = 8.5), legend.key.size = unit(9, "pt"),
        panel.grid = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_text(color = "#0b0b0b", hjust = 0, lineheight = 0.95),
        plot.margin = margin(0, 4, 0, 0), plot.background = element_rect(fill = "white", color = NA))
ggsave("brief/brief_fig.png", fig, width = 3.7, height = 2.55, dpi = 400, device = ragg::agg_png, bg = "white")

# ---- Typst source -----------------------------------------------------------
typ <- sprintf('
#set page(paper: "us-letter", margin: (x: 0.55in, top: 0.45in, bottom: 0.45in))
#set text(font: "Roboto", size: 9.4pt, fill: rgb("#1d1d1b"))
#set par(leading: 0.52em, justify: false)
#let navy = rgb("#1f3556")
#let accent = rgb("#eb6834")
#let muted = rgb("#52514e")
#let soft = rgb("#f3f5f8")

#block(width: 100%%, fill: navy, inset: (x: 14pt, y: 11pt), radius: 3pt)[
  #text(fill: rgb("#c9d6ea"), size: 8.2pt, weight: "medium", tracking: 0.6pt)[POLICY BRIEF #h(6pt) | #h(6pt) NHANES AUGUST 2021-AUGUST 2023]
  #v(2pt)
  #text(fill: white, size: 17pt, weight: "bold")[Who do screening guidelines miss?]
  #v(-4pt)
  #text(fill: white, size: 11pt)[Most undiagnosed diabetes and hypertension is in adults who already qualify for screening and see a clinician]
]

#v(5pt)
#grid(columns: (1fr, 1fr, 1fr, 1fr), gutter: 7pt,
  ..(
    ([%s million], [US adults with undiagnosed diabetes; %s million more with undiagnosed hypertension]),
    ([%s%%], [of undiagnosed diabetes falls outside USPSTF criteria, mostly adults over 70]),
    ([%s%%], [of adults with undiagnosed diabetes reported a glucose test in the past 3 years]),
    ([%s%% vs %s%%], [of cases found by a risk-based rule vs USPSTF criteria, testing about the same number of adults]),
  ).map(((big, small)) => block(width: 100%%, fill: soft, inset: 8pt, radius: 3pt, height: 62pt)[
    #text(size: 16pt, weight: "bold", fill: navy)[#big] \\
    #v(-2pt)
    #text(size: 8.2pt, fill: muted)[#small]
  ])
)

#v(4pt)
#grid(columns: (1.08fr, 1fr), gutter: 14pt,
[
  #text(size: 11pt, weight: "bold", fill: navy)[The problem]
  #v(-3pt)
  Screening rules decide who gets tested. The US Preventive Services Task Force (USPSTF) recommends diabetes screening at ages 35 to 70 years with overweight or obesity; the American Diabetes Association (ADA) recommends testing everyone from 35 years, and younger adults with risk factors. Hypertension screening is recommended for all adults. We asked how much undiagnosed disease falls outside these rules, and how much is missed even when people qualify.

  #v(2pt)
  #text(size: 11pt, weight: "bold", fill: navy)[What we found]
  #v(-3pt)
  - *The USPSTF gap is mostly an age gap.* About %s million adults with undiagnosed diabetes (%s%%) are outside USPSTF criteria; adults over 70 make up %s million of them. For prediabetes the gap is %s%% (%s million), including younger and leaner adults.
  - *Wider criteria are not efficient.* ADA criteria missed no undiagnosed diabetes in the sample but apply to %s%% of adults without diabetes, so %s people are tested for each case found (USPSTF, %s).
  - *The larger gap is follow-through.* %s%% of eligible adults with undiagnosed diabetes have a usual source of care. Of %s million adults with undiagnosed hypertension, %s%% are insured and have a usual source of care.
  - *Risk scores can do better at the same cost.* A model using information from any routine visit flagged %s%% of adults (USPSTF, %s%%) and found %s%% of undiagnosed diabetes (USPSTF, %s%%).
],
[
  #text(size: 11pt, weight: "bold", fill: navy)[Where undiagnosed adults are]
  #v(-2pt)
  #image("brief_fig.png", width: 100%%)
  #v(-4pt)
  #text(size: 7.4pt, fill: muted)[Share of US adults with each undiagnosed condition, against USPSTF criteria. For hypertension, "outside" means on the less frequent (every 3-5 years) schedule.]
  #v(3pt)
  #table(columns: (1.5fr, 1fr, 1fr, 0.9fr), stroke: none, inset: (x: 3pt, y: 3.2pt),
    align: (left, center, center, center),
    table.hline(stroke: 0.8pt + navy),
    table.header([*Diabetes screening rule*], [*Adults tested*], [*Cases found*], [*Tests per case*]),
    table.hline(stroke: 0.5pt + muted),
    [USPSTF 2021], [%s%%], [%s%%], [%s],
    [ADA 2025], [%s%%], [%s%%], [%s],
    [Risk-based rule], [%s%%], [%s%%], [%s],
    table.hline(stroke: 0.8pt + navy),
  )
]
)

#v(2pt)
#block(width: 100%%, fill: soft, inset: (x: 11pt, y: 8pt), radius: 3pt, stroke: (left: 3pt + accent))[
  #text(size: 11pt, weight: "bold", fill: navy)[What this means for policy]
  #v(-2pt)
  #grid(columns: (1fr, 1fr), gutter: 12pt,
  [
    + *Measure and pay for follow-through, not only eligibility.* Quality programs and payers can track whether an abnormal HbA#sub[1c] or blood pressure reading leads to a confirmatory test and a documented diagnosis.
    + *Build the next step into the record.* Reflex confirmatory testing after an abnormal HbA#sub[1c] and electronic health record alerts for repeated high blood pressure readings without a diagnosis target the largest gap directly.
  ],
  [
    #set enum(start: 3)
    + *Act on results already in hand for adults over 70.* Rather than routine screening, use HbA#sub[1c] values drawn for other reasons, with treatment goals matched to health and life expectancy.
    + *Pilot risk-based eligibility, with safeguards.* Risk tools should be recalibrated on local data and audited for real-world sensitivity across age and racial and ethnic groups before wider use.
  ])
]

#v(3pt)
#text(size: 7.2pt, fill: muted)[*About the study.* Survey-weighted analysis of %s adults aged 20 years or older in the National Health and Nutrition Examination Survey (NHANES), August 2021-August 2023. Undiagnosed diabetes: HbA#sub[1c] 6.5%% or higher with no prior diagnosis; prediabetes: HbA#sub[1c] 5.7%%-6.4%%; hypertension: average blood pressure 130/80 mm Hg or higher at one visit with no diagnosis or treatment. Single measurements overstate disease compared with confirmed diagnoses. The risk model was built in NHANES 2017-March 2020 and tested in 2021-2023. Full methods: [manuscript citation]. Contact: [name, email].]
',
  r1(pdm$N), r0(ph$N), r0(o_dm$p), r0(test_dm), r0(M$sens), r0(U$sens),
  r1(o_dm$N), r0(o_dm$p), r1(old$N), r0(o_pr$p), r1(o_pr$N),
  r0(A$volume), r0(A$nns), r0(U$nns),
  r0(elig_care$p), r1(ph$N), r0(h_in$p),
  r0(M$volume), r0(U$volume), r0(M$sens), r0(U$sens),
  r0(U$volume), r0(U$sens), r0(U$nns), r0(A$volume), "100", r0(A$nns), r0(M$volume), r0(M$sens), r0(M$nns),
  format(sum(readRDS("output/flow_counts.rds")$n_dm_final[2]), big.mark = ","))
writeLines(typ, "brief/brief.typ")
system2("quarto", c("typst", "compile", "brief/brief.typ", "brief/Policy_Brief_Undiagnosed_Diabetes_Hypertension.pdf"))
