# =============================================================================
# 04_figures.R  -  manuscript figures (PNG 600 dpi + PDF) and the flow diagram
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(patchwork); library(survey); library(tidyr)
  library(forcats); library(scales); library(purrr)
})
options(survey.lonely.psu = "adjust")
res <- readRDS("output/res_main.rds"); ml <- readRDS("output/res_ml.rds")
flow <- readRDS("output/flow_counts.rds"); d <- readRDS("data/analytic.rds")

# validated categorical slots (reference palette, documented adjacent order)
col_blue <- "#2a78d6"; col_orange <- "#eb6834"; col_aqua <- "#1baf7a"; col_yellow <- "#eda100"
ink <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e6e5e0"

theme_ms <- function(base = 9) {
  theme_minimal(base_size = base, base_family = "Arial") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = grid_col, linewidth = 0.3),
          axis.text = element_text(color = ink2), axis.title = element_text(color = ink2),
          strip.text = element_text(face = "bold", hjust = 0, color = ink),
          plot.title = element_text(face = "bold", color = ink),
          plot.tag = element_text(face = "bold"),
          legend.position = "top", legend.justification = "left",
          legend.title = element_blank(), legend.text = element_text(color = ink2),
          plot.background = element_rect(fill = "white", color = NA))
}
save_fig <- function(p, name, w, h) {
  ggsave(file.path("figures", paste0(name, ".png")), p, width = w, height = h, dpi = 600, bg = "white",
         device = ragg::agg_png)
  ggsave(file.path("figures", paste0(name, ".tiff")), p, width = w, height = h, dpi = 600, bg = "white",
         device = ragg::agg_tiff, compression = "lzw")
}

# -----------------------------------------------------------------------------
# Figure 1. Where undiagnosed cases sit: criteria gap vs delivery gap
# -----------------------------------------------------------------------------
# Hypertension cascade computed here (USPSTF tier, then care access)
L <- filter(d, cycle == "2021-2023"); L$.w <- coalesce(L$wt_mec, 0)
dH <- svydesign(ids = ~psu, strata = ~strata, weights = ~.w, nest = TRUE, data = L)
htn_cascade <- function(v, lab) {
  s <- subset(dH, htn_sample & get(v) == 1 & !is.na(usual_src) & !is.na(insured))
  s <- update(s, stage = factor(case_when(
    htn_tier == "Screening every 3-5 y" ~ "Outside screening criteria",
    usual_src == 0 | insured == 0 ~ "Eligible, no usual source of care or uninsured",
    TRUE ~ "Eligible, in care, no recent blood test"),
    levels = c("Outside screening criteria", "Eligible, no usual source of care or uninsured",
               "Eligible, in care, no recent blood test", "Eligible, in care, blood test in past 3 y")))
  m <- svymean(~stage, s); tt <- svytotal(~stage, s)
  tibble(stage = levels(s$variables$stage), p = 100 * coef(m), N = coef(tt) / 1e6,
         outcome = lab, rule = "uspstf_htn")
}
htn_c <- bind_rows(htn_cascade("undx_htn130", "Undiagnosed hypertension, >=130/80 mm Hg"),
                   htn_cascade("undx_htn140", "Undiagnosed hypertension, >=140/90 mm Hg"))
saveRDS(htn_c, "output/htn_cascade.rds")

casc <- bind_rows(res$cascade, htn_c) |>
  mutate(rule_lab = case_when(rule == "uspstf_dm" ~ "USPSTF 2021",
                              rule == "ada_dm" ~ "ADA 2025",
                              TRUE ~ "USPSTF 2021"),
         row = paste0(outcome, "|", rule_lab),
         stage_lab = recode(stage,
           "Outside screening criteria" = "Outside screening criteria*",
           "Eligible, no usual source of care or uninsured" = "Eligible, uninsured or no usual source of care",
           "Eligible, in care, no recent blood test" = "Eligible and in care, no glucose test in past 3 y†",
           "Eligible, in care, blood test in past 3 y" = "Eligible and in care, glucose test in past 3 y"),
         stage_lab = factor(stage_lab, levels = c(
           "Outside screening criteria*", "Eligible, uninsured or no usual source of care",
           "Eligible and in care, no glucose test in past 3 y†",
           "Eligible and in care, glucose test in past 3 y")))
row_levels <- c("Undiagnosed diabetes|USPSTF 2021", "Undiagnosed diabetes|ADA 2025",
                "Undiagnosed prediabetes|USPSTF 2021", "Undiagnosed prediabetes|ADA 2025",
                "Undiagnosed hypertension, >=140/90 mm Hg|USPSTF 2021",
                "Undiagnosed hypertension, >=130/80 mm Hg|USPSTF 2021")
tot_N <- casc |> group_by(row) |> summarise(N = sum(N))
row_labels <- setNames(
  c("Diabetes vs USPSTF 2021", "Diabetes vs ADA 2025", "Prediabetes vs USPSTF 2021",
    "Prediabetes vs ADA 2025", "Hypertension ≥140/90 vs USPSTF 2021",
    "Hypertension ≥130/80 vs USPSTF 2021"), row_levels)
row_labels <- paste0(row_labels, "\n(", sprintf("%.1f", tot_N$N[match(row_levels, tot_N$row)]), " million)")
casc <- casc |> mutate(row = factor(row, levels = rev(row_levels), labels = rev(row_labels)),
                       group = case_when(grepl("diabetes vs", tolower(as.character(row))) &
                                           !grepl("prediabetes", tolower(as.character(row))) ~ "A  Undiagnosed diabetes",
                                         grepl("prediabetes", tolower(as.character(row))) ~ "B  Undiagnosed prediabetes",
                                         TRUE ~ "C  Undiagnosed hypertension"))
fill_vals <- setNames(c(col_orange, col_blue, col_aqua, col_yellow), levels(casc$stage_lab))
f1 <- ggplot(casc, aes(x = p, y = row, fill = stage_lab)) +
  geom_col(width = 0.62, color = "white", linewidth = 0.6, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(p >= 2.5, sprintf("%.0f%%", p), "")),
            position = position_stack(vjust = 0.5, reverse = TRUE), size = ifelse(casc$p < 8, 2.2, 2.7),
            color = ifelse(casc$stage_lab %in% levels(casc$stage_lab)[1:2], "white", ink)) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = fill_vals, drop = FALSE) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0), limits = c(0, 100.5)) +
  labs(x = "Share of undiagnosed US adults (survey weighted)", y = NULL) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_ms(9) + theme(strip.text.y = element_text(angle = 0, hjust = 0),
                      strip.placement = "outside", panel.spacing.y = unit(6, "pt"))
save_fig(f1, "Figure1_criteria_vs_delivery", 7.5, 4.6)

# -----------------------------------------------------------------------------
# Figure 2. Share of undiagnosed cases outside USPSTF criteria, by subgroup
# -----------------------------------------------------------------------------
keep_vars <- c("Age, y", "Sex", "Race and ethnicity", "BMI", "Income-to-poverty ratio", "Health insurance")
# Re-estimate BMI/income/insurance in pooled data for consistency
options(survey.lonely.psu = "adjust")
dd <- d; dd$.w <- coalesce(dd$wt_lab_pool, 0)
dP <- svydesign(ids = ~psu, strata = ~strata, weights = ~.w, nest = TRUE, data = dd)
cases_pool <- subset(dP, dm_sample & undx_dm == 1)
pool_extra <- tibble(v = c("bmi_cat", "pir", "insured"), value = c("BMI", "Income-to-poverty ratio", "Health insurance"))
pool_extra <- map_dfr(seq_len(nrow(pool_extra)), function(i) {
  v <- pool_extra$v[i]; lbl <- pool_extra$value[i]
  lv <- sort(unique(na.omit(cases_pool$variables[[v]][weights(cases_pool) > 0])))
  map_dfr(lv, function(l) {
    s <- update(subset(cases_pool, get(v) == l), out = 1 - uspstf_dm)
    e <- suppressWarnings(svyciprop(~out, s, method = "beta")); ci <- attr(e, "ci")
    tibble(var = lbl, level = if (v == "insured") ifelse(l == 1, "Yes", "No") else as.character(l),
           p = 100 * as.numeric(e), lo = 100 * ci[1], hi = 100 * ci[2], n_cases = sum(weights(s) > 0))
  })
})
sub_dm <- res$sub_dm_pool |> filter(rule == "USPSTF 2021") |>
  select(var, level, p, lo, hi, n_cases) |> bind_rows(pool_extra) |>
  mutate(panel = "A  Undiagnosed diabetes (2017-2023 pooled)")
sub_pre <- res$sub_predm |> filter(rule == "USPSTF 2021", var %in% keep_vars) |>
  select(var, level, p, lo, hi, n_cases) |> mutate(panel = "B  Undiagnosed prediabetes (2021-2023)")
sub <- bind_rows(sub_dm, sub_pre) |>
  mutate(lo = ifelse(is.na(lo), p, lo), hi = ifelse(is.na(hi), p, hi),
         level = recode(level, ">=3.50" = "≥3.50", ">=71" = "≥71", ">=30" = "≥30",
                        "Yes" = "Insured", "No" = "Uninsured"),
         var = factor(var, levels = keep_vars),
         lab = paste0(level, " (n=", n_cases, ")"))
saveRDS(sub, "output/fig2_data.rds")
lev_order <- c("20-34", "35-44", "45-64", "65-70", "≥71", "Male", "Female",
               "Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Non-Hispanic Asian",
               "Other or multiracial", "<25", "25-29.9", "≥30", "<1.30", "1.30-3.49", "≥3.50",
               "Insured", "Uninsured")
sub <- sub |> mutate(level = factor(level, levels = rev(lev_order)))
overall <- tibble(panel = c("A  Undiagnosed diabetes (2017-2023 pooled)", "B  Undiagnosed prediabetes (2021-2023)"),
                  p = c(filter(res$sens, analysis == "Pooled 2017-2023", rule == "USPSTF 2021")$p,
                        filter(res$outside, outcome == "Undiagnosed prediabetes", rule == "USPSTF 2021")$p))
f2 <- ggplot(sub, aes(x = p, y = level)) +
  geom_vline(data = overall, aes(xintercept = p), linetype = "dashed", color = ink2, linewidth = 0.35) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, orientation = "y", color = col_blue, linewidth = 0.6) +
  geom_point(aes(size = n_cases), color = col_blue, fill = col_blue, shape = 21, stroke = 0.8) +
  geom_text(aes(x = 104, label = n_cases), hjust = 0, size = 2.4, color = ink2) +
  facet_grid(var ~ panel, scales = "free_y", space = "free_y", switch = "y") +
  scale_size_area(max_size = 3.2, guide = "none") +
  scale_x_continuous(limits = c(0, 112), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(x = "Undiagnosed cases outside USPSTF 2021 screening criteria (95% CI)", y = NULL,
       caption = "Dashed line, overall share. Numbers at right, unweighted cases in the subgroup.") +
  theme_ms(8.5) + theme(strip.placement = "outside",
                        strip.text.y.left = element_text(angle = 0, hjust = 1, face = "plain", color = ink2),
                        panel.grid.major.y = element_line(color = "#f1f0ec", linewidth = 0.25),
                        panel.spacing.y = unit(3, "pt"), panel.spacing.x = unit(14, "pt"),
                        plot.caption = element_text(color = ink2, hjust = 0, size = 7))
save_fig(f2, "Figure2_subgroups", 7.5, 6.4)

# -----------------------------------------------------------------------------
# Figure 3. Risk model with conformal screening rules (temporal validation)
# -----------------------------------------------------------------------------
cur <- ml$undx_dm$curve |> arrange(volume)
ev <- ml$eval |> filter(outcome == "undx_dm")
pts <- ev |> filter(rule %in% c("USPSTF 2021", "ADA 2025", "Conformal rule, target sensitivity 80%",
                                "Conformal rule, target sensitivity 90%", "Conformal rule, target sensitivity 95%")) |>
  mutate(kind = ifelse(grepl("Conformal", rule), "Conformal rule (risk model)", "Guideline rule"),
         lab = recode(rule, "Conformal rule, target sensitivity 80%" = "80% target",
                      "Conformal rule, target sensitivity 90%" = "90% target",
                      "Conformal rule, target sensitivity 95%" = "95% target"),
         lx = case_when(rule == "ADA 2025" ~ volume, rule == "USPSTF 2021" ~ volume + 1.8,
                        grepl("80%", rule) ~ volume - 1.5, TRUE ~ volume + 2.5),
         ly = case_when(rule == "ADA 2025" ~ 93, rule == "USPSTF 2021" ~ sens - 3,
                        grepl("80%", rule) ~ sens + 4, grepl("95%", rule) ~ sens - 3.5, TRUE ~ sens - 2.5),
         hj = case_when(rule == "ADA 2025" ~ 0.5, grepl("80%", rule) ~ 1, TRUE ~ 0))
f3a <- ggplot() +
  geom_step(data = cur, aes(volume, sens), color = col_blue, linewidth = 0.7, direction = "vh") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = ink2, linewidth = 0.3) +
  geom_errorbar(data = pts, aes(x = volume, ymin = sens_lo, ymax = sens_hi), width = 0, color = ink2, linewidth = 0.4) +
  geom_point(data = pts, aes(volume, sens, shape = kind, fill = kind), size = 2.8, color = "white", stroke = 0.6) +
  geom_text(data = pts, aes(lx, ly, label = lab, hjust = hj), size = 2.5, color = ink) +
  scale_shape_manual(values = c("Conformal rule (risk model)" = 21, "Guideline rule" = 22)) +
  scale_fill_manual(values = c("Conformal rule (risk model)" = col_blue, "Guideline rule" = col_orange)) +
  scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%"), expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 101), labels = function(x) paste0(x, "%"), expand = c(0.01, 0)) +
  labs(x = "Adults without diagnosed diabetes flagged for testing", y = "Undiagnosed diabetes detected (sensitivity)",
       tag = "A") +
  theme_ms(8.5) + theme(panel.grid.major.y = element_line(color = grid_col, linewidth = 0.3),
                        legend.position = c(0.98, 0.04), legend.justification = c(1, 0),
                        legend.background = element_rect(fill = "white", color = NA))
cal <- ml$undx_dm$calib
f3b <- ggplot(cal, aes(pred * 100, obs * 100)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = ink2, linewidth = 0.3) +
  geom_line(color = col_blue, linewidth = 0.6) +
  geom_point(color = "white", fill = col_blue, shape = 21, size = 2.4, stroke = 0.6) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) + scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_equal(xlim = c(0, 20), ylim = c(0, 20)) +
  labs(x = "Predicted risk (decile mean)", y = "Observed prevalence", tag = "B") +
  theme_ms(8.5) + theme(panel.grid.major.y = element_line(color = grid_col, linewidth = 0.3))
f3 <- f3a + f3b + plot_layout(widths = c(1.7, 1))
save_fig(f3, "Figure3_model_conformal", 7.5, 3.6)

# -----------------------------------------------------------------------------
# eFigure 1. Participant flow diagram
# -----------------------------------------------------------------------------
fl <- flow |> tidyr::pivot_longer(-cycle) |> tidyr::pivot_wider(names_from = cycle, values_from = value)
g <- function(n, cyc = "2021-2023") format(fl[[cyc]][fl$name == n], big.mark = ",")
box <- function(x, y, w, h, txt, fill = "white", bold = FALSE) {
  list(annotate("rect", xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2,
                fill = fill, color = ink2, linewidth = 0.35),
       annotate("text", x = x, y = y, label = txt, size = 2.6, lineheight = 0.95,
                fontface = ifelse(bold, "bold", "plain"), color = ink))
}
arr <- function(x1, y1, x2, y2) annotate("segment", x = x1, y = y1, xend = x2, yend = y2, color = ink2,
                                         linewidth = 0.35, arrow = arrow(length = unit(1.4, "mm"), type = "closed"))
ex <- function(xs, y, x2, txt) list(annotate("segment", x = xs, y = y, xend = x2 - 1.6, yend = y, color = ink2,
                                              linewidth = 0.35, arrow = arrow(length = unit(1.2, "mm"), type = "closed")),
                                     box(x2 + 1.5, y, 6.2, 0.9, txt, fill = "#f6f6f4"))
nexam <- as.numeric(fl$`2021-2023`[fl$name=="n_adults"]) - as.numeric(fl$`2021-2023`[fl$name=="n_examined"])
npreg <- as.numeric(fl$`2021-2023`[fl$name=="n_examined"]) - as.numeric(fl$`2021-2023`[fl$name=="n_notpreg"])
side <- function(y, txt) list(annotate("segment", x = 7.5, y = y, xend = 13.4, yend = y, color = ink2, linewidth = 0.35,
                                       arrow = arrow(length = unit(1.2, "mm"), type = "closed")),
                              box(16.5, y, 6.2, 0.85, txt, fill = "#f6f6f4"))
pf <- ggplot() + coord_cartesian(xlim = c(-0.3, 24.3), ylim = c(-0.3, 12.3), expand = FALSE) + theme_void() +
  box(7.5, 11.6, 9, 0.9, paste0("NHANES August 2021-August 2023\n", g("n_total"), " participants"), bold = TRUE) +
  arr(7.5, 11.15, 7.5, 10.45) +
  box(7.5, 10, 9, 0.9, paste0("Adults aged \u226520 y\n", g("n_adults"))) +
  side(9.2, paste0("Excluded: not examined at MEC\n", format(nexam, big.mark = ","))) +
  arr(7.5, 9.55, 7.5, 8.85) +
  box(7.5, 8.4, 9, 0.9, paste0("Examined and not pregnant\n", g("n_notpreg"))) +
  side(7.6, paste0("Excluded: pregnant at exam\n", npreg)) +
  annotate("segment", x = 7.5, y = 7.95, xend = 7.5, yend = 7.15, color = ink2, linewidth = 0.35) +
  annotate("segment", x = 3.5, y = 7.15, xend = 11.5, yend = 7.15, color = ink2, linewidth = 0.35) +
  arr(3.5, 7.15, 3.5, 6.85) + arr(11.5, 7.15, 11.5, 6.85) +
  box(3.5, 6.3, 7.4, 1.1, paste0("Diabetes analytic sample\n(HbA1c, diabetes history, BMI)\n", g("n_dm_final")), bold = TRUE) +
  box(11.5, 6.3, 7.4, 1.1, paste0("Hypertension analytic sample\n(\u22651 BP reading, history, BMI)\n", g("n_htn_final")), bold = TRUE) +
  arr(3.5, 5.75, 3.5, 5.05) + arr(11.5, 5.75, 11.5, 5.05) +
  box(3.5, 4.6, 7.4, 0.9, paste0("Diagnosed diabetes, ", g("n_dm_dx"), "\nNo diagnosed diabetes, ", g("n_dm_nodx"))) +
  box(11.5, 4.6, 7.4, 0.9, paste0("Diagnosed or treated, ", g("n_htn_dx"), "\nNo diagnosis or treatment, ", g("n_htn_nodx"))) +
  annotate("segment", x = 3.5, y = 4.15, xend = 3.5, yend = 3.75, color = ink2, linewidth = 0.35) +
  annotate("segment", x = 1.6, y = 3.75, xend = 5.4, yend = 3.75, color = ink2, linewidth = 0.35) +
  arr(1.6, 3.75, 1.6, 3.4) + arr(5.4, 3.75, 5.4, 3.4) +
  annotate("segment", x = 11.5, y = 4.15, xend = 11.5, yend = 3.75, color = ink2, linewidth = 0.35) +
  annotate("segment", x = 9.6, y = 3.75, xend = 13.4, yend = 3.75, color = ink2, linewidth = 0.35) +
  arr(9.6, 3.75, 9.6, 3.4) + arr(13.4, 3.75, 13.4, 3.4) +
  box(1.6, 2.85, 3.4, 1.0, paste0("Undiagnosed\ndiabetes\n", g("n_undx_dm")), fill = "#fdf0ea") +
  box(5.4, 2.85, 3.4, 1.0, paste0("Undiagnosed\nprediabetes\n", g("n_undx_predm")), fill = "#fdf0ea") +
  box(9.6, 2.85, 3.4, 1.0, paste0("Undiagnosed\n\u2265130/80 mm Hg\n", g("n_undx_htn130")), fill = "#fdf0ea") +
  box(13.4, 2.85, 3.4, 1.0, paste0("Undiagnosed\n\u2265140/90 mm Hg\n", g("n_undx_htn140")), fill = "#fdf0ea") +
  box(20.9, 5.6, 6.4, 2.6, paste0("Model development cohort\nNHANES 2017-March 2020\n\nAdults without diagnosed diabetes\n",
      g("n_dm_nodx", "2017-2020"), " (", g("n_undx_dm", "2017-2020"),
      " undiagnosed diabetes)\n\n70% training / 30% conformal calibration"), fill = "#eef4fc") +
  arr(20.9, 4.3, 20.9, 1.85) +
  box(20.9, 1.3, 6.4, 1.1, paste0("Temporal validation\nNHANES 2021-2023, ", g("n_dm_nodx"), " adults\nwithout diagnosed diabetes (",
      g("n_undx_dm"), " cases)"), fill = "#eef4fc") +
  annotate("segment", x = 3.5, y = 4.15, xend = 3.5, yend = 4.15, color = ink2) +
  annotate("segment", x = 7.2, y = 4.6, xend = 7.9, yend = 4.6, color = NA) +
  annotate("segment", x = 7.2, y = 1.3, xend = 17.6, yend = 1.3, color = ink2, linewidth = 0.35, linetype = "dashed",
           arrow = arrow(length = unit(1.2, "mm"), type = "closed")) +
  annotate("segment", x = 7.2, y = 4.6, xend = 7.2, yend = 1.3, color = ink2, linewidth = 0.35, linetype = "dashed") +
  annotate("text", x = 0, y = 0.2, hjust = 0, size = 2.3, color = ink2,
           label = "BP indicates blood pressure; HbA1c, hemoglobin A1c; MEC, mobile examination center.")
save_fig(pf, "eFigure1_flow", 7.5, 4.4)

cat("Figures written.\n")
