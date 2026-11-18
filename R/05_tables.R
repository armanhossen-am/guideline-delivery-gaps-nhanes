# =============================================================================
# 05_tables.R  -  formatted tables for the manuscript and supplement
# Output: output/tables.rds (list of data frames, character cells)
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(survey); library(purrr); library(tidyr) })
options(survey.lonely.psu = "adjust")
d <- readRDS("data/analytic.rds"); res <- readRDS("output/res_main.rds"); ml <- readRDS("output/res_ml.rds")
L <- filter(d, cycle == "2021-2023")
f1 <- function(x) formatC(x, format = "f", digits = 1)
pci <- function(p, lo, hi) paste0(f1(p), " (", f1(lo), "-", f1(hi), ")")

# -----------------------------------------------------------------------------
# Table 1. Characteristics (2021-2023)
# -----------------------------------------------------------------------------
L$.wl <- coalesce(L$wt_lab, 0); L$.wm <- coalesce(L$wt_mec, 0)
dl <- svydesign(ids = ~psu, strata = ~strata, weights = ~.wl, nest = TRUE, data = L)
dm <- svydesign(ids = ~psu, strata = ~strata, weights = ~.wm, nest = TRUE, data = L)
groups <- list(
  "All adults without diagnosed diabetes" = subset(dl, dm_sample & dm_dx == 0),
  "Undiagnosed diabetes" = subset(dl, dm_sample & undx_dm == 1),
  "Undiagnosed prediabetes" = subset(dl, dm_sample & undx_predm == 1),
  "Undiagnosed hypertension (≥130/80 mm Hg)" = subset(dm, htn_sample & undx_htn130 == 1))
L <- L |> mutate(sex = factor(ifelse(female == 1, "Female", "Male"), levels = c("Male", "Female")),
                 nativity = factor(ifelse(foreign_born == 1, "Born outside the US", "US born")),
                 ins = factor(ifelse(insured == 1, "Yes", "No"), levels = c("Yes", "No")),
                 usual = factor(ifelse(usual_src == 1, "Yes", "No"), levels = c("Yes", "No")),
                 test3 = factor(ifelse(blood_test3y == 1, "Yes", "No"), levels = c("Yes", "No")),
                 elig_usp = factor(ifelse(uspstf_dm == 1, "Yes", "No"), levels = c("Yes", "No")),
                 elig_ada = factor(ifelse(ada_dm == 1, "Yes", "No"), levels = c("Yes", "No")))
groups <- map(groups, ~ { .x$variables <- L[match(.x$variables$SEQN, L$SEQN), ]; .x })
catvars <- list("Age, y" = "age_cat", "Sex" = "sex", "Race and ethnicity" = "race",
                "Education" = "educ", "Family income-to-poverty ratio" = "pir",
                "Nativity" = "nativity", "BMI" = "bmi_cat", "Health insurance" = "ins",
                "Usual source of care" = "usual",
                "Reported glucose test in past 3 y" = "test3",
                "Eligible under USPSTF 2021 diabetes criteria" = "elig_usp",
                "Eligible under ADA 2025 diabetes criteria" = "elig_ada")
t1 <- tibble(Characteristic = "Unweighted No.", !!!map(groups, ~ format(sum(weights(.x) > 0), big.mark = ",")))
t1 <- bind_rows(t1, tibble(Characteristic = "Weighted No., millions",
                           !!!map(groups, ~ f1(sum(weights(.x)) / 1e6))))
t1 <- bind_rows(t1, tibble(Characteristic = "Age, mean (SE), y",
                           !!!map(groups, ~ { m <- svymean(~age, .x); paste0(f1(coef(m)), " (", f1(SE(m)), ")") })))
for (nm in names(catvars)) {
  v <- catvars[[nm]]
  if (v %in% c("ins", "usual", "test3", "elig_usp", "elig_ada")) {
    row <- map(groups, ~ { s <- subset(.x, !is.na(get(v)))
      paste0(f1(100 * coef(svymean(as.formula(paste0("~I(", v, "=='Yes')")), s))[2])) })
    t1 <- bind_rows(t1, tibble(Characteristic = paste0(nm, ", %"), !!!row))
    next
  }
  t1 <- bind_rows(t1, tibble(Characteristic = paste0(nm, ", %")))
  lv <- levels(L[[v]])
  for (l in lv) {
    row <- map(groups, ~ { s <- subset(.x, !is.na(get(v)))
      f1(100 * coef(svymean(as.formula(paste0("~I(", v, "=='", l, "')")), s))[2]) })
    lab <- recode(l, ">=3.50" = "≥3.50", ">=71" = "≥71", ">=30" = "≥30")
    t1 <- bind_rows(t1, tibble(Characteristic = paste0("   ", lab), !!!row))
  }
}
t1 <- bind_rows(t1,
  tibble(Characteristic = "HbA1c, mean (SE), %", !!!map(groups[1:3], ~ { m <- svymean(~a1c, .x); paste0(f1(coef(m)), " (", formatC(SE(m), format = "f", digits = 2), ")") }),
         `Undiagnosed hypertension (≥130/80 mm Hg)` = "NA"),
  tibble(Characteristic = "Systolic BP, mean (SE), mm Hg", !!!map(groups, ~ { s <- subset(.x, !is.na(sbp)); m <- svymean(~sbp, s); paste0(f1(coef(m)), " (", f1(SE(m)), ")") })))
t1 <- t1 |> mutate(across(everything(), ~ replace_na(.x, "")))
# drop eligibility rows for hypertension column (not applicable)
t1[grepl("Eligible under", t1$Characteristic), 5] <- "NA"
t1[grepl("glucose test", t1$Characteristic), 5] <- t1[grepl("glucose test", t1$Characteristic), 5]

# -----------------------------------------------------------------------------
# Table 2. Screening strategies, 2021-2023 (temporal validation)
# -----------------------------------------------------------------------------
ev <- ml$eval |> filter(rule != "Conformal rule, target sensitivity 85%")
t2 <- ev |> mutate(
  Outcome = ifelse(outcome == "undx_dm", "Undiagnosed diabetes", "Undiagnosed diabetes or prediabetes"),
  Strategy = rule,
  `Adults flagged, % (95% CI)` = pci(volume, volume_lo, volume_hi),
  `Adults flagged, millions` = f1(N_screen),
  `Sensitivity, % (95% CI)` = ifelse(is.na(sens_lo), paste0(f1(sens), " (97.6-100)†"), pci(sens, sens_lo, sens_hi)),
  `Positive yield, %` = f1(ppv),
  `No. tested per case found` = formatC(nns, format = "f", digits = 1),
  `Cases outside USPSTF captured, % (95% CI)` = case_when(
    rule == "USPSTF 2021" ~ "0 (by definition)",
    is.na(capture_lo) ~ "100 (by definition)",
    TRUE ~ pci(capture_missed, capture_lo, capture_hi))) |>
  select(Outcome, Strategy:`Cases outside USPSTF captured, % (95% CI)`)

# -----------------------------------------------------------------------------
# Supplement tables
# -----------------------------------------------------------------------------
# eTable 1: operational definitions (static text in Rmd)
# eTable 2: prevalence, counts, share outside criteria and reasons
e2a <- res$prev |> transmute(Condition = outcome, `Unweighted cases` = NA_character_,
                             `Prevalence, % (95% CI)` = pci(p, lo, hi),
                             `US adults, millions (95% CI)` = pci(N, N_lo, N_hi))
fc <- readRDS("output/flow_counts.rds") |> filter(cycle == "2021-2023")
e2a$`Unweighted cases` <- format(c(fc$n_undx_dm, fc$n_undx_predm, fc$n_undx_htn130, fc$n_undx_htn140), big.mark = ",")
e2a$Condition <- c("Undiagnosed diabetes (HbA1c ≥6.5%)", "Undiagnosed prediabetes (HbA1c 5.7%-6.4%)",
                   "Undiagnosed hypertension (≥130/80 mm Hg)", "Undiagnosed hypertension (≥140/90 mm Hg)")
e2b <- res$outside |> transmute(Condition = outcome, Rule = rule,
  `Outside criteria, % (95% CI)` = ifelse(is.na(lo) & p == 0,
     paste0("0 (0-", f1(100 * (1 - 0.025^(1 / n_cases))), ")‡"), pci(p, lo, hi)),
  `US adults outside criteria, millions (95% CI)` = ifelse(N == 0, "0", pci(N, N_lo, N_hi)))
e2c <- res$reasons |> transmute(Condition = outcome, `USPSTF 2021 status` = reason,
  `Unweighted cases` = as.character(n), `% (95% CI)` = pci(p, lo, hi), `Millions` = f1(N))

# eTable 3: sensitivity analyses
e3 <- res$sens |> transmute(Analysis = analysis, Rule = rule, `Unweighted cases` = as.character(n_cases),
  `Outside criteria, % (95% CI)` = ifelse(is.na(lo) | p < 0.5,
      ifelse(p == 0, paste0("0 (0-", f1(100 * (1 - 0.025^(1 / n_cases))), ")"), paste0(f1(p), " (", f1(lo), "-", f1(hi), ")")),
      pci(p, lo, hi)))
e3 <- bind_rows(e3, res$outside |> filter(rule == "USPSTF 2021 (BMI>=25 for all)") |>
  transmute(Analysis = ifelse(outcome == "Undiagnosed diabetes", "BMI ≥25 for all groups (no Asian-specific threshold)",
                              "BMI ≥25 for all groups, prediabetes"),
            Rule = "USPSTF 2021", `Unweighted cases` = as.character(n_cases), `Outside criteria, % (95% CI)` = pci(p, lo, hi)))

# eTable 4: model performance
e4 <- map_dfr(c("undx_dm", "undx_dysgly"), function(y) {
  a <- ml[[y]]$auc
  a |> transmute(Outcome = ifelse(y == "undx_dm", "Undiagnosed diabetes", "Undiagnosed diabetes or prediabetes"),
                 Model = ifelse(model == "glm", "Logistic regression with splines", "Gradient boosting (XGBoost)"),
                 `Selected` = ifelse(model == ml[[y]]$best_model, "Yes", "No"),
                 `AUC, calibration split` = formatC(ml[[y]]$auc_ca[model], format = "f", digits = 3),
                 `AUC, 2021-2023 (95% CI)` = paste0(formatC(auc, format = "f", digits = 3), " (",
                                                    formatC(auc_lo, format = "f", digits = 3), "-",
                                                    formatC(auc_hi, format = "f", digits = 3), ")"),
                 `Calibration slope` = formatC(cal_slope, format = "f", digits = 2),
                 `Calibration-in-the-large` = formatC(cal_int, format = "f", digits = 3),
                 `Observed / expected, %` = paste0(f1(100 * obs), " / ", f1(100 * exp)),
                 `Brier score` = formatC(brier, format = "f", digits = 3))
})
e4dev <- map_dfr(c("undx_dm", "undx_dysgly"), function(y) tibble(
  Outcome = ifelse(y == "undx_dm", "Undiagnosed diabetes", "Undiagnosed diabetes or prediabetes"),
  `Training, n (cases)` = paste0(ml[[y]]$n_tr, " (", ml[[y]]$pos_tr, ")"),
  `Calibration, n (cases)` = paste0(ml[[y]]$n_ca, " (", ml[[y]]$pos_ca, ")"),
  `Validation, n (cases)` = paste0(ml[[y]]$n_test, " (", ml[[y]]$pos_test, ")")))

# eTable 5: conformal thresholds and sensitivity by subgroup
e5 <- ml$sub_sens |> mutate(var = recode(var, age_cat = "Age, y", race = "Race and ethnicity", female = "Sex"),
                            level = ifelse(var == "Sex", ifelse(level == "1", "Female", "Male"), level),
                            level = recode(level, ">=71" = "≥71")) |>
  transmute(Subgroup = var, Level = level, `Unweighted cases` = as.character(n),
            `Detected by USPSTF 2021, %` = f1(uspstf), `Detected by conformal rule (90% target), %` = f1(conf10))
e5thr <- ml$undx_dm$conf |> transmute(`Target sensitivity` = paste0(100 * (1 - alpha), "%"),
  `Flag if predicted risk ≥, %` = formatC(100 * thr_case, format = "f", digits = 2),
  `Rule out if predicted risk ≤, %` = formatC(100 * thr_noncase, format = "f", digits = 2))
sets <- ml$sets |> transmute(`Conformal prediction set (90% level)` = set10, `Unweighted adults` = as.character(n),
                             `Weighted share, %` = f1(pct), `Undiagnosed diabetes cases` = as.character(cases))

# eTable 6: undiagnosed hypertension by subgroup
e6 <- res$htn_sub |> select(var, level, def, p, lo, hi, n) |>
  mutate(est = pci(p, lo, hi), level = recode(level, ">=3.50" = "≥3.50", ">=71" = "≥71", ">=30" = "≥30")) |>
  select(-p, -lo, -hi) |> pivot_wider(names_from = def, values_from = est) |>
  transmute(Subgroup = var, Level = level, `Adults without diagnosis, n` = format(n, big.mark = ","),
            `≥130/80 mm Hg, % (95% CI)` = `>=130/80`, `≥140/90 mm Hg, % (95% CI)` = `>=140/90`)
e6b <- res$htn |> transmute(Condition = outcome, Measure = m, `% (95% CI)` = pci(p, lo, hi),
                            `Millions` = f1(N))

saveRDS(list(t1 = t1, t2 = t2, e2a = e2a, e2b = e2b, e2c = e2c, e3 = e3, e4 = e4, e4dev = e4dev,
             e5 = e5, e5thr = e5thr, sets = sets, e6 = e6, e6b = e6b), "output/tables.rds")
print(t1, n = 60); print(t2, width = 250); print(e3); print(e4)
