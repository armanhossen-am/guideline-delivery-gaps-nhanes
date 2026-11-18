# =============================================================================
# 02_analysis.R
# Survey-weighted estimates: prevalence, share outside screening criteria,
# national counts, subgroup patterns, care-contact ("delivery") gaps, and
# sensitivity analyses.
# Output: output/res_*.rds / .csv
# =============================================================================
suppressPackageStartupMessages({ library(survey); library(dplyr); library(tidyr); library(purrr) })
options(survey.lonely.psu = "adjust")

d <- readRDS("data/analytic.rds")

mk_des <- function(data, w) {
  data$.w <- coalesce(data[[w]], 0)
  svydesign(ids = ~psu, strata = ~strata, weights = ~.w, nest = TRUE, data = data)
}
L  <- filter(d, cycle == "2021-2023")
P  <- filter(d, cycle == "2017-2020")
dL_lab <- mk_des(L, "wt_lab")
dL_mec <- mk_des(L, "wt_mec")
dL_fst <- mk_des(L, "wt_fast")
dP_lab <- mk_des(P, "wt_lab")
dPool  <- mk_des(d, "wt_lab_pool")
dPoolM <- mk_des(d, "wt_mec_pool")

# Korn-Graubard proportion with RSE flag (NCHS presentation standards, simplified)
kg <- function(formula, des) {
  est <- svyciprop(formula, des, method = "beta", level = 0.95)
  ci  <- attr(est, "ci"); se <- SE(est)
  tibble(p = as.numeric(est) * 100, lo = ci[1] * 100, hi = ci[2] * 100,
         n = sum(weights(des) > 0),
         rse = as.numeric(se / est) * 100)
}
tot <- function(formula, des) {
  t <- svytotal(formula, des, na.rm = TRUE); ci <- confint(t)
  tibble(N = as.numeric(coef(t))[1] / 1e6, N_lo = ci[1, 1] / 1e6, N_hi = ci[1, 2] / 1e6)
}
n_unw <- function(des) sum(weights(des) > 0)

res <- list()

# -----------------------------------------------------------------------------
# 1. Prevalence and national counts (2021-2023)
# -----------------------------------------------------------------------------
s_dm  <- subset(dL_lab, dm_sample)
s_htn <- subset(dL_mec, htn_sample)
res$prev <- bind_rows(
  kg(~undx_dm, s_dm)     |> bind_cols(tot(~undx_dm, s_dm))     |> mutate(outcome = "Undiagnosed diabetes"),
  kg(~undx_predm, s_dm)  |> bind_cols(tot(~undx_predm, s_dm))  |> mutate(outcome = "Undiagnosed prediabetes"),
  kg(~undx_htn130, s_htn)|> bind_cols(tot(~undx_htn130, s_htn))|> mutate(outcome = "Undiagnosed hypertension (>=130/80)"),
  kg(~undx_htn140, s_htn)|> bind_cols(tot(~undx_htn140, s_htn))|> mutate(outcome = "Undiagnosed hypertension (>=140/90)")
)
# share of all diabetes that is undiagnosed
s_anydm <- subset(dL_lab, dm_sample & (dm_dx == 1 | undx_dm == 1))
res$share_undx <- kg(~undx_dm, s_anydm)

# -----------------------------------------------------------------------------
# 2. Share outside screening criteria, among undiagnosed cases
# -----------------------------------------------------------------------------
cases <- list(
  dm    = subset(dL_lab, dm_sample & undx_dm == 1),
  predm = subset(dL_lab, dm_sample & undx_predm == 1)
)
outside <- function(des, lab) {
  des <- update(des, out_usp = 1 - uspstf_dm, out_ada = 1 - ada_dm,
                out_usp_strict = 1 - uspstf_dm_strict,
                out_both = as.integer(uspstf_dm == 0 & ada_dm == 0))
  bind_rows(
    kg(~out_usp, des) |> bind_cols(tot(~out_usp, des)) |> mutate(rule = "USPSTF 2021"),
    kg(~out_usp_strict, des) |> bind_cols(tot(~out_usp_strict, des)) |> mutate(rule = "USPSTF 2021 (BMI>=25 for all)"),
    kg(~out_ada, des) |> bind_cols(tot(~out_ada, des)) |> mutate(rule = "ADA 2025"),
    kg(~out_both, des) |> bind_cols(tot(~out_both, des)) |> mutate(rule = "Outside both")
  ) |> mutate(outcome = lab, n_cases = n_unw(des))
}
res$outside <- bind_rows(outside(cases$dm, "Undiagnosed diabetes"),
                         outside(cases$predm, "Undiagnosed prediabetes"))

# reasons for USPSTF ineligibility
reason_tab <- function(des, lab) {
  m <- svymean(~uspstf_reason, des)
  ci <- confint(m)
  tt <- svytotal(~uspstf_reason, des)
  tibble(reason = levels(des$variables$uspstf_reason),
         p = 100 * coef(m), lo = 100 * pmax(ci[, 1], 0), hi = 100 * ci[, 2],
         N = coef(tt) / 1e6,
         n = as.numeric(table(des$variables$uspstf_reason[weights(des) > 0])),
         outcome = lab)
}
res$reasons <- bind_rows(reason_tab(cases$dm, "Undiagnosed diabetes"),
                         reason_tab(cases$predm, "Undiagnosed prediabetes"))

# -----------------------------------------------------------------------------
# 3. Subgroups: % outside USPSTF and ADA, and counts missed
# -----------------------------------------------------------------------------
subvars <- c(age_cat = "Age, y", female = "Sex", race = "Race and ethnicity",
             bmi_cat = "BMI", pir = "Income-to-poverty ratio", educ = "Education",
             foreign_born = "Nativity", insured = "Health insurance",
             usual_src = "Usual source of care")
lab_level <- function(v, x) {
  if (v == "female") return(ifelse(x == 1, "Female", "Male"))
  if (v == "foreign_born") return(ifelse(x == 1, "Born outside US", "US born"))
  if (v %in% c("insured", "usual_src")) return(ifelse(x == 1, "Yes", "No"))
  as.character(x)
}
sub_est <- function(des, lab) {
  map_dfr(names(subvars), function(v) {
    lv <- sort(unique(na.omit(des$variables[[v]][weights(des) > 0])))
    map_dfr(lv, function(l) {
      s <- subset(des, get(v) == l)
      s <- update(s, out_usp = 1 - uspstf_dm, out_ada = 1 - ada_dm)
      n <- n_unw(s)
      bind_rows(
        kg(~out_usp, s) |> bind_cols(tot(~out_usp, s)) |> mutate(rule = "USPSTF 2021"),
        kg(~out_ada, s) |> bind_cols(tot(~out_ada, s)) |> mutate(rule = "ADA 2025")
      ) |> mutate(var = subvars[[v]], level = lab_level(v, l), n_cases = n)
    })
  }) |> mutate(outcome = lab)
}
res$sub_dm    <- sub_est(cases$dm, "Undiagnosed diabetes")
res$sub_predm <- sub_est(cases$predm, "Undiagnosed prediabetes")

# Composition: distribution of the USPSTF-missed undiagnosed diabetes population
miss_dm <- subset(cases$dm, uspstf_dm == 0)
res$missed_comp <- map_dfr(c("race", "age_cat", "bmi_cat", "female", "insured"), function(v) {
  m <- svymean(as.formula(paste0("~factor(", v, ")")), miss_dm, na.rm = TRUE)
  tibble(var = v, level = sub(".*\\)", "", names(coef(m))), p = 100 * coef(m))
})

# -----------------------------------------------------------------------------
# 4. Criteria gap vs delivery gap (undiagnosed diabetes, prediabetes)
# -----------------------------------------------------------------------------
cascade <- function(des, elig, lab) {
  v <- des$variables
  des <- update(des,
    stage = factor(case_when(
      get(elig) == 0 ~ "Outside screening criteria",
      usual_src == 0 | insured == 0 ~ "Eligible, no usual source of care or uninsured",
      blood_test3y == 1 ~ "Eligible, in care, blood test in past 3 y",
      TRUE ~ "Eligible, in care, no recent blood test"),
      levels = c("Outside screening criteria",
                 "Eligible, no usual source of care or uninsured",
                 "Eligible, in care, no recent blood test",
                 "Eligible, in care, blood test in past 3 y")))
  m <- svymean(~stage, des, na.rm = TRUE); ci <- confint(m)
  tt <- svytotal(~stage, des, na.rm = TRUE)
  tibble(stage = levels(des$variables$stage), p = 100 * coef(m),
         lo = 100 * pmax(ci[, 1], 0), hi = 100 * ci[, 2], N = coef(tt) / 1e6,
         outcome = lab, rule = elig)
}
res$cascade <- bind_rows(
  cascade(cases$dm, "uspstf_dm", "Undiagnosed diabetes"),
  cascade(cases$dm, "ada_dm", "Undiagnosed diabetes"),
  cascade(cases$predm, "uspstf_dm", "Undiagnosed prediabetes"),
  cascade(cases$predm, "ada_dm", "Undiagnosed prediabetes"))

# Among eligible undiagnosed diabetes: care contact
elig_dm <- subset(cases$dm, uspstf_dm == 1)
res$elig_care <- bind_rows(
  kg(~usual_src, subset(elig_dm, !is.na(usual_src))) |> mutate(m = "Usual source of care"),
  kg(~insured, subset(elig_dm, !is.na(insured))) |> mutate(m = "Insured"),
  kg(~blood_test3y, subset(elig_dm, !is.na(blood_test3y))) |> mutate(m = "Blood test in past 3 y"))

# -----------------------------------------------------------------------------
# 5. Screening volume and yield of each rule (all adults without diagnosed DM)
# -----------------------------------------------------------------------------
nodx <- subset(dL_lab, dm_sample & dm_dx == 0)
rule_perf <- function(des, rule, lab) {
  des <- update(des, r = get(rule))
  vol  <- kg(~r, des)                              # % of adults flagged
  e_dm <- kg(~r, subset(des, undx_dm == 1))     # sensitivity, diabetes
  e_dg <- kg(~r, subset(des, undx_dysgly == 1)) # sensitivity, dysglycemia
  ppv  <- kg(~undx_dm, subset(des, r == 1))
  N_flag <- tot(~r, des)
  tibble(rule = lab,
         volume = vol$p, volume_lo = vol$lo, volume_hi = vol$hi, N_flag = N_flag$N,
         sens_dm = e_dm$p, sens_dm_lo = e_dm$lo, sens_dm_hi = e_dm$hi,
         sens_dg = e_dg$p, sens_dg_lo = e_dg$lo, sens_dg_hi = e_dg$hi,
         yield = ppv$p, nns = 100 / ppv$p)
}
res$rules <- bind_rows(
  rule_perf(nodx, "uspstf_dm", "USPSTF 2021"),
  rule_perf(nodx, "ada_dm", "ADA 2025"))

# -----------------------------------------------------------------------------
# 6. Hypertension: USPSTF tier and care contact
# -----------------------------------------------------------------------------
htn_out <- function(var, lab) {
  s <- subset(dL_mec, htn_sample & get(var) == 1)
  s <- update(s, tier35 = as.integer(htn_tier == "Screening every 3-5 y"),
              no_usual = 1L - usual_src, unins = 1L - insured,
              in_care_missed = as.integer(usual_src == 1 & insured == 1))
  bind_rows(
    kg(~tier35, s) |> bind_cols(tot(~tier35, s)) |> mutate(m = "Less frequent screening tier (18-39 y, not Black, BMI<25)"),
    kg(~no_usual, subset(s, !is.na(no_usual))) |> bind_cols(tot(~no_usual, s)) |> mutate(m = "No usual source of care"),
    kg(~unins, subset(s, !is.na(unins))) |> bind_cols(tot(~unins, s)) |> mutate(m = "Uninsured"),
    kg(~in_care_missed, subset(s, !is.na(in_care_missed))) |> bind_cols(tot(~in_care_missed, s)) |> mutate(m = "Insured with usual source of care")
  ) |> mutate(outcome = lab, n_cases = n_unw(s))
}
res$htn <- bind_rows(htn_out("undx_htn130", "Undiagnosed hypertension (>=130/80)"),
                     htn_out("undx_htn140", "Undiagnosed hypertension (>=140/90)"))

# Hypertension by subgroup: prevalence of undiagnosed HTN (>=130/80) among adults without dx
nodx_htn <- subset(dL_mec, htn_sample & htn_dx == 0)
res$htn_sub <- map_dfr(c("age_cat", "female", "race", "bmi_cat", "pir", "insured", "usual_src"), function(v) {
  lv <- sort(unique(na.omit(nodx_htn$variables[[v]][weights(nodx_htn) > 0])))
  map_dfr(lv, function(l) {
    s <- subset(nodx_htn, get(v) == l)
    bind_rows(kg(~undx_htn130, s) |> mutate(def = ">=130/80"),
              kg(~undx_htn140, s) |> mutate(def = ">=140/90")) |>
      mutate(var = subvars[[v]], level = lab_level(v, l), n = n_unw(s))
  })
})

# -----------------------------------------------------------------------------
# 7. Overlap of undiagnosed conditions
# -----------------------------------------------------------------------------
both <- subset(dL_mec, dm_sample & htn_sample)
res$overlap <- kg(~undx_htn130, subset(both, undx_dm == 1))

# -----------------------------------------------------------------------------
# 8. Sensitivity analyses
# -----------------------------------------------------------------------------
sens <- list()
# (a) HbA1c or fasting glucose, fasting subsample weights
fdes <- subset(dL_fst, dm_sample & !is.na(fpg) & wt_fast > 0 & undx_dm_fpg == 1)
fdes <- update(fdes, out_usp = 1 - uspstf_dm, out_ada = 1 - ada_dm)
sens$fpg <- bind_rows(kg(~out_usp, fdes) |> mutate(rule = "USPSTF 2021"),
                      kg(~out_ada, fdes) |> mutate(rule = "ADA 2025")) |>
  mutate(analysis = "HbA1c >=6.5% or FPG >=126 mg/dL (fasting subsample)", n_cases = n_unw(fdes))
# (b) pooled 2017-2023
pdes <- subset(dPool, dm_sample & undx_dm == 1)
pdes <- update(pdes, out_usp = 1 - uspstf_dm, out_ada = 1 - ada_dm)
sens$pool <- bind_rows(kg(~out_usp, pdes) |> mutate(rule = "USPSTF 2021"),
                       kg(~out_ada, pdes) |> mutate(rule = "ADA 2025")) |>
  mutate(analysis = "Pooled 2017-2023", n_cases = n_unw(pdes))
# (c) 2017-2020 with family history and GDM added to ADA rule
p2 <- subset(dP_lab, dm_sample & undx_dm == 1)
p2 <- update(p2, out_usp = 1 - uspstf_dm, out_ada = 1 - ada_dm, out_ada_full = 1 - ada_dm_full)
sens$famhx <- bind_rows(kg(~out_usp, p2) |> mutate(rule = "USPSTF 2021"),
                        kg(~out_ada, p2) |> mutate(rule = "ADA 2025"),
                        kg(~out_ada_full, p2) |> mutate(rule = "ADA 2025 + family history + GDM")) |>
  mutate(analysis = "2017-2020 only", n_cases = n_unw(p2))
# (d) Restrict hypertension to >=140/90 already shown; (e) Asian BMI 25 shown in outside table
res$sens <- bind_rows(sens)

# Subgroup in pooled data (more stable race-specific estimates)
res$sub_dm_pool <- sub_est(pdes, "Undiagnosed diabetes (pooled 2017-2023)") |>
  filter(var %in% c("Race and ethnicity", "Age, y", "Sex"))

saveRDS(res, "output/res_main.rds")
iwalk(res, ~ if (is.data.frame(.x)) write.csv(.x, file.path("output", paste0(.y, ".csv")), row.names = FALSE))

# quick console summary
print(res$prev); print(res$share_undx); print(res$outside); print(res$reasons)
print(res$cascade); print(res$rules); print(res$htn); print(res$sens); print(res$elig_care)
print(res$overlap)
print(res$sub_dm |> filter(rule == "USPSTF 2021") |> select(var, level, n_cases, p, lo, hi, N, rse), n = 50)
