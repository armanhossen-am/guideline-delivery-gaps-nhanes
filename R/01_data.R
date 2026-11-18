# =============================================================================
# 01_data.R
# Build a harmonized adult analytic file from NHANES 2017-Mar 2020 (pre-pandemic)
# and NHANES Aug 2021-Aug 2023.
# Output: data/analytic.rds
# =============================================================================
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(purrr)
})

rd <- function(f) read_xpt(file.path("data/raw", paste0(f, ".xpt"))) |> zap_labels()
mjoin <- function(files) reduce(map(files, rd), left_join, by = "SEQN")

# ---- 2021-2023 (cycle "L") ---------------------------------------------------
L <- mjoin(c("DEMO_L", "BMX_L", "BPXO_L", "GHB_L", "GLU_L", "DIQ_L", "BPQ_L",
             "MCQ_L", "HIQ_L", "HUQ_L", "SMQ_L", "HDL_L", "TRIGLY_L", "PAQ_L")) |>
  transmute(
    SEQN, cycle = "2021-2023",
    wt_mec = WTMEC2YR, wt_lab = WTPH2YR.x, wt_fast = WTSAF2YR.x,
    strata = SDMVSTRA, psu = SDMVPSU,
    RIDSTATR, RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2, INDFMPIR, DMDBORN4, RIDEXPRG,
    BMXBMI, BMXWAIST,
    BPXOSY1, BPXOSY2, BPXOSY3, BPXODI1, BPXODI2, BPXODI3,
    LBXGH, LBXGLU, LBDHDD, TG = LBXTLG,
    DIQ010, DIQ160, DIQ180, DIQ050, DIQ070,
    BPQ020, bp_med = as.integer(BPQ150 == 1),
    MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F,
    famhx = NA_integer_, gdm = NA_integer_,          # not collected in 2021-2023
    HIQ011, HUQ030, SMQ020, SMQ040,
    inactive = case_when(PAD790Q == 0 & PAD810Q == 0 ~ 1L,
                         PAD790Q %in% 1:9000 | PAD810Q %in% 1:9000 ~ 0L)
  )

# ---- 2017-Mar 2020 pre-pandemic (cycle "P") ----------------------------------
P <- mjoin(c("P_DEMO", "P_BMX", "P_BPXO", "P_GHB", "P_GLU", "P_DIQ", "P_BPQ",
             "P_MCQ", "P_HIQ", "P_HUQ", "P_SMQ", "P_HDL", "P_TRIGLY", "P_PAQ", "P_RHQ")) |>
  transmute(
    SEQN, cycle = "2017-2020",
    wt_mec = WTMECPRP, wt_lab = WTMECPRP, wt_fast = WTSAFPRP.x,
    strata = SDMVSTRA, psu = SDMVPSU,
    RIDSTATR, RIDAGEYR, RIAGENDR, RIDRETH3, DMDEDUC2, INDFMPIR, DMDBORN4, RIDEXPRG,
    BMXBMI, BMXWAIST,
    BPXOSY1, BPXOSY2, BPXOSY3, BPXODI1, BPXODI2, BPXODI3,
    LBXGH, LBXGLU, LBDHDD, TG = LBXTR,
    DIQ010, DIQ160, DIQ180, DIQ050, DIQ070,
    BPQ020, bp_med = as.integer(BPQ050A == 1),
    MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F,
    famhx = case_when(MCQ300C == 1 ~ 1L, MCQ300C == 2 ~ 0L),
    gdm   = case_when(RHQ162 == 1 ~ 1L, RHQ162 == 2 ~ 0L),
    HIQ011, HUQ030, SMQ020, SMQ040,
    inactive = case_when(PAQ650 == 2 & PAQ665 == 2 ~ 1L,
                         PAQ650 == 1 | PAQ665 == 1 ~ 0L)
  )

# ---- Harmonize and derive ----------------------------------------------------
d <- bind_rows(P, L) |>
  mutate(
    sbp = rowMeans(cbind(BPXOSY1, BPXOSY2, BPXOSY3), na.rm = TRUE),
    dbp = rowMeans(cbind(BPXODI1, BPXODI2, BPXODI3), na.rm = TRUE),
    sbp = ifelse(is.nan(sbp), NA, sbp), dbp = ifelse(is.nan(dbp), NA, dbp),
    age = RIDAGEYR,
    female = as.integer(RIAGENDR == 2),
    race = factor(case_when(RIDRETH3 %in% 1:2 ~ "Hispanic",
                            RIDRETH3 == 3 ~ "Non-Hispanic White",
                            RIDRETH3 == 4 ~ "Non-Hispanic Black",
                            RIDRETH3 == 6 ~ "Non-Hispanic Asian",
                            RIDRETH3 == 7 ~ "Other or multiracial"),
                  levels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic",
                             "Non-Hispanic Asian", "Other or multiracial")),
    asian = race == "Non-Hispanic Asian",
    educ = factor(case_when(DMDEDUC2 %in% 1:2 ~ "Less than high school",
                            DMDEDUC2 == 3 ~ "High school or GED",
                            DMDEDUC2 == 4 ~ "Some college",
                            DMDEDUC2 == 5 ~ "College graduate"),
                  levels = c("Less than high school", "High school or GED",
                             "Some college", "College graduate")),
    pir = factor(case_when(INDFMPIR < 1.3 ~ "<1.30",
                           INDFMPIR < 3.5 ~ "1.30-3.49",
                           INDFMPIR >= 3.5 ~ ">=3.50"),
                 levels = c("<1.30", "1.30-3.49", ">=3.50")),
    foreign_born = case_when(DMDBORN4 == 1 ~ 0L, DMDBORN4 == 2 ~ 1L),
    insured = case_when(HIQ011 == 1 ~ 1L, HIQ011 == 2 ~ 0L),
    usual_src = case_when(HUQ030 %in% c(1, 3) ~ 1L, HUQ030 == 2 ~ 0L),
    blood_test3y = case_when(DIQ180 == 1 ~ 1L, DIQ180 == 2 ~ 0L),
    smoke = factor(case_when(SMQ020 == 2 ~ "Never",
                             SMQ020 == 1 & SMQ040 == 3 ~ "Former",
                             SMQ020 == 1 & SMQ040 %in% 1:2 ~ "Current"),
                   levels = c("Never", "Former", "Current")),
    cvd = as.integer(if_any(c(MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F), ~ .x %in% 1)),
    bmi = BMXBMI, waist = BMXWAIST,
    bmi_cat = factor(case_when(bmi < 25 ~ "<25", bmi < 30 ~ "25-29.9", bmi >= 30 ~ ">=30"),
                     levels = c("<25", "25-29.9", ">=30")),
    age_cat = cut(age, c(19, 34, 44, 64, 70, Inf),
                  labels = c("20-34", "35-44", "45-64", "65-70", ">=71")),
    # ---- diabetes status
    dm_dx    = case_when(DIQ010 == 1 ~ 1L, DIQ010 %in% 2:3 ~ 0L),
    predm_dx = as.integer(DIQ160 %in% 1 | DIQ010 %in% 3),
    a1c = LBXGH, fpg = LBXGLU,
    undx_dm    = as.integer(dm_dx == 0 & a1c >= 6.5),
    undx_predm = as.integer(dm_dx == 0 & predm_dx == 0 & a1c >= 5.7 & a1c < 6.5),
    undx_dysgly = as.integer(undx_dm == 1 | undx_predm == 1),
    undx_dm_fpg = as.integer(dm_dx == 0 & (a1c >= 6.5 | fpg >= 126)),
    # ---- hypertension status (ACC/AHA 2017 and 2025 thresholds)
    htn_dx = case_when(BPQ020 == 1 | bp_med %in% 1 ~ 1L, BPQ020 == 2 ~ 0L),
    high_bp130 = as.integer(sbp >= 130 | dbp >= 80),
    high_bp140 = as.integer(sbp >= 140 | dbp >= 90),
    undx_htn130 = as.integer(htn_dx == 0 & high_bp130 == 1),
    undx_htn140 = as.integer(htn_dx == 0 & high_bp140 == 1),
    # ---- risk factors used by ADA Standards of Care 2025 (Table 2.3)
    bmi_thr = ifelse(asian, 23, 25),
    overwt_ada = as.integer(bmi >= bmi_thr),
    hi_risk_race = as.integer(race != "Non-Hispanic White"),
    htn_any = as.integer(htn_dx %in% 1 | high_bp130 %in% 1),
    dyslip = as.integer(coalesce(LBDHDD < 35, FALSE) | coalesce(TG > 250, FALSE)),
    # ---- guideline eligibility (diabetes)
    uspstf_dm = as.integer(age >= 35 & age <= 70 & bmi >= bmi_thr),
    uspstf_dm_strict = as.integer(age >= 35 & age <= 70 & bmi >= 25),
    ada_rf_n = hi_risk_race + cvd + htn_any + coalesce(dyslip, 0L) +
               coalesce(inactive, 0L),
    ada_dm = as.integer(age >= 35 | predm_dx == 1 |
                        (overwt_ada == 1 & ada_rf_n >= 1)),
    ada_dm_full = as.integer(ada_dm == 1 | coalesce(gdm, 0L) == 1 |
                             (overwt_ada == 1 & coalesce(famhx, 0L) == 1)),
    uspstf_reason = factor(case_when(uspstf_dm == 1 ~ "Eligible",
                                     age < 35 ~ "Younger than 35 y",
                                     age > 70 ~ "Older than 70 y",
                                     TRUE ~ "Age 35-70 y, BMI below threshold"),
                           levels = c("Eligible", "Younger than 35 y",
                                      "Age 35-70 y, BMI below threshold",
                                      "Older than 70 y")),
    # ---- USPSTF 2021 hypertension screening tier
    htn_tier = factor(ifelse(age >= 40 | race == "Non-Hispanic Black" | bmi >= 25,
                             "Annual screening", "Screening every 3-5 y"),
                      levels = c("Annual screening", "Screening every 3-5 y")),
    # ---- pooled weights (3.2 y + 2.0 y = 5.2 y)
    wt_lab_pool = ifelse(cycle == "2021-2023", wt_lab * 2 / 5.2, wt_lab * 3.2 / 5.2),
    wt_mec_pool = ifelse(cycle == "2021-2023", wt_mec * 2 / 5.2, wt_mec * 3.2 / 5.2)
  )

# ---- Analytic flags ----------------------------------------------------------
d <- d |>
  mutate(
    adult = age >= 20,
    examined = RIDSTATR == 2,
    not_preg = !(RIDEXPRG %in% 1),
    dm_sample = adult & examined & not_preg & !is.na(a1c) & !is.na(dm_dx) &
                !is.na(bmi) & wt_lab > 0,
    htn_sample = adult & examined & not_preg & !is.na(sbp) & !is.na(htn_dx) &
                 !is.na(bmi) & wt_mec > 0
  )

saveRDS(d, "data/analytic.rds")

# ---- Flow counts for the participant diagram ---------------------------------
flow <- d |>
  group_by(cycle) |>
  summarise(
    n_total = n(),
    n_adults = sum(adult),
    n_examined = sum(adult & examined),
    n_notpreg = sum(adult & examined & not_preg),
    n_hba1c = sum(adult & examined & not_preg & !is.na(a1c)),
    n_dm_final = sum(dm_sample),
    n_dm_dx = sum(dm_sample & dm_dx == 1),
    n_dm_nodx = sum(dm_sample & dm_dx == 0),
    n_undx_dm = sum(dm_sample & undx_dm %in% 1),
    n_undx_predm = sum(dm_sample & undx_predm %in% 1),
    n_bp = sum(adult & examined & not_preg & !is.na(sbp)),
    n_htn_final = sum(htn_sample),
    n_htn_dx = sum(htn_sample & htn_dx == 1),
    n_htn_nodx = sum(htn_sample & htn_dx == 0),
    n_undx_htn130 = sum(htn_sample & undx_htn130 %in% 1),
    n_undx_htn140 = sum(htn_sample & undx_htn140 %in% 1),
    .groups = "drop")
print(t(flow))
saveRDS(flow, "output/flow_counts.rds")
