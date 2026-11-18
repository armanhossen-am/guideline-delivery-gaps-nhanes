# =============================================================================
# 03_ml_conformal.R
# Non-laboratory risk model for undiagnosed diabetes (and dysglycemia),
# developed in NHANES 2017-Mar 2020 and temporally validated in Aug 2021-Aug 2023.
# Split (Mondrian, class-conditional) conformal prediction converts risk scores
# into screening rules with a target sensitivity (coverage of true cases).
# Output: output/ml_*.rds
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(xgboost); library(survey); library(splines); library(purrr); library(tidyr)
})
options(survey.lonely.psu = "adjust")
set.seed(20231118)

d <- readRDS("data/analytic.rds")
feats <- c("age", "female", "race", "bmi", "waist", "sbp", "dbp", "htn_dx", "cvd",
           "smoke", "inactive", "educ", "pir", "foreign_born", "insured",
           "usual_src", "predm_dx")

dev  <- d |> filter(cycle == "2017-2020", dm_sample, dm_dx == 0)
test <- d |> filter(cycle == "2021-2023", dm_sample, dm_dx == 0)

# ---- design matrix (one-hot; NA kept for xgboost, imputed for logistic) -----
mm <- function(df) {
  x <- df |> transmute(
    age, female, bmi, waist, sbp, dbp, htn_dx, cvd, inactive, foreign_born,
    insured, usual_src, predm_dx,
    race_black = as.integer(race == "Non-Hispanic Black"),
    race_hisp  = as.integer(race == "Hispanic"),
    race_asian = as.integer(race == "Non-Hispanic Asian"),
    race_other = as.integer(race == "Other or multiracial"),
    smoke_former = as.integer(smoke == "Former"), smoke_current = as.integer(smoke == "Current"),
    educ_lths = as.integer(educ == "Less than high school"),
    educ_hs = as.integer(educ == "High school or GED"),
    educ_some = as.integer(educ == "Some college"),
    pir_low = as.integer(pir == "<1.30"), pir_mid = as.integer(pir == "1.30-3.49"))
  as.matrix(x)
}
X_dev <- mm(dev); X_test <- mm(test)
med <- apply(X_dev, 2, median, na.rm = TRUE)
imp <- function(X) { for (j in seq_len(ncol(X))) X[is.na(X[, j]), j] <- med[j]; X }

# ---- development split: 70% training, 30% conformal calibration -------------
run_outcome <- function(yvar) {
  y_dev <- dev[[yvar]]; y_test <- test[[yvar]]
  w_dev <- dev$wt_lab / mean(dev$wt_lab); w_test <- test$wt_lab
  idx_pos <- which(y_dev == 1); idx_neg <- which(y_dev == 0)
  tr <- c(sample(idx_pos, round(0.7 * length(idx_pos))),
          sample(idx_neg, round(0.7 * length(idx_neg))))
  ca <- setdiff(seq_along(y_dev), tr)

  # (1) logistic regression with natural splines (reference model)
  df_tr <- data.frame(imp(X_dev[tr, ]), y = y_dev[tr])
  f_glm <- y ~ ns(age, 3) + ns(bmi, 3) + ns(waist, 3) + ns(sbp, 3) + dbp + female +
    htn_dx + cvd + inactive + foreign_born + insured + usual_src + predm_dx +
    race_black + race_hisp + race_asian + race_other + smoke_former + smoke_current +
    educ_lths + educ_hs + educ_some + pir_low + pir_mid
  m_glm <- suppressWarnings(glm(f_glm, family = quasibinomial, data = df_tr, weights = w_dev[tr]))
  p_glm <- function(X) as.numeric(predict(m_glm, newdata = data.frame(imp(X)), type = "response"))

  # (2) gradient boosting, tuned by 5-fold CV on the training split
  dtr <- xgb.DMatrix(X_dev[tr, ], label = y_dev[tr], weight = w_dev[tr])
  grid <- expand.grid(eta = c(0.02, 0.05), max_depth = c(2, 3, 4), min_child_weight = c(5, 10))
  cvres <- pmap_dfr(grid, function(eta, max_depth, min_child_weight) {
    cv <- xgb.cv(params = list(objective = "binary:logistic", eval_metric = "logloss",
                               eta = eta, max_depth = max_depth,
                               min_child_weight = min_child_weight,
                               subsample = 0.8, colsample_bytree = 0.8),
                 data = dtr, nrounds = 1500, nfold = 5, early_stopping_rounds = 50,
                 verbose = 0)
    tibble(eta, max_depth, min_child_weight, best_iter = which.min(cv$evaluation_log$test_logloss_mean),
           loss = min(cv$evaluation_log$test_logloss_mean))
  })
  best <- slice_min(cvres, loss, n = 1, with_ties = FALSE)
  m_xgb <- xgb.train(params = list(objective = "binary:logistic", eta = best$eta,
                                   max_depth = best$max_depth,
                                   min_child_weight = best$min_child_weight,
                                   subsample = 0.8, colsample_bytree = 0.8),
                     data = dtr, nrounds = best$best_iter, verbose = 0)
  p_xgb <- function(X) predict(m_xgb, xgb.DMatrix(X))

  list(yvar = yvar, tr = tr, ca = ca, m_glm = m_glm, m_xgb = m_xgb, best = best,
       p_ca = list(glm = p_glm(X_dev[ca, ]), xgb = p_xgb(X_dev[ca, ])),
       p_test = list(glm = p_glm(X_test), xgb = p_xgb(X_test)),
       y_ca = y_dev[ca], w_ca = dev$wt_lab[ca], y_test = y_test, w_test = w_test)
}

# ---- weighted metrics --------------------------------------------------------
wauc <- function(p, y, w) {           # weighted Mann-Whitney AUC
  pos <- y == 1; o <- order(p)
  p <- p[o]; y <- y[o]; w <- w[o]; pos <- y == 1
  # handle ties by averaging ranks within tied groups
  wneg_cum <- cumsum(ifelse(!pos, w, 0))
  tie_grp <- match(p, unique(p))
  neg_below <- ave(wneg_cum - ifelse(!pos, w, 0), tie_grp, FUN = min)
  neg_tied  <- ave(ifelse(!pos, w, 0), tie_grp, FUN = sum)
  sum(w[pos] * (neg_below[pos] + 0.5 * neg_tied[pos])) / (sum(w[pos]) * sum(w[!pos]))
}
wquant <- function(x, w, prob) {     # weighted quantile (inverse CDF)
  o <- order(x); x <- x[o]; w <- w[o]; cw <- cumsum(w) / sum(w)
  x[min(which(cw >= prob))]
}
# Weighted split-conformal threshold for the positive class with finite-sample
# correction: score s = 1 - p; include "case" in the prediction set if s <= q.
conf_thr <- function(p_ca, y_ca, w_ca, alpha) {
  s <- 1 - p_ca[y_ca == 1]; w <- w_ca[y_ca == 1]
  n <- length(s); w <- w / sum(w)
  lev <- min(1, ceiling((n + 1) * (1 - alpha)) / n)
  1 - wquant(s, w, lev)               # flag if p >= threshold
}
conf_thr0 <- function(p_ca, y_ca, w_ca, alpha) {   # negative class: s = p
  s <- p_ca[y_ca == 0]; w <- w_ca[y_ca == 0]; n <- length(s)
  lev <- min(1, ceiling((n + 1) * (1 - alpha)) / n)
  wquant(s, w / sum(w), lev)          # "non-case" in set if p <= threshold
}

res_ml <- list()
for (yv in c("undx_dm", "undx_dysgly")) {
  r <- run_outcome(yv)
  # rep-weight design for test set (subbootstrap) for AUC uncertainty
  des_t <- svydesign(ids = ~psu, strata = ~strata, weights = ~wt_lab, nest = TRUE, data = test)
  rep <- as.svrepdesign(des_t, type = "subbootstrap", replicates = 200)
  RW <- weights(rep, type = "analysis")
  auc_tab <- map_dfr(c("glm", "xgb"), function(m) {
    p <- r$p_test[[m]]
    est <- wauc(p, r$y_test, r$w_test)
    reps <- apply(RW, 2, function(w) wauc(p, r$y_test, w))
    se <- sd(reps)
    # calibration-in-the-large and slope (weighted)
    lp <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
    cs <- suppressWarnings(glm(r$y_test ~ lp, family = quasibinomial, weights = r$w_test / mean(r$w_test)))
    ci <- suppressWarnings(glm(r$y_test ~ offset(lp), family = quasibinomial, weights = r$w_test / mean(r$w_test)))
    tibble(model = m, auc = est, auc_lo = est - 1.96 * se, auc_hi = est + 1.96 * se,
           cal_slope = coef(cs)[2], cal_int = coef(ci)[1],
           obs = weighted.mean(r$y_test, r$w_test), exp = weighted.mean(p, r$w_test),
           brier = weighted.mean((p - r$y_test)^2, r$w_test))
  })
  # development (calibration split) AUC for reference
  auc_ca <- map_dbl(c(glm = "glm", xgb = "xgb"), ~ wauc(r$p_ca[[.x]], r$y_ca, r$w_ca))

  # choose model with higher calibration-set AUC (pre-specified rule)
  mbest <- names(which.max(auc_ca))
  pc <- r$p_ca[[mbest]]; pt <- r$p_test[[mbest]]

  # conformal rules at several alphas
  alphas <- c(0.05, 0.10, 0.15, 0.20)
  conf <- map_dfr(alphas, function(a) {
    t1 <- conf_thr(pc, r$y_ca, r$w_ca, a)
    t0 <- conf_thr0(pc, r$y_ca, r$w_ca, a)
    tibble(alpha = a, thr_case = t1, thr_noncase = t0)
  })
  # matched-volume threshold: same share flagged as USPSTF in calibration data
  vol_usp <- weighted.mean(dev$uspstf_dm[r$ca], r$w_ca)
  thr_match <- wquant(pc, r$w_ca, 1 - vol_usp)

  # attach flags to full 2021-2023 data for survey-based CIs
  flag_df <- test |> select(SEQN) |> mutate(p_hat = pt)
  for (i in seq_len(nrow(conf))) {
    flag_df[[paste0("conf", conf$alpha[i] * 100)]] <- as.integer(pt >= conf$thr_case[i])
    flag_df[[paste0("set", conf$alpha[i] * 100)]] <- case_when(
      pt >= conf$thr_case[i] & pt <= conf$thr_noncase[i] ~ "Uncertain {0,1}",
      pt >= conf$thr_case[i] ~ "Flag {1}",
      pt <= conf$thr_noncase[i] ~ "Rule out {0}",
      TRUE ~ "Empty")
  }
  flag_df$match_usp <- as.integer(pt >= thr_match)

  # sensitivity/volume curve across thresholds (for figure)
  thr_grid <- quantile(pt, probs = seq(0, 0.995, by = 0.005))
  curve <- map_dfr(thr_grid, function(t) tibble(
    thr = t,
    volume = 100 * weighted.mean(pt >= t, r$w_test),
    sens = 100 * weighted.mean(pt[r$y_test == 1] >= t, r$w_test[r$y_test == 1])))

  # calibration by decile (weighted)
  calib <- tibble(p = pt, y = r$y_test, w = r$w_test) |>
    mutate(bin = ntile(p, 10)) |> group_by(bin) |>
    summarise(pred = weighted.mean(p, w), obs = weighted.mean(y, w), n = n(), .groups = "drop")

  # feature importance (xgb gain)
  imp_tab <- xgb.importance(model = r$m_xgb)

  res_ml[[yv]] <- list(auc = auc_tab, auc_ca = auc_ca, best_model = mbest, tune = r$best,
                       conf = conf, thr_match = thr_match, vol_usp_cal = vol_usp,
                       flags = flag_df, curve = curve, calib = calib, imp = imp_tab,
                       n_dev = nrow(dev), n_tr = length(r$tr), n_ca = length(r$ca),
                       pos_tr = sum(dev[[yv]][r$tr]), pos_ca = sum(r$y_ca),
                       n_test = nrow(test), pos_test = sum(r$y_test))
  cat("\n===", yv, "best:", mbest, "\n"); print(auc_tab); print(auc_ca); print(conf)
}

# ---- survey-based evaluation of rules in 2021-2023 ---------------------------
L <- d |> filter(cycle == "2021-2023")
eval_rules <- function(yv) {
  f <- res_ml[[yv]]$flags
  LL <- L |> left_join(f, by = "SEQN")
  LL$.w <- coalesce(LL$wt_lab, 0)
  des <- svydesign(ids = ~psu, strata = ~strata, weights = ~.w, nest = TRUE, data = LL)
  des <- subset(des, dm_sample & dm_dx == 0)
  rules <- c("uspstf_dm" = "USPSTF 2021", "ada_dm" = "ADA 2025",
             "match_usp" = "Risk model, volume matched to USPSTF",
             "conf20" = "Conformal rule, target sensitivity 80%",
             "conf15" = "Conformal rule, target sensitivity 85%",
             "conf10" = "Conformal rule, target sensitivity 90%",
             "conf5"  = "Conformal rule, target sensitivity 95%")
  kgp <- function(fm, ds) {
    e <- suppressWarnings(svyciprop(fm, ds, method = "beta")); ci <- attr(e, "ci")
    c(100 * as.numeric(e), 100 * ci[1], 100 * ci[2])
  }
  map_dfr(names(rules), function(rv) {
    ds <- update(des, r = get(rv), y = get(yv))
    vol <- kgp(~r, ds)
    se  <- kgp(~r, subset(ds, y == 1))
    ppv <- kgp(~y, subset(ds, r == 1))
    cap <- if (yv == "undx_dm") kgp(~r, subset(ds, y == 1 & uspstf_dm == 0)) else
      kgp(~r, subset(ds, y == 1 & uspstf_dm == 0))
    Nf <- coef(svytotal(~r, ds)) / 1e6
    tibble(outcome = yv, rule = rules[[rv]], volume = vol[1], volume_lo = vol[2], volume_hi = vol[3],
           N_screen = Nf, sens = se[1], sens_lo = se[2], sens_hi = se[3],
           ppv = ppv[1], nns = 100 / ppv[1],
           capture_missed = cap[1], capture_lo = cap[2], capture_hi = cap[3])
  })
}
res_ml$eval <- bind_rows(eval_rules("undx_dm"), eval_rules("undx_dysgly"))
print(res_ml$eval, width = 200)

# ---- subgroup sensitivity in 2021-2023 (undiagnosed diabetes, pooled rule) ---
f <- res_ml$undx_dm$flags
LL <- L |> left_join(f, by = "SEQN"); LL$.w <- coalesce(LL$wt_lab, 0)
des <- subset(svydesign(ids = ~psu, strata = ~strata, weights = ~.w, nest = TRUE, data = LL),
              dm_sample & undx_dm == 1)
res_ml$sub_sens <- map_dfr(c("age_cat", "race", "female"), function(v) {
  lv <- sort(unique(na.omit(des$variables[[v]][weights(des) > 0])))
  map_dfr(lv, function(l) {
    s <- subset(des, get(v) == l)
    tibble(var = v, level = as.character(l), n = sum(weights(s) > 0),
           uspstf = 100 * coef(svymean(~uspstf_dm, s)),
           conf10 = 100 * coef(svymean(~conf10, s)))
  })
})
print(res_ml$sub_sens)

# conformal set composition (test, undx_dm, alpha=0.10)
res_ml$sets <- LL |> filter(dm_sample, dm_dx == 0) |>
  group_by(set10) |> summarise(n = n(), w = sum(wt_lab), cases = sum(undx_dm), .groups = "drop") |>
  mutate(pct = 100 * w / sum(w))
print(res_ml$sets)

saveRDS(res_ml, "output/res_ml.rds")
