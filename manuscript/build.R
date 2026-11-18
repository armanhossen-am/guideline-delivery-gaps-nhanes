# =============================================================================
# build.R - number citations (order of first appearance, JAMA superscripts),
# insert the AMA reference list, and render the manuscript to Word.
# Run from the manuscript/ folder:  Rscript build.R
# =============================================================================
refs <- list(
  uspstf_dm = "US Preventive Services Task Force; Davidson KW, Barry MJ, Mangione CM, et al. Screening for prediabetes and type 2 diabetes: US Preventive Services Task Force recommendation statement. *JAMA*. 2021;326(8):736-743. doi:10.1001/jama.2021.12531",
  ada2025 = "American Diabetes Association Professional Practice Committee. 2. Diagnosis and classification of diabetes: Standards of Care in Diabetes—2025. *Diabetes Care*. 2025;48(suppl 1):S27-S49. doi:10.2337/dc25-S002",
  uspstf_htn = "US Preventive Services Task Force; Krist AH, Davidson KW, Mangione CM, et al. Screening for hypertension in adults: US Preventive Services Task Force reaffirmation recommendation statement. *JAMA*. 2021;325(16):1650-1656. doi:10.1001/jama.2021.4987",
  acc2017 = "Whelton PK, Carey RM, Aronow WS, et al. 2017 ACC/AHA/AAPA/ABC/ACPM/AGS/APhA/ASH/ASPC/NMA/PCNA guideline for the prevention, detection, evaluation, and management of high blood pressure in adults: a report of the American College of Cardiology/American Heart Association Task Force on Clinical Practice Guidelines. *Hypertension*. 2018;71(6):e13-e115. doi:10.1161/HYP.0000000000000065",
  aha2025 = "Jones DW, Ferdinand KC, Taler SJ, et al. 2025 AHA/ACC/AANP/AAPA/ABC/ACCP/ACPM/AGS/AMA/ASPC/NMA/PCNA/SGIM guideline for the prevention, detection, evaluation and management of high blood pressure in adults: a report of the American College of Cardiology/American Heart Association Joint Committee on Clinical Practice Guidelines. *Circulation*. 2025;152(11):e114-e218. doi:10.1161/CIR.0000000000001356",
  fang2022 = "Fang M, Wang D, Coresh J, Selvin E. Undiagnosed diabetes in U.S. adults: prevalence and trends. *Diabetes Care*. 2022;45(9):1994-2002. doi:10.2337/dc22-0242",
  aggarwal2022 = "Aggarwal R, Bibbins-Domingo K, Yeh RW, et al. Diabetes screening by race and ethnicity in the United States: equivalent body mass index and age thresholds. *Ann Intern Med*. 2022;175(6):765-773. doi:10.7326/M20-8079",
  obrien2018 = "O'Brien MJ, Bullard KM, Zhang Y, et al. Performance of the 2015 US Preventive Services Task Force screening criteria for prediabetes and undiagnosed diabetes. *J Gen Intern Med*. 2018;33(7):1100-1108. doi:10.1007/s11606-018-4436-4",
  gwira2024 = "Gwira JA, Fryar CD, Gu Q. Prevalence of total, diagnosed, and undiagnosed diabetes in adults: United States, August 2021–August 2023. *NCHS Data Brief*. 2024;(516). doi:10.15620/cdc/165794",
  fryar2024 = "Fryar CD, Kit B, Carroll MD, Afful J. Hypertension prevalence, awareness, treatment, and control among adults age 18 and older: United States, August 2021–August 2023. *NCHS Data Brief*. 2024;(511). doi:10.15620/cdc/164016",
  terry2024 = "Terry AL, Chiappa MM, McAllister J, Woodwell DA, Graber JE. Plan and operations of the National Health and Nutrition Examination Survey, August 2021–August 2023. *Vital Health Stat 1*. 2024;(66):1-21. doi:10.15620/cdc/151927",
  nchs_overview = "National Center for Health Statistics. Brief overview of sample design, nonresponse bias assessment, and analytic guidelines for NHANES August 2021–August 2023. September 20, 2024. Accessed November 17, 2026. https://wwwn.cdc.gov/nchs/nhanes/ContinuousNhanes/OverviewBrief.aspx?Cycle=2021-2023",
  stierman2021 = "Stierman B, Afful J, Carroll MD, et al. National Health and Nutrition Examination Survey 2017–March 2020 prepandemic data files—development of files and prevalence estimates for selected health outcomes. *Natl Health Stat Report*. 2021;(158). doi:10.15620/cdc:106273",
  angelopoulos = "Angelopoulos AN, Bates S. Conformal prediction: a gentle introduction. *Found Trends Mach Learn*. 2023;16(4):494-591. doi:10.1561/2200000101",
  vovk = "Vovk V, Gammerman A, Shafer G. *Algorithmic Learning in a Random World*. 2nd ed. Springer; 2022. doi:10.1007/978-3-031-06649-8",
  tibshirani = "Tibshirani RJ, Barber RF, Candès EJ, Ramdas A. Conformal prediction under covariate shift. In: *Advances in Neural Information Processing Systems 32 (NeurIPS 2019)*. Curran Associates; 2019:2530-2540.",
  tripod = "Collins GS, Moons KGM, Dhiman P, et al. TRIPOD+AI statement: updated guidance for reporting clinical prediction models that use regression or machine learning methods. *BMJ*. 2024;385:e078378. doi:10.1136/bmj-2023-078378",
  strobe = "von Elm E, Altman DG, Egger M, Pocock SJ, Gøtzsche PC, Vandenbroucke JP; STROBE Initiative. The Strengthening the Reporting of Observational Studies in Epidemiology (STROBE) statement: guidelines for reporting observational studies. *Lancet*. 2007;370(9596):1453-1457. doi:10.1016/S0140-6736(07)61602-X",
  korn = "Korn EL, Graubard BI. Confidence intervals for proportions with small expected number of positive counts estimated from survey data. *Surv Methodol*. 1998;24(2):193-201.",
  parker = "Parker JD, Talih M, Malec DJ, et al. National Center for Health Statistics data presentation standards for proportions. *Vital Health Stat 2*. 2017;(175):1-22.",
  bang2009 = "Bang H, Edwards AM, Bomback AS, et al. Development and validation of a patient self-assessment score for diabetes risk. *Ann Intern Med*. 2009;151(11):775-783. doi:10.7326/0003-4819-151-11-200912010-00005",
  selvin2018 = "Selvin E, Wang D, Matsushita K, Grams ME, Coresh J. Prognostic implications of single-sample confirmatory testing for undiagnosed diabetes: a prospective cohort study. *Ann Intern Med*. 2018;169(3):156-164. doi:10.7326/M18-0091",
  chen2016 = "Chen T, Guestrin C. XGBoost: a scalable tree boosting system. In: *Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining*. ACM; 2016:785-794. doi:10.1145/2939672.2939785",
  lumley = "Lumley T. Analysis of complex survey samples. *J Stat Softw*. 2004;9(8):1-19. doi:10.18637/jss.v009.i08",
  herman2015 = "Herman WH, Ye W, Griffin SJ, et al. Early detection and treatment of type 2 diabetes reduce cardiovascular morbidity and mortality: a simulation of the results of the Anglo-Danish-Dutch Study of Intensive Treatment in People With Screen-Detected Diabetes in Primary Care (ADDITION-Europe). *Diabetes Care*. 2015;38(8):1449-1455. doi:10.2337/dc14-2459",
  dpp2002 = "Knowler WC, Barrett-Connor E, Fowler SE, et al; Diabetes Prevention Program Research Group. Reduction in the incidence of type 2 diabetes with lifestyle intervention or metformin. *N Engl J Med*. 2002;346(6):393-403. doi:10.1056/NEJMoa012512",
  hsu2015 = "Hsu WC, Araneta MRG, Kanaya AM, Chiang JL, Fujimoto W. BMI cut points to identify at-risk Asian Americans for type 2 diabetes screening. *Diabetes Care*. 2015;38(1):150-158. doi:10.2337/dc14-2391",
  leroith2019 = "LeRoith D, Biessels GJ, Braithwaite SS, et al. Treatment of diabetes in older adults: an Endocrine Society clinical practice guideline. *J Clin Endocrinol Metab*. 2019;104(5):1520-1574. doi:10.1210/jc.2019-00198",
  wall2014 = "Wall HK, Hannan JA, Wright JS. Patients with undiagnosed hypertension: hiding in plain sight. *JAMA*. 2014;312(19):1973-1974. doi:10.1001/jama.2014.15388",
  gopalan2015 = "Gopalan A, Lorincz IS, Wirtalla C, Marcus SC, Long JA. Awareness of prediabetes and engagement in diabetes risk-reducing behaviors. *Am J Prev Med*. 2015;49(4):512-519. doi:10.1016/j.amepre.2015.03.007",
  ada_older = "American Diabetes Association Professional Practice Committee. 13. Older adults: Standards of Care in Diabetes—2025. *Diabetes Care*. 2025;48(suppl 1):S266-S282. doi:10.2337/dc25-S013",
  fang2022b = "Fang M, Wang D, Echouffo-Tcheugui JB, Selvin E. Prediabetes and diabetes screening eligibility and detection in US adults after changes to US Preventive Services Task Force and American Diabetes Association recommendations. *JAMA*. 2022;327(19):1924-1925. doi:10.1001/jama.2022.5185",
  ali2023 = "Ali MK, Imperatore G, Benoit SR, et al. Impact of changes in diabetes screening guidelines on testing eligibility and potential yield among adults without diagnosed diabetes in the United States. *Diabetes Res Clin Pract*. 2023;197:110572. doi:10.1016/j.diabres.2023.110572",
  olsson2022 = "Olsson H, Kartasalo K, Mulliqi N, et al. Estimating diagnostic uncertainty in artificial intelligence assisted pathology using conformal prediction. *Nat Commun*. 2022;13(1):7761. doi:10.1038/s41467-022-34945-8",
  sreenivasan2025 = "Sreenivasan AP, Vaivade A, Noui Y, et al. Conformal prediction enables disease course prediction and allows individualized diagnostic uncertainty in multiple sclerosis. *NPJ Digit Med*. 2025;8(1):224. doi:10.1038/s41746-025-01616-z",
  muntner2020 = "Muntner P, Hardy ST, Fine LJ, et al. Trends in blood pressure control among US adults with hypertension, 1999-2000 to 2017-2018. *JAMA*. 2020;324(12):1190-1200. doi:10.1001/jama.2020.14545",
  nahin_cascade = "Nahin KSA, Rony AR, Tasnim SJ, Islam T, Hossen A, Asha AS. The hypertension care cascade in Bangladesh: a nationally representative analysis of BDHS 2022. *J Epidemiol Glob Health*. Published online October 17, 2026. doi:10.1007/s44197-026-00653-5",
  nahin_prehtn = "Nahin KSA, Hossen A. Pre-hypertension in Bangladesh: evidence from BDHS 2022. *Front Public Health*. 2026;14:1777932. doi:10.3389/fpubh.2026.1777932",
  carson2010 = "Carson AP, Reynolds K, Fonseca VA, Muntner P. Comparison of A1C and fasting glucose criteria to diagnose diabetes among U.S. adults. *Diabetes Care*. 2010;33(1):95-97. doi:10.2337/dc09-1227"
)
# keys used in text that expand to more than one reference
alias <- list(uspstf_eval = c("fang2022b", "ali2023"), conformal_clin = c("olsson2022", "sreenivasan2025"),
              nhanes_design = c("terry2024", "nchs_overview"), a1c_fpg = "carson2010")

src <- readLines("manuscript_src.Rmd", warn = FALSE) |> paste(collapse = "\n")
tags <- regmatches(src, gregexpr("\\{\\{[^}]+\\}\\}", src))[[1]]
tags <- setdiff(tags, "{{REFERENCES}}")
order <- character(0)
expand <- function(k) if (!is.null(alias[[k]])) alias[[k]] else k
for (t in tags) for (k in strsplit(gsub("[{}]", "", t), ",")[[1]]) for (kk in expand(trimws(k)))
  if (!kk %in% order) order <- c(order, kk)
stopifnot(all(order %in% names(refs)))
compress <- function(x) {             # 1,2,3,5 -> 1-3,5
  x <- sort(unique(x)); out <- c(); i <- 1
  while (i <= length(x)) { j <- i; while (j < length(x) && x[j + 1] == x[j] + 1) j <- j + 1
    out <- c(out, if (j - i >= 2) paste0(x[i], "-", x[j]) else if (j > i) paste0(x[i], ",", x[j]) else x[i]); i <- j + 1 }
  paste(out, collapse = ",")
}
for (t in unique(tags)) {
  ks <- unlist(lapply(trimws(strsplit(gsub("[{}]", "", t), ",")[[1]]), expand))
  src <- gsub(t, paste0("^", compress(match(ks, order)), "^"), src, fixed = TRUE)
}
reflist <- paste0(seq_along(order), ". ", unlist(refs[order]), collapse = "\n\n")
src <- sub("{{REFERENCES}}", reflist, src, fixed = TRUE)
writeLines(src, "manuscript.Rmd")
cat(length(order), "references numbered\n")
rmarkdown::render("manuscript.Rmd", output_file = "Manuscript_JAMANetwOpen_UndiagnosedDiabetesHypertension.docx", quiet = TRUE)
