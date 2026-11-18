# Reproduce the full analysis and manuscript starting from the public NHANES files (downloaded automatically).
# Run from this folder:  Rscript run_all.R
for (d in c("output", "figures", "brief")) dir.create(d, showWarnings = FALSE)
for (f in c("R/00_download.R", "R/01_data.R", "R/02_analysis.R", "R/03_ml_conformal.R", "R/04_figures.R", "R/05_tables.R", "R/06_brief.R")) {
  message("Running ", f); source(f, echo = FALSE, local = new.env())
}
setwd("manuscript"); source("build.R", local = new.env())
