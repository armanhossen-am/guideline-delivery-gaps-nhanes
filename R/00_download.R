# =============================================================================
# 00_download.R - download the public NHANES files used in this project
# Source: CDC/NCHS (https://wwwn.cdc.gov/nchs/nhanes/). Output: data/raw/*.xpt
# =============================================================================
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
files <- list(
  "2021" = c("DEMO_L", "BMX_L", "BPXO_L", "GHB_L", "GLU_L", "DIQ_L", "BPQ_L", "MCQ_L", "HIQ_L",
             "HUQ_L", "SMQ_L", "RHQ_L", "HDL_L", "TRIGLY_L", "PAQ_L"),
  "2017" = c("P_DEMO", "P_BMX", "P_BPXO", "P_GHB", "P_GLU", "P_DIQ", "P_BPQ", "P_MCQ", "P_HIQ",
             "P_HUQ", "P_SMQ", "P_RHQ", "P_HDL", "P_TRIGLY", "P_PAQ"))
for (yr in names(files)) for (f in files[[yr]]) {
  dest <- file.path("data/raw", paste0(f, ".xpt"))
  if (file.exists(dest)) next
  url <- sprintf("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/%s/DataFiles/%s.xpt", yr, f)
  message("Downloading ", f)
  download.file(url, dest, mode = "wb", quiet = TRUE)
}
