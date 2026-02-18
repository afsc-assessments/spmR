#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(devtools)
  library(readr)
})

load_all("spmR")

example_dir <- "spmR/examples/atka"

# Run RTMB path and write spm_detail_rtmb.csv
rtmb <- runSPM(example_dir, run = TRUE, engine = "rtmb")

# Use existing ADMB output if available
admb_path <- file.path(example_dir, "spm_detail.csv")
if (!file.exists(admb_path)) {
  stop("Missing ADMB output at ", admb_path)
}

admb <- read_csv(admb_path, show_col_types = FALSE)

cat("admb rows:", nrow(admb), "rtmb rows:", nrow(rtmb), "\n")

common <- intersect(names(admb), names(rtmb))
key <- c("Alt", "Sim", "Year")
if (!all(key %in% common)) {
  stop("Key columns missing; cannot merge for diff.")
}

m <- merge(admb, rtmb, by = key, suffixes = c(".admb", ".rtmb"))
numcols <- common[sapply(admb[common], is.numeric) & sapply(rtmb[common], is.numeric)]
numcols <- setdiff(numcols, key)

if (length(numcols) == 0) {
  stop("No numeric columns to compare.")
}

stats <- t(sapply(numcols, function(n) {
  x <- m[[paste0(n, ".admb")]]
  y <- m[[paste0(n, ".rtmb")]]
  c(mean_diff = mean(y - x, na.rm = TRUE),
    rmse = sqrt(mean((y - x)^2, na.rm = TRUE)))
}))

print(stats)
