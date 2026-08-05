# RTMB vs ADMB comparison (experimental)

This vignette compares the legacy ADMB workflow (`spm.tpl`) with the new
RTMB path. The RTMB path is **experimental** and currently supports
alternatives 1–5 only.

## Run both engines

``` r

# Replace with your run directory that contains spm.dat
run_dir <- c("../examples/atka", "examples/atka")
run_dir <- run_dir[file.exists(run_dir)][1]
if (is.na(run_dir)) stop("Could not locate examples/atka")
run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)

set.seed(123)

# ADMB run (requires spm binary)
# admb_res <- runSPM(run_dir, run = TRUE, engine = "admb")

# RTMB run
rtmb_file <- file.path(run_dir, "spm_detail_rtmb.csv")
if (file.exists(rtmb_file)) {
  rtmb_res <- utils::read.csv(rtmb_file)
} else if (requireNamespace("spmR", quietly = TRUE)) {
  rtmb_res <- spmR::runSPM(run_dir, run = TRUE, engine = "rtmb")
} else {
  stop("Need either an existing spm_detail_rtmb.csv file or an installed spmR package.")
}
```

## Compare outputs

``` r

# If ADMB results are available, compare the distributions
# admb_res <- readr::read_csv(file.path(run_dir, "spm_detail.csv"))

if (exists("admb_res")) {
  summary(admb_res$SSB)
}
summary(rtmb_res$SSB)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  181439  181439  181439  181439  181439  181439

``` r

if (exists("admb_res")) {
  library(ggplot2)
  metrics <- c("SSB", "ABC", "OFL", "Catch", "F")
  plot_data <- rbind(
    transform(admb_res[, metrics], engine = "ADMB"),
    transform(rtmb_res[, metrics], engine = "RTMB")
  )

  plot_long <- tidyr::pivot_longer(
    plot_data,
    cols = all_of(metrics),
    names_to = "metric",
    values_to = "value"
  )

  ggplot(plot_long, aes(x = value, color = engine)) +
    geom_density() +
    facet_wrap(~ metric, scales = "free") +
    labs(x = NULL, y = "Density", color = "Engine") +
    theme_minimal()
}
```

## Notes

- RTMB output is written to `spm_detail_rtmb.csv`.
- Differences are expected due to seed and the current experimental
  scope.
