# RTMB vs ADMB comparison (experimental)

This vignette compares the legacy ADMB workflow (`spm.tpl`) with the new
RTMB prototype. Its archived outputs illustrate the interface; they are
placeholders for population dynamics and cannot establish scientific
equivalence. Version 0.4.0 stops new RTMB projections until the required
dynamics and input contract are implemented. New ADMB runs require input
format 2 metadata.

## Read archived prototype output

``` r

# Replace with your run directory that contains spm.dat
run_dir <- c("../examples/atka", "examples/atka")
run_dir <- run_dir[file.exists(run_dir)][1]
if (is.na(run_dir)) stop("Could not locate examples/atka")
run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)

set.seed(123)

# ADMB run (requires a format-2 binary and validated spm_metadata.json)
# admb_res <- runSPM(run_dir, run = TRUE, engine = "admb")

# RTMB run
rtmb_file <- file.path(run_dir, "spm_detail_rtmb.csv")
if (file.exists(rtmb_file)) {
  rtmb_res <- utils::read.csv(rtmb_file)
} else {
  stop("Need the archived spm_detail_rtmb.csv example file.")
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
    facet_wrap(~metric, scales = "free") +
    labs(x = NULL, y = "Density", color = "Engine") +
    theme_minimal()
}
```

## Notes

- Archived RTMB output is read from `spm_detail_rtmb.csv`.
- Its differences from ADMB include missing population dynamics.
- Use the native regression suite for validated pooled-sex and split-sex
  comparisons.
