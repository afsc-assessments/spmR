# Run SPM with RTMB (experimental)

This is an experimental, non-ADMB path that produces spm_detail.csv-like
output for alternatives 1-5 using RTMB-compatible R code. It is not yet
a full translation of spm.tpl.

## Usage

``` r
runSPM_rtmb(dirname, run = TRUE, seed = 123)
```

## Arguments

- dirname:

  Directory containing spm.dat and species files.

- run:

  Logical. If TRUE, run the RTMB path and write spm_detail_rtmb.csv.

- seed:

  Random seed for stochastic components.

## Value

A data frame similar to spm_detail.csv.
