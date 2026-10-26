# Greenland turbot 2020 split-sex projection

`run_spmR_split_sex.R` converts the archived GT20 inputs to the input-format-2
contract used by spmR 0.4.1, validates them, and runs the ADMB projection when a
compatible `spm` executable is available.

Run from the repository root with:

```r
library(spmR)
source("examples/gt20/run_spmR_split_sex.R")
```

Derived inputs and results are written to `spmR_run/`, which is intentionally
ignored by Git. Set `SPMR_GT20_RUN_DIR` to use another output directory.

## Conversion decisions

- `setup.dat` and `spp_catch.dat` are consolidated into the current `spm.dat`
  layout. The legacy `tacpar.dat` is not used by format 2.
- Tier is `3`. The 2020 assessment classified BSAI Greenland turbot as Tier 3a;
  the SPM input stores the numeric tier, while the harvest-control rule selects
  the `a` condition from stock status.
- Ages are `1:30`, and age 30 is the plus group. This matches both the assessment
  description and the legacy model's plus-group update.
- Recruitment is declared as `total`. The legacy executable calculated the mean
  of the stored series and allocated projected recruitment equally to females
  and males.
- The abundance, weight, and biomass units are `thousand_fish`, `kg`, and `t`.
  `N_scalar` is therefore `1`, as required by input format 2.
- The legacy file stores 41 recruitment values but 43 SSB values. Only the first
  41 SSB values were read by the legacy executable; the converter explicitly
  removes the trailing 2019 and 2020 values before validation.
- The first two female spawning weights are zero. Format 2 requires values at
  least machine epsilon, so they are replaced by that negligible positive value.
- For parity with `main.tpl`, female spawning weights are also used as female
  population weights. Male population weights are the unweighted mean of the two
  male fishery-weight vectors. The metadata records the latter explicitly as
  `male_population_substitute = "mean_male_fishery"`.
- GT20 has no separate male spawning-weight vector. The same mean male fishery
  weights provide the required positive `wt_M` vector; spawning biomass remains
  female-only.

These choices reproduce the archived beginning-of-2020 total biomass of
`92,104.6 t`. They preserve the legacy example; they should not be represented
as newly measured population weights.

## Build the format-2 executable

ADMB 13 or newer must compile the template installed with spmR. First copy the
installed template and its format-2 header into the run directory from R:

```r
run_dir <- file.path("examples", "gt20", "spmR_run")
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
admb_dir <- system.file("admb", package = "spmR", mustWork = TRUE)
file.copy(
  file.path(admb_dir, c("spm.tpl", "spmr_input_v2.hpp")),
  run_dir,
  overwrite = TRUE
)
```

Then use a shell where the ADMB compiler is on `PATH`:

```powershell
Push-Location 'examples\gt20\spmR_run'
try {
  admb -f spm.tpl
  & .\spm.exe -spmr-capabilities
} finally {
  Pop-Location
}
```

The capability check must print `SPMR_INPUT_FORMAT=2`. Rerun the R script after
building; it will validate the inputs and call `runSPM(..., engine = "admb")`.

The tested run produces 14,000 detail rows: seven alternatives, 100 simulations,
and 20 years (2020-2039). For alternative 1, the mean 2021 SSB is `51,913.9 t`
and ABC is `7,326.4 t`, reproducing the published projection after rounding.

## Sources

- [spmR split-sex input migration guide](https://afsc-assessments.github.io/spmR/articles/split_sex_inputs.html)
- [2020 BSAI Greenland turbot assessment](https://apps-afsc.fisheries.noaa.gov/refm/docs/2020/BSAIturbot.pdf)
