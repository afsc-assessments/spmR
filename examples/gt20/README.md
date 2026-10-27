# Greenland turbot 2020 split-sex projection

This is a source-checkout example for the 2020 Bering Sea and Aleutian Islands
Greenland turbot projection. It converts the archived two-sex species input to
spmR input format 2, validates it, and runs all seven Tier 3 alternatives.

The directory intentionally retains only:

- `data/Model_Proj.dat`: the archived assessment input;
- `run_spmR_split_sex.R`: the documented converter and runner; and
- this README.

Current inputs, the compiled engine, results, and provenance are generated under
the ignored `spmR_run/` directory.

## Run the example

Install spmR 0.4.3 or newer and either provide a compatible format-2 executable
or install ADMB 13 or newer. From the repository root:

```r
library(spmR)
source("examples/gt20/run_spmR_split_sex.R")

# Use an existing executable.
gt20 <- run_gt20_projection(executable = "path/to/spm")

# Alternatively, compile automatically when ADMB and a matching compiler exist:
# gt20 <- run_gt20_projection()

gt20$summary[gt20$summary$Alt == 1 & gt20$summary$Year <= 2022, ]
gt20$provenance$executable_sha256
```

Environment-variable equivalents are `SPMR_EXECUTABLE` for an existing engine,
`SPMR_ADMB` for ADMB, `SPMR_CXX` for an optional matching C++ compiler, and
`SPMR_GT20_RUN_DIR` for the output directory. `SPMR_CXX` is useful on Windows
when the first compiler on `PATH` is newer than the libraries shipped with
ADMB; for example, set it to the full path of a compatible `g++.exe`.
A concrete Windows compiler setup is:

```r
Sys.setenv(
  SPMR_ADMB = "C:/path/to/admb.cmd",
  SPMR_CXX = "C:/path/to/compatible/g++.exe"
)
gt20 <- run_gt20_projection()
```

To create and validate the format-2 inputs without a native engine, use:

```r
gt20 <- run_gt20_projection(run = FALSE)
gt20$initial_biomass
```

The helper embeds the validated recruitment, units, ages and population-weight
declarations as comments in the generated species file. With the current native
engine, a standalone run uses that species file and `spm.dat`. Separate JSON
metadata and a native sidecar are optional compatibility paths; the R runner
generates a sidecar when executing an older format-2 binary.

Running the file directly is equivalent to the default full run:

```sh
Rscript examples/gt20/run_spmR_split_sex.R
```

## Conversion decisions

- Tier is stored as `3`; projected stock status selects the Tier 3a rule.
- Ages are `1:30`, with age 30 as the plus group.
- Recruitment is total across sexes and is allocated equally to females and
  males during projection.
- Abundance, weight, and biomass units are `thousand_fish`, `kg`, and `t`;
  consequently, `N_scalar = 1`.
- The two trailing SSB values that the legacy executable did not pair with the
  41 recruitment observations are removed.
- The first two zero female spawning weights are replaced by machine epsilon,
  the minimum value allowed by input format 2.
- The archived female spawning weights are reused as female population weights,
  preserving the legacy biomass convention.
- No independently estimated male spawning or population-weight vector is in
  the archive. The unweighted age-specific mean of the two male fishery-weight
  vectors is therefore declared and recorded as `mean_male_fishery`.

These choices reproduce the legacy beginning-of-2020 biomass of `92,104.6 t`.
They are retained for reproducibility and should not be described as newly
measured sex-specific population weights.

The tested run produces 14,000 detail rows: seven alternatives, 100 simulations,
and 20 years (2020–2039). For alternative 1, mean 2021 SSB is `51,913.9 t` and
ABC is `7,326.4 t`, matching the published assessment after rounding.

## Sources

- [spmR split-sex input migration guide](https://afsc-assessments.github.io/spmR/articles/split_sex_inputs.html)
- [2020 BSAI Greenland turbot assessment](https://apps-afsc.fisheries.noaa.gov/refm/docs/2020/BSAIturbot.pdf)
