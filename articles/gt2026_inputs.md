# Audit and stage the 2026 Greenland turbot inputs

The `examples/gt_2026` directory preserves the supplied Greenland turbot
projection files. The revised species input contains 47 recruitment
values and 45 SSB values. The helper audits this mismatch and stops
staging before creating an attempt directory. Matching source histories,
population body weights, units, age and plus-group alignment, and SS3
export-field mapping require confirmation before assessment use. This
article executes the audit and demonstrates the staging block; it
generates no projection results.

## Simple commands

Install the current spmR package and work from a clone of the [source
repository](https://github.com/afsc-assessments/spmR). From its root:

``` r

source("examples/gt_2026/run.R")
check_gt2026_inputs()
```

The audit reads the current source values. Supply matching recruitment
and SSB histories before staging. `diagnostic = TRUE` selects working
population weights, units and ages explicitly; it cannot supply missing
history values. Once inputs are complete, each successful default
staging call creates `examples/gt_2026/spmR_runs/attempt-###`, retaining
source snapshots, SHA-256 hashes, embedded declarations, and staging
decisions. Existing attempt directories are protected against
overwriting.

## Audit the supplied files

The following chunks locate and audit the example during an article
build. They require the source checkout or source package, where the
example files are available.

``` r

audit <- check_gt2026_inputs(example_dir)
gt::gt(audit)
```

| item | status | detail |
|----|----|----|
| Native metadata | info | A separate spm_input_v2.dat is optional; the runner embeds validated declarations in the staged species file. |
| R metadata | info | A separate spm_metadata.json is optional. Existing files are archived; staged declarations come from explicit arguments or diagnostic defaults. |
| Recruitment rows | repair | 2 copies of a 47 value series across 4 lines; retain one complete series. A second copy would be read as SSB. |
| Recruitment basis | ok | Source explicitly declares per_sex recruitment. |
| Spawning weights | repair | 4 zeros in effective spawning weights; stage them as 1e-12 for the current positive-weight contract. |
| Population weights | missing | Population body weights require explicit runner arguments and SS3 verification. Diagnostic defaults use Fratio-weighted catch-weight proxies, with fleet fractions 0.96, 0.04 |
| Units and ages | review | Thousand fish, kg, tonnes and ages 1:30/30+ are working assumptions requiring SS3 export verification. |
| SSB history | missing | 47 recruitment values but 45 SSB values. Supply matching source histories before staging; values are never padded or discarded. |
| Maturity fields | info | All-one maturity retains effective spawning weights. Ntot and SexRatio describe all fish and the all-fish female fraction. Confirm the SS3 spawning-weight definition. |
| Controls | ok | 2026-2040; 7 alternatives; 1000 simulations. Adapter requires Tier 3, Rec_Gen=1, TAC_ABC=1, beginning year 2026 and ForProjections.dat. |
| TAC parameters | info | tacpar.dat is archived when supplied and unused with TAC_ABC=1. |

The input now explicitly declares `per sex`. Its two identical
recruitment series contain 47 values each, wrapped across four lines and
ending in `811.8, 460.5`. The SSB series has 45 values, ending in
`28053.3, 27020.6, 25862.1`. The recruitment label still says 1978–2022
and the SSB label says 1978–2026; confirm the numerical histories and
their calendar alignment from the source export. The revised average
five-year F is `0.05605452`, with new female and male abundance vectors
over 30 ages.

The helper accepts the wrapped recruitment series and explicit basis.
Once matching source histories are available, it can stage one
recruitment copy, move the basis declaration into the versioned comment
block, and replace four zero spawning weights with `1e-12` for the
positive-weight contract. It keeps Tier 3 historical-recruitment mode
(`Rec_Gen = 1`). That mode still requires matching positional
recruitment and SSB lengths.

The obsolete placeholder sidecar was removed from the canonical example;
earlier source snapshots remain archived. Separate JSON and native
metadata files are optional in version 0.4.3: the helper writes
validated declarations as comments in the staged species file. The
current native engine reads those comments directly. `spm.tpl` is
self-contained and requires no separate C++ header.

## Confirm the staging block

The current mismatch is reported as an expected input error, and no
attempt directory is created. An unexpected error still stops the
article build.

``` r

attempt_dir <- tempfile("spmr-gt2026-article-")
staging_status <- tryCatch(
  run_gt2026_projection(
    source_dir = example_dir,
    run_dir = attempt_dir,
    run = FALSE
  ),
  error = function(condition) {
    detail <- conditionMessage(condition)
    if (!grepl("47 recruitment values but 45 SSB values", detail, fixed = TRUE)) {
      stop(condition)
    }
    detail
  }
)
stopifnot(is.character(staging_status), !dir.exists(attempt_dir))
cat("Staging blocked:", staging_status)
```

    Staging blocked: 47 recruitment values but 45 SSB values. Supply matching source histories before staging; values are never padded or discarded.

## Stage complete inputs after review

Diagnostic mode uses `0.96 × fleet 1 + 0.04 × fleet 2` catch-weight
proxies separately for each sex as population weights. These
coefficients are the supplied fishing-mortality allocation. The mixture
is a computational assumption and requires review as a representation of
population body weight. Diagnostic mode also declares thousand fish, kg,
tonnes, and ages 1–30 with a terminal plus group. Whether the supplied
spawning weights already incorporate maturity requires confirmation; the
all-one maturity fields are neutral multipliers. Under those fields,
`Ntot` and `SexRatio` describe all fish and the all-fish female
fraction.

``` r

staged <- run_gt2026_projection(
  source_dir = example_dir,
  run_dir = tempfile("spmr-gt2026-article-"),
  diagnostic = TRUE,
  run = FALSE
)
staged$run_dir
```

This command is displayed without execution. It requires matching source
histories and an explicit decision to use the diagnostic assumptions.

## Run an explicitly chosen diagnostic projection

After supplying matching histories and reviewing the assumptions, these
commands enable native execution. They are displayed without execution
in this article.

``` r

gt2026 <- run_gt2026_projection(diagnostic = TRUE)
gt2026$summary
gt2026$run_dir
```

The helper uses `admb` on `PATH`, `SPMR_ADMB`, or its `admb` argument to
compile the installed template. `SPMR_CXX` selects a matching C++
compiler. A compatible existing executable can be selected with
`SPMR_EXECUTABLE` or `executable`. If compilation still requests
`spmr_input_v2.hpp`, reinstall spmR from the current checkout and
restart R to refresh the installed template.

A completed run under the supplied controls would contain 105,000 detail
rows: seven alternatives, 1,000 simulations, and 15 projection years
(2026–2040). The helper verifies the row keys, finite outputs, initial
biomass, fixed catches, and successful provenance. Gradient and Hessian
fit diagnostics are inapplicable to historical-recruitment simulation.
These checks establish the conditional calculation; the SS3 export
definitions and scientific interpretation require their own review.

## Replace the population-weight assumptions

Provide actual female and male population body-weight vectors with
verified units and age labels:

``` r

gt2026 <- run_gt2026_projection(
  population_weights = list(
    female = female_body_weights,
    male = male_body_weights
  ),
  units = verified_units,
  ages = verified_ages
)
```

Each vector must align with the 30 supplied age positions. Confirm the
beginning-of-2026 abundance state, plus group, and recruitment/SSB year
pairing against the SS3 export code and source fields. Supplying
population weights alone leaves those remaining checks open. The example
README contains the complete input evaluation and terminal commands.
