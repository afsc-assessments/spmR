# Audit and stage the 2026 Greenland turbot inputs

The `examples/gt_2026` directory preserves the supplied Greenland turbot
projection files. Its helper audits the inputs and creates a separate
attempt directory for staging and execution. Population body weights,
units, age and plus-group alignment, and the SS3 export-field mapping
require confirmation before assessment use. This article executes the
audit and input validation; it generates no projection results.

## Simple commands

Install the current spmR package and work from a clone of the [source
repository](https://github.com/afsc-assessments/spmR). From its root:

``` r

source("examples/gt_2026/run.R")
check_gt2026_inputs()
staged <- run_gt2026_projection(diagnostic = TRUE, run = FALSE)
staged$run_dir
```

`diagnostic = TRUE` explicitly accepts the documented working
assumptions. `run = FALSE` stages and validates inputs without compiling
or executing ADMB. Each default call creates
`examples/gt_2026/spmR_runs/attempt-###`, retaining source snapshots,
SHA-256 hashes, metadata, and staging decisions. Supplying a new
`run_dir` changes the destination. Existing attempt directories are
protected against overwriting.

## Audit the supplied files

The following chunks locate the example during an article build and
stage it in a temporary directory. They require the source checkout or
source package, where the example files are available.

``` r

audit <- check_gt2026_inputs(example_dir)
gt::gt(audit)
```

| item | status | detail |
|----|----|----|
| Native metadata | missing | spm_input_v2.dat must contain numeric declarations and two population-weight vectors; the runner generates it. |
| R metadata | missing | The runner archives existing spm_metadata.json and writes staged metadata from explicit arguments before calling runSPM(). |
| Recruitment rows | repair | 2 copies of a 45 value per-sex series; retain one. A second row would be read as SSB. |
| Spawning weights | repair | 4 zeros in effective spawning weights; stage them as 1e-12 for the current positive-weight contract. |
| Population weights | missing | Population body weights require explicit runner arguments and SS3 verification. Diagnostic defaults use Fratio-weighted catch-weight proxies, with fleet fractions 0.96, 0.04 |
| Units and ages | review | Thousand fish, kg, tonnes and ages 1:30/30+ are working assumptions requiring SS3 export verification. |
| SSB history | review | 45 positions; source label: \# SSB 1978-2026 . Calendar pairing remains unresolved; restrict to Tier 3, Rec_Gen=1. |
| Maturity fields | info | All-one maturity retains effective spawning weights. Ntot and SexRatio describe all fish and the all-fish female fraction. Confirm the SS3 spawning-weight definition. |
| Controls | ok | 2026-2040; 7 alternatives; 1000 simulations. Adapter requires Tier 3, Rec_Gen=1, TAC_ABC=1, beginning year 2026 and ForProjections.dat. |
| TAC parameters | info | tacpar.dat is archived when supplied and unused with TAC_ABC=1. |

The supplied native sidecar contains `SET_...` placeholders, and R
metadata is absent. The helper generates metadata from declarations and
lets
[`runSPM()`](https://afsc-assessments.github.io/spmR/reference/runSPM.md)
write the native sidecar when executing. These runtime metadata files
serve a different purpose from the compiler template: `spm.tpl` is
self-contained and requires no separate C++ header.

The staged species file retains one of the two identical recruitment
rows, declares that history as `per_sex`, and replaces four zero
spawning weights with `1e-12` to satisfy the current positive-weight
contract. The 45 SSB values include two terminal repeats. Their calendar
alignment remains unresolved; the helper restricts execution to Tier 3
historical-recruitment mode (`Rec_Gen = 1`), which does not fit a
stock–recruitment relationship.

## Validate the conditional inputs

Diagnostic mode uses `0.96 × fleet 1 + 0.04 × fleet 2` catch-weight
proxies separately for each sex as population weights. These
coefficients are the supplied fishing-mortality allocation. The mixture
is a computational assumption and requires review as a representation of
population body weight. Diagnostic mode also declares thousand fish, kg,
tonnes, and ages 1–30 with a terminal plus group. The supplied spawning
weights already incorporate maturity; the all-one maturity fields are
neutral multipliers. Under those fields, `Ntot` and `SexRatio` describe
all fish and the all-fish female fraction.

``` r

staged <- run_gt2026_projection(
  source_dir = example_dir,
  run_dir = tempfile("spmr-gt2026-article-"),
  diagnostic = TRUE,
  run = FALSE
)
```

    Conditional computational projection; SS3 export review remains open; population weights: Fratio-weighted fishery proxies.

``` r

stopifnot(
  staged$inputs$format_version == 2L,
  is.null(staged$result),
  is.null(staged$summary),
  file.exists(file.path(staged$run_dir, "staging-decisions.json"))
)
cat("Format-2 inputs validated; native projection remains unrun.")
```

    Format-2 inputs validated; native projection remains unrun.

## Run an explicitly chosen diagnostic projection

After reviewing the assumptions, the following commands enable native
execution. They are displayed without execution in this article.

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

A completed run must contain 105,000 detail rows: seven alternatives,
1,000 simulations, and 15 projection years (2026–2040). The helper
verifies the row keys, finite outputs, initial biomass, fixed catches,
and successful provenance. Gradient and Hessian fit diagnostics are
inapplicable to historical-recruitment simulation. These checks
establish the conditional calculation; the SS3 export definitions and
scientific interpretation require their own review.

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
