# Greenland turbot 2026 projection

`run.R` audits the supplied projection files and preserves them when preparing
a separate attempt. The revised `ForProjections.dat` has 47 recruitment values
and 45 SSB values. Staging and execution stop until matching source histories
are supplied; the helper never pads or discards values to make them agree.
Population body weights and several SS3 export definitions also remain open.

## Simple commands

From the repository root in R:

```r
source("examples/gt_2026/run.R")
check_gt2026_inputs()
```

Equivalent terminal audit:

```sh
Rscript examples/gt_2026/run.R check
```

The audit reports the history mismatch without attempting a projection. Supply
the matching SSB history and confirm its calendar alignment before using the
commands below. `diagnostic = TRUE` explicitly selects the working population
weights, units and ages; it does not resolve missing history values.

After those inputs are complete, stage a **conditional diagnostic** without
compiling or projecting:

```r
staged <- run_gt2026_projection(diagnostic = TRUE, run = FALSE)
staged$run_dir
```

After reviewing the staged assumptions, execute it with:

```r
gt2026 <- run_gt2026_projection(
  diagnostic = TRUE
)
gt2026$summary
gt2026$run_dir
```

Install ADMB 13 or later and put `admb` on `PATH`, supply its path with the
`admb` argument, or set `SPMR_ADMB`; `SPMR_CXX` selects a matching C++ compiler.
An existing format-2 binary can be passed as `executable` or through
`SPMR_EXECUTABLE`. Compilation uses the installed spmR template; reinstall this
checkout if the installed template still refers to `spmr_input_v2.hpp`.

The corresponding terminal execution command is:

```sh
Rscript examples/gt_2026/run.R diagnostic
```

Successful staging creates a new `spmR_runs/attempt-###/` directory with source
snapshots, SHA-256 hashes, staging decisions, and versioned declarations embedded
as comments in the staged species file. With the current native engine, the
projection inputs are `spm.dat` and that species file. Separate JSON metadata
and native sidecars are optional compatibility paths. After execution, the
attempt also contains logs, provenance, detail output and annual means.
Earlier attempts and supplied inputs retain their contents. The configured
complete run would have 105,000 rows: 7 alternatives × 1,000 simulations ×
15 years. The current incomplete source file has no completed projection.
Existing source metadata is archived for review; staged declarations come
from the runner arguments or explicitly selected diagnostic defaults.

After supplying matching histories, use actual population body weights by
providing two numeric vectors in age order and explicit units and ages:

```r
gt2026 <- run_gt2026_projection(
  population_weights = list(female = female_body_weights, male = male_body_weights),
  units = list(abundance = "thousand_fish", weight = "kg", biomass = "t"),
  ages = 1:30,
  admb = Sys.getenv("SPMR_ADMB")
)
```

Replace the vectors, units and age labels with verified SS3 export values. This
does not resolve the remaining history/export review by itself. Calling the
runner without declarations or `diagnostic = TRUE` stops with the missing inputs.

## Input evaluation

| Item | Finding and handling |
|---|---|
| Embedded declarations | The runner uses `write_spm_metadata()` to embed validated declarations in the staged species file. Separate `spm_metadata.json` and `spm_input_v2.dat` files are optional. The obsolete placeholder sidecar was removed from this example; earlier source snapshots remain archived. |
| Recruitment | `No Recruitments` is 47. Two identical 47-value series are wrapped across four lines; each ends in `811.8, 460.5`. The helper reads complete numeric series across line breaks and stages one copy. |
| Recruitment basis | The new `per sex` line explicitly declares the basis. The helper moves that declaration into the embedded metadata so the positional species fields retain their expected order. The engine doubles the complete per-sex history once to total recruitment. |
| SSB history | Only 45 values are supplied, ending in `28053.3, 27020.6, 25862.1`. Supply the corresponding source values and year alignment for the declared 47 recruitment positions. The helper stops before creating an attempt. |
| History labels | The recruitment label still says 1978–2022, which spans 45 years; the SSB label says 1978–2026, which spans 49. Neither establishes the calendar years for the revised numerical histories. Confirm both against the SS3 export. |
| Spawning weights | Both sex-specific vectors already exist, with two zeros each. Once histories are complete, stage those four zeros as `1e-12` to satisfy the positive-weight validator; retain all other values. |
| Maturity | The working interpretation is that spawning weights contain body weight × maturity; confirm this against the SS3 export. The all-one maturity vectors are neutral multipliers for SSB. Consequently, `Ntot` and `SexRatio` describe all fish and the all-fish female fraction here. |
| Population weights | Actual female and male population body weights are absent. Spawning and fleet catch weights have different meanings. Diagnostic mode uses `0.96 × fleet 1 + 0.04 × fleet 2` catch-weight proxies separately by sex; `Fratio` is an F allocation, so this mixture is a computational assumption. |
| Units and ages | Diagnostic mode declares thousand fish, kg, tonnes, and ages 1–30 with a terminal plus group. Verify these against the SS3 export. |
| Initial state and average F | The revision supplies new 30-age female and male abundance vectors and average five-year F `0.05605452`. Preserve those values. The abundance label and control start year both say 2026; confirm that the SS3 export represents beginning-of-2026 abundance. |
| Recruitment model | Preserve Tier 3 and `Rec_Gen = 1`: historical recruitment simulation. The positional input contract still requires matching recruitment and SSB lengths. The runner rejects fitted recruitment modes, which require paired observed histories. Gradient and Hessian fit diagnostics are inapplicable to historical-recruitment simulation. |
| Catch controls | Fifteen years (2026–2040), seven alternatives, and 1,000 simulations. Fixed catches are 193.28, 378 and 378 for 2026–2028. All three catches apply to alternatives 1–5; alternatives 6–7 share the first-year catch and then follow their status rules. |
| Control comments | The two zeros labelled `srr_conditioning` and `srr_reserved` mean `Fmsy_F35 = 0` and `Rec_Cond = 0` positionally. Values are retained. |
| TAC parameters | `tacpar.dat` is unused with `TAC_ABC = 1`; keep it in the source archive. |

The input findings above refer to the revised files in this directory.
`check_gt2026_inputs()` reads the current source directory, reports its control
settings and history lengths, and flags unsupported control modes for review.
The runner requires the neighboring `examples/gt20/run_spmR_split_sex.R` helper
script; retain both example directories when copying them out of the repository.

For assessment use, obtain the matching source histories and SS3 export code
and fields for population body weights, units, age labels/plus group, and
recruitment/SSB year pairing. A later successful execution would verify the
declared conditional calculation; those definitions and catch recommendations
require separate scientific review.
