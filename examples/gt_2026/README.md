# Greenland turbot 2026 projection

The four supplied `.dat` files are preserved as received. `run.R` audits them,
stages a corrected copy, and runs the self-contained format-2 ADMB engine.
Population body weights and several SS3 export definitions remain unresolved.

## Simple commands

From the repository root in R:

```r
source("examples/gt_2026/run.R")
check_gt2026_inputs()
staged <- run_gt2026_projection(diagnostic = TRUE, run = FALSE)
```

For a **conditional diagnostic** using the explicitly declared assumptions below:

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

Equivalent terminal commands:

```sh
Rscript examples/gt_2026/run.R check
Rscript examples/gt_2026/run.R diagnostic
```

Use `run = FALSE` to stage and validate inputs without compiling or projecting.
Every call creates a new `spmR_runs/attempt-###/` directory. It contains source
snapshots, SHA-256 hashes and staging decisions, validated JSON metadata, and,
after execution, the native sidecar, logs, provenance, detail output and annual
means. Earlier attempts and supplied inputs retain their contents. The expected
complete run has 105,000 rows: 7 alternatives × 1,000 simulations × 15 years.
Existing source metadata is archived for review; staged metadata always comes
from the runner arguments or explicitly selected diagnostic defaults.

To use actual population body weights, provide two numeric vectors in age order
and explicit units and ages:

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
| Native sidecar | `spm_input_v2.dat` contains `SET_...` placeholders instead of numeric recruitment/unit codes and 30 female and 30 male population weights. Preserve it in the source snapshot; generate a new sidecar through `runSPM()`. |
| R metadata | `spm_metadata.json` is missing. The runner creates it from explicit declarations and validates it. |
| Recruitment | Two identical 45-value rows are present. SPM expects one recruitment row followed by SSB, so the second recruitment row would occupy the SSB slot. Retain one row and declare `per_sex`; the engine doubles it once to total recruitment. |
| Spawning weights | Both sex-specific vectors already exist, with two zeros each. Stage the four zeros as `1e-12` to satisfy the current positive-weight validator; retain all other values. The initial female-SSB change is about `1.19e-9` in the declared biomass unit. |
| Maturity | The working interpretation is that spawning weights contain body weight × maturity; confirm this against the SS3 export. The all-one maturity vectors are neutral multipliers for SSB. Consequently, `Ntot` and `SexRatio` describe all fish and the all-fish female fraction here. |
| Population weights | Actual female and male population body weights are absent. Spawning and fleet catch weights have different meanings. Diagnostic mode uses `0.96 × fleet 1 + 0.04 × fleet 2` catch-weight proxies separately by sex; `Fratio` is an F allocation, so this mixture is a computational assumption. |
| Units and ages | Diagnostic mode declares thousand fish, kg, tonnes, and ages 1–30 with a terminal plus group. Verify these against the SS3 export. |
| Initial year | The supplied abundance label and control start year both say 2026. The runner treats abundance as the beginning-of-2026 state; confirm the SS3 export timing. |
| SSB history | There are 45 values, ending in three identical values of `28053.3`. The working interpretation treats the final two values as auxiliary repeats; their provenance still requires review. “1978–2026” implies 49 years and mislabels this vector. The staged comment describes input positions without assigning calendar years. |
| Recruitment model | Preserve Tier 3 and `Rec_Gen = 1`: historical recruitment simulation. The auxiliary SSB values do not drive this path. The runner rejects fitted recruitment modes, which require genuinely paired observed histories. Gradient and Hessian fit diagnostics are inapplicable here. |
| Catch controls | Fifteen years (2026–2040), seven alternatives, and 1,000 simulations. Fixed catches are 193.28, 378 and 378 for 2026–2028. All three catches apply to alternatives 1–5; alternatives 6–7 share the first-year catch and then follow their status rules. |
| Control comments | The two zeros labelled `srr_conditioning` and `srr_reserved` mean `Fmsy_F35 = 0` and `Rec_Cond = 0` positionally. Values are retained. |
| TAC parameters | `tacpar.dat` is unused with `TAC_ABC = 1`; keep it in the source archive. |

The input findings above refer to the four supplied files in this directory.
`check_gt2026_inputs()` reads the current source directory, reports its control
settings and history lengths, and flags unsupported control modes for review.
The runner requires the neighboring `examples/gt20/run_spmR_split_sex.R` helper
script; retain both example directories when copying them out of the repository.

For assessment use, obtain the SS3 export code and fields for population body
weights, units, age labels/plus group, and the true recruitment/SSB year pairing.
Successful execution verifies the conditional calculation, not those definitions
or an adopted catch recommendation.
