# Greenland turbot 2026: fully worked example

Download the [fully worked example ZIP](https://afsc-assessments.github.io/spmR/gt2026-handoff.zip).
It contains the actual inputs, a field guide, installation requirements, a short
R script, one verified saved run, CSV exports and two figures. Open the
[rendered results](https://afsc-assessments.github.io/spmR/greenland_turbot.html)
for a quick reference. Jim confirmed this local 47-value SSB revision is the
intended example; the scientific qualifications below still apply.

`run.R` audits the supplied projection files and preserves them when preparing
a separate attempt. The revised `ForProjections.dat` has 47 recruitment values
and 47 SSB values. The counts now match. The last two SSB values repeat the
preceding pair, and the history labels still require calendar verification.
The helper never pads or discards values to make the histories agree.
Population body weights and several SS3 export definitions also remain open;
matching counts alone do not establish assessment-ready inputs.

## Simple commands

### Standalone Quarto report

`greenland_turbot.qmd` is the editable run-and-results report. From the repository
root, render the current input audit without compiling or running ADMB:

```sh
quarto render examples/gt_2026/greenland_turbot.qmd
```

The default `saved_run: ""` works without any local attempt directory. It
shows the audit only; projection figures require an explicit
completed attempt.

Open the resulting `examples/gt_2026/greenland_turbot.html`. The report has visible
manual run controls, input checks, provenance, and two Alternative 2 figures:
spawning biomass and catch. The spawning-biomass figure uses a logarithmic
y-axis and dashed B40 and B35 reference lines. Both figures show the
all-simulation median and a salmon 90% projection interval. Without an
explicitly selected saved attempt, rendering shows the current audit.

After completing and reviewing a run, select its directory explicitly:

```sh
quarto render examples/gt_2026/greenland_turbot.qmd -P saved_run:spmR_runs/attempt-001
```

Replace `attempt-001` with the actual saved attempt. Paths are relative to this
example directory, or may be absolute. `report.R` validates saved provenance and
hashes before reading output; older source revisions are clearly labelled.
At least 15 simulations are required. Rendering preserves saved attempts and
never starts a new projection. The manual run chunk is excluded from rendering.

### R and terminal controls

From the repository root in R:

```r
source("examples/gt_2026/run.R")
check_gt2026_inputs()
```

Equivalent terminal audit:

```sh
Rscript examples/gt_2026/run.R check
```

The audit reads the current histories without attempting a projection. Confirm
the repeated terminal SSB values and calendar alignment before using the
commands below. `diagnostic = TRUE` explicitly selects the working population
weights, units and ages; it does not validate source values or year pairing.

After reviewing those inputs, stage an explicitly chosen **conditional diagnostic** without
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
15 years. Matching source counts alone provide no evidence of a completed
projection; inspect a saved attempt and its diagnostics separately.
Existing source metadata is archived for review; staged declarations come
from the runner arguments or explicitly selected diagnostic defaults.

After confirming the histories, use actual population body weights by
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
| Recruitment | `No Recruitments` is 47. Two identical 47-value series occupy two lines; each ends in `811.8, 460.5`. The helper reads complete numeric series across line breaks and stages one copy. |
| Recruitment basis | The new `per sex` line explicitly declares the basis. The helper moves that declaration into the embedded metadata so the positional species fields retain their expected order. The engine doubles the complete per-sex history once to total recruitment. |
| SSB history | There are 47 values, matching the recruitment count. The tail is `28053.3, 27020.6, 25862.1, 27020.6, 25862.1`: the last pair repeats the preceding pair. Confirm these are intended source values and establish the year alignment; the runner preserves them. |
| History labels | The recruitment label still says 1978–2022, which spans 45 years; the SSB label says 1978–2026, which spans 49. Neither establishes the calendar years for the revised numerical histories. Confirm both against the SS3 export. |
| Spawning weights | Both sex-specific vectors already exist, with two zeros each. Staging changes those four zeros to `1e-12` to satisfy the positive-weight validator and retains all other values. |
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

For assessment use, verify the source histories and obtain the SS3 export code
and fields for population body weights, units, age labels/plus group, and
recruitment/SSB year pairing. A later successful execution would verify the
declared conditional calculation; those definitions and catch recommendations
require separate scientific review.
