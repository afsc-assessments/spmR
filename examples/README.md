# Projection examples

The [canonical examples article](https://afsc-assessments.github.io/spmR/articles/spm_example.html)
introduces the current workflows and distinguishes saved-output exploration
from a new projection.

| Example | Purpose | Status |
|---|---|---|
| [`atka`](atka) | Read archived detail output and plot projections. | Saved historical output; `runSPM(run = FALSE)`. |
| [`BSRE_AI`](BSRE_AI) | Read and plot archived summary output. | Saved historical output. |
| [`gt20`](gt20) | Convert the 2020 Greenland turbot input to format 2 and reproduce its documented benchmark. | Runnable with explicit recorded legacy weight substitutions. |
| [`gt_2026`](gt_2026) | Audit the revised 2026 inputs and identify missing declarations. | Staging is blocked by 47 recruitment values versus 45 SSB values; population weights, units, ages and history alignment also require review. |

Start the GT2026 workflow from the repository root:

```r
source("examples/gt_2026/run.R")
check_gt2026_inputs()
```

The [GT2026 guide](https://afsc-assessments.github.io/spmR/articles/gt2026_inputs.html)
and its [README](gt_2026/README.md) explain the missing definitions and native
run commands. Supply matching source histories before staging. Once complete,
the helper preserves the source inputs and gives every attempt a separate
ignored output directory. Fishery-weight proxies require an explicit diagnostic
choice; they do not supply the missing SSB values.

Other directories preserve historical models, inputs and saved outputs. Their
Makefiles may refer to older layouts or toolchains, and old inputs need format-2
declarations before a new run. `write_spm_metadata()` embeds these declarations
as comments in each species file, so a standalone current-engine run uses
`spm.dat` and its species files. Build the current self-contained
[`inst/admb/spm.tpl`](../inst/admb/spm.tpl) for current projections. Historical
Windows executables and editor recovery files are excluded from source control.
