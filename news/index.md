# Changelog

## spmR (development version)

- Multi-stock native status-determination alternatives retain each
  stock’s own catch instead of assigning the last stock’s catch to every
  stock.

- Multi-stock Ricker projections keep each stock’s productivity
  coefficient separate, so changing stock order preserves the
  projections.

- Multi-stock zero-catch initialization indexes the catch year and stock
  separately, preventing an out-of-bounds matrix write.

- The GT2026 adapter rejects spawning months other than January before
  staging because its initial spawning-biomass verification assumes
  beginning-of-year spawning.

- [`tier3_scenario_table()`](https://afsc-assessments.github.io/spmR/reference/tier3_scenario_table.md)
  summarizes each stock separately for multi-stock inputs and retains
  the existing column layout for single-stock results.

- The GT2026 report defaults to an audit-only render, so the plain
  Quarto command works in a fresh source checkout without ignored local
  run directories. Completed results are selected explicitly with
  `saved_run`.

- The GT2026 example preserves the author’s confirmed 47-value SSB
  history and provides a portable fully worked example with the actual
  inputs, editable R commands, verified saved results, two Alternative 2
  figures and CSV tables. Its conditional results are published as a
  reference with the remaining scientific assumptions stated explicitly.

- The standalone GT2026 report reads saved controls through the
  validated positional parser, so supported `spm.dat` comment labels no
  longer cause completed runs to be rejected.

## spmR 0.4.3

- [`write_spm_metadata()`](https://afsc-assessments.github.io/spmR/reference/write_spm_metadata.md)
  embeds validated version-2 declarations as comments in each species
  file by default.
  [`validate_spm_inputs()`](https://afsc-assessments.github.io/spmR/reference/write_spm_metadata.md)
  and
  [`runSPM()`](https://afsc-assessments.github.io/spmR/reference/runSPM.md)
  read those declarations automatically; an explicit
  `metadata = "name.json"` retains the separate JSON workflow.
- The self-contained native engine reads the embedded declarations
  directly and advertises `SPMR_INLINE_INPUT=1`. A standalone run needs
  `spm.dat` and its species files. Legacy native sidecars remain
  readable, and the R runner generates a sidecar for older format-2
  executables.
- The GT2026 adapter accepts wrapped recruitment histories and an
  explicit `per sex` declaration. At the 0.4.3 release, the supplied
  example had 47 recruitment values and 45 SSB values; the adapter
  rejected that mismatch without padding or discarding values. Later
  source revisions are evaluated by the live input audit.

## spmR 0.4.2

- The ADMB projection engine now compiles from `inst/admb/spm.tpl`
  alone; the format-2 input checks are included in the template, and a
  separate C++ header is no longer required.
- Package vignettes and website articles use Quarto, with a canonical
  GT2026 audit/staging example and a separate guide to its input
  definitions and conditional diagnostic runs.
- `run_gt2026_projection()` in `examples/gt_2026/run.R` stages the
  supplied Greenland turbot inputs, records input repairs and metadata,
  and separates explicitly requested diagnostic weight proxies from
  supplied population weights. `check_gt2026_inputs()` lists the
  remaining source-data issues.

## spmR 0.4.1

- Split-sex species files now contain a male spawning weight-at-age
  vector, `wt_M`, immediately after `wt_F`. The R validator and ADMB
  engine both read and validate the new field. Pooled-sex files continue
  to use `wt_F` for both internal sex groups.
- Version-2 projections no longer read or require `tacpar.dat`. They
  continue to require `TAC_ABC = 1`; legacy fitted and external TAC
  modes remain rejected.

## spmR 0.4.0

- New projections require input metadata format 2 and a compatible
  executable. Legacy output remains readable with `run = FALSE`.
- Recruitment basis is explicit. The complete history is converted to
  total recruitment before calculating arithmetic and harmonic means,
  variability, simulations, and stock–recruitment inputs. Each projected
  total is allocated equally to the two sexes once.
- Female and male population weights are separate from spawning and
  fishery weights. Missing male population weights require an explicit,
  recorded substitute for split-sex inputs. Metadata validation checks
  age order, dimensions, finite values, and compatible units.
- Total biomass uses population weights throughout projections and
  biomass reference calculations. Male reference calculations use male
  selectivity.
- Stock–recruitment fits use bounds tied to the normalized recruitment
  history and mean-corrected lognormal draws. Unit-scaled fixtures check
  the fitted relationships. New fitted runs require a maximum gradient
  below 1e-4 and a positive-definite Hessian.
- New runs validate inputs before execution, check the executable’s
  format support, use a clean execution directory, and write SHA-256
  provenance. Existing results survive a failed run.
- Format 2 supports direct ABC projections (`TAC_ABC = 1`), recruitment
  modes 1 and 2, abundance scaling `N_scalar = 1`, and weight in kg.
  Auxiliary recruitment modes 3 and 4 and the experimental RTMB
  projection runner require further implementation before use with this
  format.

## spmR 0.3.0

- [`as_spm_result()`](https://afsc-assessments.github.io/spmR/reference/as_spm_result.md)
  provides a validated common result format for projection model
  backends while preserving model-specific columns.
- [`runSPM()`](https://afsc-assessments.github.io/spmR/reference/runSPM.md)
  now dispatches through internal model adapters and returns an
  `spm_result` while remaining compatible with legacy ADMB and
  experimental RTMB calls.
- [`tier3_scenario_table()`](https://afsc-assessments.github.io/spmR/reference/tier3_scenario_table.md)
  summarizes simulation output into assessment-ready rows for the seven
  Tier 3 projection alternatives.

## spmR 0.2.1

- The experimental RTMB path can reuse existing output when rendering
  its comparison vignette. This path remains a simplified prototype and
  is not yet a full RTMB translation or numerically equivalent
  replacement for the ADMB model.
