# Changelog

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

- [`as_spm_result()`](http://afsc-assessments.github.io/spmR/reference/as_spm_result.md)
  provides a validated common result format for projection model
  backends while preserving model-specific columns.
- [`runSPM()`](http://afsc-assessments.github.io/spmR/reference/runSPM.md)
  now dispatches through internal model adapters and returns an
  `spm_result` while remaining compatible with legacy ADMB and
  experimental RTMB calls.
- [`tier3_scenario_table()`](http://afsc-assessments.github.io/spmR/reference/tier3_scenario_table.md)
  summarizes simulation output into assessment-ready rows for the seven
  Tier 3 projection alternatives.

## spmR 0.2.1

- The experimental RTMB path can reuse existing output when rendering
  its comparison vignette. This path remains a simplified prototype and
  is not yet a full RTMB translation or numerically equivalent
  replacement for the ADMB model.
