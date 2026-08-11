# Changelog

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
