# spmR (development version)

* `as_spm_result()` provides a validated common result format for projection model backends while preserving model-specific columns.
* `runSPM()` now dispatches through internal model adapters and returns an `spm_result` while remaining compatible with legacy ADMB and experimental RTMB calls.

# spmR 0.2.1

* The experimental RTMB path can reuse existing output when rendering its comparison vignette. This path remains a simplified prototype and is not yet a full RTMB translation or numerically equivalent replacement for the ADMB model.
