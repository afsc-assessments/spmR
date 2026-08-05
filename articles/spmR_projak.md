# projak vs spmR

## 1 Purpose

- `projak` (Ben Williams, v0.0.0.9000): pure-R package for NPFMC
  projection scenarios (1-7) downstream of an RTMB assessment; accepts a
  report object as input.
- `spmR` (Jim Ianelli, v0.2.0): Standard Projection Model wrapper around
  the legacy ADMB `spm` executable with an experimental RTMB path; reads
  `.dat` inputs (ADMB-style).

## 2 Architecture

| Component | `projak` | `spmR` |
|----|----|----|
| Input | RTMB report object | `spm.dat` plus species files |
| Compiled code | None (pure R) | ADMB `spm.tpl` plus partial RTMB stub |
| Multi-species support | Single-species | Multi-species natively (`nspp`) |
| Scenario coverage | 1-7 fully implemented | Alternatives 1-5 (RTMB path is a stub) |
| Maturity | Early development (0.0.0.9000) | More complete (tests, vignettes, pkgdown) |

## 3 Recruitment

Both projects implement inverse-Gaussian recruitment, but with different
parameterization paths:

- `projak`: takes arithmetic and harmonic means directly from
  `report$recruits`.
- `spmR` RTMB path: derives CV from the harmonic-to-arithmetic ratio,
  then samples recruitment.

## 4 Tier and Harvest Control Rule Logic

- `projak` includes a full `get_tier_f()` implementation with Amendment
  56 ramp logic, SSL protection, and scenario-specific F rules.
- `spmR` RTMB path (`runSPM_rtmb`) currently stubs the scenario loop;
  SSB, catch, and F are placeholder means rather than forward
  age-structured projections.

## 5 Key Gap

[`runSPM_rtmb()`](http://afsc-assessments.github.io/spmR/reference/runSPM_rtmb.md)
in `spmR` is currently a scaffold: recruitment draws are implemented,
but age-structured population dynamics are not. In contrast,
`projak::project_step()` performs full age-structured projection
(survival, catch-at-age, plus-group dynamics, and SSB updates).

## 6 Dependencies

- `projak`: `data.table`, `tidytable`, `magrittr`
- `spmR`: `dplyr`, `ggplot2`, `ggthemes`, `patchwork`, `readr`,
  `stringr`, `tibble`, `tidyr`

## 7 Summary

`projak` is effectively a focused, RTMB-native replacement for the
projection component, with complete harvest control rule logic and
lighter dependencies. `spmR` remains broader infrastructure (data I/O,
multi-species SPM workflow, ADMB wrapper, and visualization), but its
RTMB path still needs substantial development to match `projak`
projection fidelity.
