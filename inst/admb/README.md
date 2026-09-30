# SPM source code

The ADMB templates and helper scripts for the Standard Projection Model live
in this installed-data directory because an R package's `src/` directory is
reserved for source code compiled during package installation.

To compile the main model with ADMB 13.0 or newer, copy this directory to a
writable location and run:

```sh
admb -f spm.tpl
```

`Makefile.admb` contains an optional cross-platform wrapper for this command.

The executable requires metadata format 2. Keep `spmr_input_v2.hpp` beside the
template while compiling. `spm -spmr-capabilities` must print
`SPMR_INPUT_FORMAT=2`. Use `write_spm_metadata()` and `runSPM(run = TRUE)` to
validate the complete input set and generate `spm_input_v2.dat`.

The native sidecar starts with `SPMR_INPUT_V2 2 <stock_count>`. For each stock,
it contains the legacy filename, number of sexes, number of ages, recruitment
basis code, abundance unit code, weight unit code, biomass unit code, and male
substitute code, followed by age, female population weight, and male population
weight vectors. It ends with `END_SPMR_INPUT_V2`. These fields stay outside the
legacy positional files. The R interface is the supported writer.

Codes: recruitment 1 = total, 2 = per sex; abundance 1 = fish, 2 = thousand fish,
3 = million fish; weight 1 = kg; biomass 1 = kg, 2 = t, 3 = thousand t; male
substitute 0 = supplied, 1 = female population weights, 2 = unweighted arithmetic
mean of male fishery weight vectors. Abundance and biomass codes must match.
The native reader checks the version, dimensions, age order, finite positive
weights, unit compatibility, filename matching, sentinel, and trailing fields.
The R validator also checks the original positional input files before execution.

For the synthetic scientific regression suite, set `SPMR_TEST_EXE` to the absolute
path of the newly compiled executable before running package tests. The native
suite compares recruitment statistics, reference points, complete stochastic
trajectories, and an independent abundance-times-population-weight calculation.
The dedicated GitHub Actions job compiles the engine and requires these tests.

Historical outputs can be read without metadata. New execution requires metadata
and a version-compatible binary. Use the R runner to record executable, input,
and output hashes with each successful or failed run. A direct executable call
produces a native input receipt but lacks the R runner's complete hash record.
