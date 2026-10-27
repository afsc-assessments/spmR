# SPM source code

The ADMB templates and helper scripts for the Standard Projection Model live
in this installed-data directory because an R package's `src/` directory is
reserved for source code compiled during package installation.

To compile the main model with ADMB 13.0 or newer, copy `spm.tpl` to a
writable location and run:

```sh
admb -f spm.tpl
```

`Makefile.admb` contains an optional cross-platform wrapper for this command.

The template includes its input-format-2 metadata reader and needs no separate
header. `spm -spmr-capabilities` must print
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

## Run directly from a terminal

The native executable reads `spm.dat`, the species files named in it, and
`spm_input_v2.dat` from the current directory. It does not read the R interface's
`spm_metadata.json`. Once those inputs are prepared, run:

```sh
cd /path/to/projection-inputs
spm -spmr-capabilities  # must print SPMR_INPUT_FORMAT=2
spm
```

Keep the positional `spm.dat` layout. Supported settings include `TAC_ABC = 1`,
`Rec_Gen = 1` or `2`, and `N_scalar = 1`. A split-sex species file includes the
male spawning-weight vector `wt_M` immediately after `wt_F`. Each historical
recruitment and SSB series must have exactly `nrec` values, and the initial
abundance vectors must represent the start of the first projection year.

For example, this native metadata file describes a synthetic two-sex,
three-age stock with total recruitment, fish abundance, kg weights, and kg
biomass. The stock filename and dimensions must match the species file:

```text
SPMR_INPUT_V2 2 1
stock.prj 2 3 1 1 1 1 0
1 2 3
0.10 0.20 0.30
0.08 0.16 0.24
END_SPMR_INPUT_V2
```

The last two vectors are female and male population weights. Supply actual
population weights for the assessment; spawning weights and fishery weights
serve different purposes. Set the recruitment code to `2` only when the
complete historical series is per sex. A direct run writes
`spm_input_receipt.tsv` and projection outputs into the current directory, so
use a separate run folder to retain earlier results.

For the synthetic scientific regression suite, set `SPMR_TEST_EXE` to the absolute
path of the newly compiled executable before running package tests. The native
suite compares recruitment statistics, reference points, complete stochastic
trajectories, and an independent abundance-times-population-weight calculation.
The dedicated GitHub Actions job compiles the engine and requires these tests.

Historical outputs can be read without metadata. New execution requires metadata
and a version-compatible binary. Use the R runner to record executable, input,
and output hashes with each successful or failed run. A direct executable call
produces a native input receipt but lacks the R runner's complete hash record.
