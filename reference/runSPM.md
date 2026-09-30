# Run SPM Analysis in a Specific Directory

This function runs a Stock Production Model (SPM) analysis in the
specified directory. The function changes the working directory to
\`dirname\`, runs the SPM analysis, and then reads the results from
\`spm_detail.csv\`. It returns to the original working directory after
completing the analysis.

## Usage

``` r
runSPM(
  dirname,
  ctrl = NULL,
  run = FALSE,
  engine = c("admb", "rtmb"),
  metadata = "spm_metadata.json",
  strict = TRUE
)
```

## Arguments

- dirname:

  A string specifying the directory in which to run the SPM analysis.

- ctrl:

  Optional control settings for the SPM analysis. If NULL, default
  settings are used.

- run:

  Logical. If TRUE, the SPM analysis is run. If FALSE, the function only
  reads the results from \`spm_detail.csv\`.

- engine:

  Model backend to use. \`"admb"\` runs or reads the legacy SPM
  implementation; \`"rtmb"\` can read experimental output but cannot run
  validated scientific projections. New ADMB runs require explicit
  version-2 metadata and a compatible executable. Existing output can
  still be read with \`run = FALSE\` without metadata.

- metadata:

  Filename of the version-2 metadata created by
  \[write_spm_metadata()\]. Used only for new ADMB runs.

- strict:

  Reject unknown metadata fields when TRUE. Scientific input
  requirements always apply.

## Value

An \`spm_result\` data frame containing standardized projection results.
Existing model-specific columns are preserved.

## Examples

``` r
if (FALSE) { # \dontrun{
runSPM("examples/atka")
} # }
```
