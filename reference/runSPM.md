# Run SPM Analysis in a Specific Directory

This function runs a Stock Production Model (SPM) analysis in the
specified directory. The function changes the working directory to
\`dirname\`, runs the SPM analysis, and then reads the results from
\`spm_detail.csv\`. It returns to the original working directory after
completing the analysis.

## Usage

``` r
runSPM(dirname, ctrl = NULL, run = FALSE, engine = c("admb", "rtmb"))
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
  implementation; \`"rtmb"\` uses the experimental R implementation.

## Value

An \`spm_result\` data frame containing standardized projection results.
Existing model-specific columns are preserved.

## Examples

``` r
if (FALSE) { # \dontrun{
runSPM("examples/atka")
} # }
```
