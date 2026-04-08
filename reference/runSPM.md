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

  Which execution path to use: legacy ADMB (\`"admb"\`) or the
  experimental RTMB path (\`"rtmb"\`).

## Value

A data frame containing the results from \`spm_detail.csv\`.

## Examples

``` r
if (FALSE) { # \dontrun{
runSPM("examples/atka")
} # }
```
