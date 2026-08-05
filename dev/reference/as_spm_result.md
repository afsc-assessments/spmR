# Validate and standardize projection model results

\`as_spm_result()\` converts a model output data frame to the common
result format used by spmR. Legacy SPM output is supported by deriving
\`Scenario\` from \`Alt\` when needed. Model-specific columns are
preserved.

## Usage

``` r
as_spm_result(x)
```

## Arguments

- x:

  A data frame containing projection model results.

## Value

\`x\` with a \`Scenario\` column and the additional S3 class
\`spm_result\`.

## Examples

``` r
result <- data.frame(
  Stock = "example",
  Alt = 1,
  Sim = 1,
  Year = 2025,
  Catch = 100,
  SSB = 500,
  ABC = 110,
  OFL = 120
)
as_spm_result(result)
#>     Stock Alt Sim Year Catch SSB ABC OFL Scenario
#> 1 example   1   1 2025   100 500 110 120        1
```
