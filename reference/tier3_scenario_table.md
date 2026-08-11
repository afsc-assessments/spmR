# Summarize the seven Tier 3 projection alternatives

Converts simulation-level SPM output into one row per Tier 3
alternative. Results for the requested projection years are placed in
separate columns so the returned object can be printed directly as an
assessment table.

## Usage

``` r
tier3_scenario_table(
  x,
  years = NULL,
  scenario_names = c("Maximum permissible ABC", "Author-specified ABC",
    "Average recent F", "Alternative SPR rate", "No fishing",
    "OFL threshold determination", "Status-determination ramp"),
  digits = 1
)
```

## Arguments

- x:

  An object returned by \[runSPM()\] or a data frame accepted by
  \[as_spm_result()\].

- years:

  Projection years to include. The default uses all years.

- scenario_names:

  Optional character vector of seven scenario names.

- digits:

  Number of digits used to round summarized values.

## Value

A tibble with one row per Tier 3 alternative and year-specific mean
Catch, ABC, OFL, spawning biomass, fishing mortality, and B/B35 columns.

## Examples

``` r
detail <- data.frame(
  Stock = "example", Alt = rep(1:7, each = 2), Sim = 1,
  Year = rep(2027:2028, 7), Catch = 100, SSB = 500,
  ABC = 110, OFL = 120, F = 0.2, B35 = 350
)
tier3_scenario_table(detail, years = 2027:2028)
#> # A tibble: 7 × 14
#>     Alt Scenario       Catch_2027 Catch_2028 ABC_2027 ABC_2028 OFL_2027 OFL_2028
#>   <int> <chr>               <dbl>      <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#> 1     1 Maximum permi…        100        100      110      110      120      120
#> 2     2 Author-specif…        100        100      110      110      120      120
#> 3     3 Average recen…        100        100      110      110      120      120
#> 4     4 Alternative S…        100        100      110      110      120      120
#> 5     5 No fishing            100        100      110      110      120      120
#> 6     6 OFL threshold…        100        100      110      110      120      120
#> 7     7 Status-determ…        100        100      110      110      120      120
#> # ℹ 6 more variables: SSB_2027 <dbl>, SSB_2028 <dbl>, F_2027 <dbl>,
#> #   F_2028 <dbl>, B_B35_2027 <dbl>, B_B35_2028 <dbl>
```
