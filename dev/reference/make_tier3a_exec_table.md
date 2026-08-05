# Takes spm results and produces the values for the Tier3a executive summary table

Takes spm results and produces the values for the Tier3a executive
summary table

## Usage

``` r
make_tier3a_exec_table(run_dir, endyr, the_scalar)
```

## Arguments

- run_dir:

  the directory where the spm output is located

- endyr:

  the current year

- the_scalar:

  the scalar used in spm.dat for outputting results

## Value

Values for Tier 3a executive summary table

## Examples

``` r
make_tier3a_exec_table(run_dir = "C:/GitProjects/goa_deepwater/2025/harvest_projections", endyr = 2025, the_scalar = 1000)
#> Error in make_tier3a_exec_table(run_dir = "C:/GitProjects/goa_deepwater/2025/harvest_projections",     endyr = 2025, the_scalar = 1000): could not find function "make_tier3a_exec_table"
```
