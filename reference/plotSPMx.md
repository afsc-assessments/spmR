# Plot Results from SPM Analysis from a Tibble

This function creates plots from the results of a Stock Production Model
(SPM) analysis contained in a tibble. It generates three separate plots
for different metrics and combines them using the \`patchwork\` package.

## Usage

``` r
plotSPMx(df, alt = 2, thisyr = 2022, mytitle = NULL)
```

## Arguments

- df:

  A tibble containing the results from \`spm_detail.csv\`.

- alt:

  The alternative to be plotted.

- thisyr:

  The current year for the projection model.

- mytitle:

  Title for the plot

## Value

A combined plot of the results from the SPM analysis.
