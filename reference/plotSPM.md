# Plot SPM Data

This function filters and processes a dataframe, then creates a plot of
the SPM data with error bands and faceting by type and alternative.

## Usage

``` r
plotSPM(df, alt = c(1, 3, 5, 7), mytitle = NULL)
```

## Arguments

- df:

  A dataframe containing the SPM data with columns \`Year\`, \`Alt\`,
  \`variable\`, and \`value\`.

- alt:

  A vector of alternatives to include in the plot. Default is \`c(1, 4,
  5, 7)\`.

- mytitle:

  An optional title for the plot. Default is \`NULL\`.

## Value

A ggplot object.

## Examples

``` r
# Example usage:
df <- data.frame(Year = rep(2000:2020, 4),
                 Alt = rep(1:4, each = 21),
                 variable = rep(c("mean_ub", "mean_lb", "mean_mean"), times = 28),
                 value = runif(84, 0, 1))
plotSPM(df)
#> Warning: Removed 4 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> Warning: Removed 42 rows containing missing values or values outside the scale range
#> (`geom_ribbon()`).
```
