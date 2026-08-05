# Convert Data to List

This function reads data from a file and converts it into a list. If the
data are numeric, it maintains the numeric list. If the data are
strings, it returns a character string.

## Usage

``` r
dat2list(fn)
```

## Arguments

- fn:

  A character string representing the file name to be read.

## Value

A list with numeric data or character strings based on the content of
the file.

## Examples

``` r
# Example usage:
# result <- dat2list("datafile.txt")
```
