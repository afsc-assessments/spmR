# Write List Object to Projection Model Data File

This function writes a list object to a file formatted for a projection
model. Each element of the list is written with a header.

## Usage

``` r
list2dat(D, fn, hdr = "a new file")
```

## Arguments

- D:

  List object to be written.

- fn:

  Filename where the data will be written.

- hdr:

  Header text to be included in the file.

## Value

The function does not return a value; it writes to a file.

## Examples

``` r
# Example usage:
# list2dat(myList, "datafile.dat", "Header for new file")
```
