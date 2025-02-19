README
================
Jim Ianelli
2023-09-15

## Standard Projection Model for Alaska Groundfish

This is the standard projection code for federally managed Alaskan
groundfish species in Tier 3.

The file src/spm.tpl is the main code for running these projections.
Required input files are spm.dat, tacpar.dat, and a species-specific
data input file with key outputs from an assessment, such as
examples/data/ai_spm.dat.

Note: spm.dat combines information from two files used in previous
versions of the projection code (setup.dat and spp_catch.dat)

ADMB version 13.0 or higher is required to compile spm.tpl.

These are the steps to run the projection code:  
1. Compile src/spm.tpl using admb and copy spm.exe to a a folder with
spm.dat, tacpar.dat, and a file containing assessment outputs, such as
ai_spm.dat  
2. Within spm.dat specify the location of the assessment input file
relative to the folder with spm.exe  
3. From the command line pointed to the folder with these inputs, type
“spm”  

<!-- README.md is generated from README.Rmd. Please edit that file -->

# spmR

The R package `spmR` was developed for doing stock projections for
groundfish at the AFSC. The model was coded using the Autodif (`AD`)
Model Builder (`ADMB`) software.

## Cloning the repository (optional)

The R package `spmR` lives on a public GitHub repository. The repository
can be cloned to your computer from the command line or using a user
interface. From the command line using Linux the repository can be
cloned using:

``` r
git clone https://github.com/afsc-assessments/spmR
```

## Installation

There are several options for installing the `spmR` R package.

### Option 1

The `spmR` package can be installed from within R using:

``` r
devtools::install_github(repo = "afsc-assessments/spmR", dependencies = TRUE, 
                         build_vignettes = TRUE, auth_token = "your_PAT")
```

### Option 2

The GitHub repository can be cloned to your computer and the package
installed from the command line. From Linux this would involve:

``` r
git clone https://github.com/afsc-assessments/spmR
R CMD INSTALL spmR
```

### Option 3

This time from within R using:

``` r
devtools::install("spmR")
```

## Help

Help for all `spmR` functions and data sets can be found on the R help
pages associated with each function and data set. Help for a specific
function can be viewed using `?function_name`, for example:

``` r
?run_model
?tab_fit
?plot_sel
```

Alternatively, to see a list of all available functions and data sets
use:

``` r
help(package = "spmR")
```

## Examples

The package vignettes are a great place to see what `spmR` can do. You
can view the package vignettes from within R using:

``` r
browseVignettes(package = "spmR")
vignette(topic = "spmR", package = "spmR")
```

## Website

All of the vignettes and the help pages for each function are bundled
together and published on the website
<https://afsc-assessments.github.io/spmR/>.

## Developers

Developers will want to do things slightly differently. See the
`Model development` vignette.

# Acronymns

NOAA: National Oceanic and Atmospheric Administration  
NMFS: National Marine Fisheries Service  
AFSC: Alaska Fisheries Science Center  
REFM: Resource and Ecology and Fisheries Management

# Legal disclaimer

This repository is a software product and is not official communication
of the National Oceanic and Atmospheric Administration (NOAA), or the
United States Department of Commerce (DOC). All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the DOC or DOC bureaus stemming from the use
of this GitHub project will be governed by all applicable Federal law.
Any reference to specific commercial products, processes, or services by
service mark, trademark, manufacturer, or otherwise, does not constitute
or imply their endorsement, recommendation, or favoring by the DOC. The
DOC seal and logo, or the seal and logo of a DOC bureau, shall not be
used in any manner to imply endorsement of any commercial product or
activity by the DOC or the United States Government.
