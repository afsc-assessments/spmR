# spmR

The R package `spmR` was developed for doing stock projections for
groundfish at the AFSC. The model was coded using Automatic
Differentiation Model Builder (`ADMB`).

The main projection model is
[`inst/admb/spm.tpl`](http://afsc-assessments.github.io/spmR/inst/admb/spm.tpl).
A model run requires `spm.dat`, `tacpar.dat`, and a species-specific
file containing assessment outputs. The
[`examples`](http://afsc-assessments.github.io/spmR/examples) directory
contains complete example inputs and outputs. ADMB 13.0 or newer is
required to compile the model.

## Input format 2

Version 0.4.0 requires a `spm_metadata.json` file for new projections.
Keep the legacy positional files unchanged. An extra vector in a
positional file can shift subsequent fields or be ignored by an older
program. The R runner validates the metadata and generates the native
`spm_input_v2.dat` file. It checks the executable’s `-spmr-capabilities`
response before running the projection. Recompile `inst/admb/spm.tpl`
for this version.

Each stock declares `recruitment_basis = "total"` or `"per_sex"`.
Per-sex inputs represent either sex under a 50:50 recruitment ratio. The
engine converts the **complete recruitment history** to total
recruitment first, then calculates its mean, variability, harmonic mean,
and stock–recruitment inputs. It allocates each projected total equally
to females and males once. This choice must describe the input series;
select it from the assessment’s definitions.

Population weights determine total biomass. Supply separate female and
male population weight-at-age vectors, with ages attached to each. The
legacy female weight vector remains the spawning weight; fishery weights
remain catch weights. A split-sex run with missing male population
weights stops before execution. An explicit `male_population_substitute`
can select `"female_population"` or `"mean_male_fishery"`; either choice
produces a warning and is recorded. Review that scientific assumption
before using a substitute. Strict mode never chooses one automatically.

The supported units are weight in kg with abundance/biomass pairs
`fish`/`kg`, `thousand_fish`/`t`, or `million_fish`/`thousand_t`. All
values must already use those units; the metadata records and checks the
declaration. Ages must be consecutive, ascending integers, and each
weight vector must match their order. Format 2 requires `N_scalar = 1`.
Convert scaled inputs before creating metadata. All stocks within one
run must share abundance and biomass units because the engine adds their
catches when applying overall limits.

For an existing projection folder, provide a species entry such as:

``` r

ages <- 1:15
species <- list(list(
  file = "stock.prj",
  recruitment_basis = "total",
  ages = ages,
  units = list(abundance = "million_fish", weight = "kg", biomass = "thousand_t"),
  population_weights = list(
    female = list(ages = ages, values = female_population_weight),
    male = list(ages = ages, values = male_population_weight)
  )
))
write_spm_metadata("projection", species = species)
validate_spm_inputs("projection")
result <- runSPM("projection", run = TRUE)
```

Stock–recruitment fits must have a maximum gradient below 1e-4 and a
positive-definite Hessian before their outputs are accepted. Recruitment
CV² below 1e-12 is treated as zero to handle nearly constant histories.

The output provenance records the executable and input SHA-256 hashes,
recruitment basis, unit declarations, substitutions, and run outcome.
Each attempt has a manifest in `spm_run_history/`;
`spm_last_success_provenance.json` retains the last successful result.
Publication failures restore prior outputs. The runner uses a clean
execution directory so an old output file cannot pass as a new run.
Historical results remain readable with `runSPM(..., run = FALSE)`.

Projection timing follows the legacy engine: `Year = t` biomass uses the
beginning-of-year abundance supplied or advanced into year t. The
detailed output `Rec` column is the draw assigned to the youngest age in
**year t + 1**. Initial-year recruitment is already part of the supplied
abundance vector. The `Ntot` column reports mature abundance under the
supplied maturity vectors; calculate total biomass from all ages and the
population weights.

Format 2 currently supports `TAC_ABC = 1` and recruitment modes 1 and 2.
Modes 3 and 4 need an explicit convention for their auxiliary inputs.
New experimental RTMB projections also require further implementation;
existing output can still be read. These boundaries are checked before
execution.

## Supported public API

The supported exported functions are:

- [`dat2list()`](http://afsc-assessments.github.io/spmR/reference/dat2list.md)
- [`list2dat()`](http://afsc-assessments.github.io/spmR/reference/list2dat.md)
- [`write_spm_metadata()`](http://afsc-assessments.github.io/spmR/reference/write_spm_metadata.md)
- [`validate_spm_inputs()`](http://afsc-assessments.github.io/spmR/reference/write_spm_metadata.md)
- [`as_spm_result()`](http://afsc-assessments.github.io/spmR/reference/as_spm_result.md)
- [`runSPM()`](http://afsc-assessments.github.io/spmR/reference/runSPM.md)
- [`plotSPM()`](http://afsc-assessments.github.io/spmR/reference/plotSPM.md)
- [`plotSPMx()`](http://afsc-assessments.github.io/spmR/reference/plotSPMx.md)
- [`tier3_scenario_table()`](http://afsc-assessments.github.io/spmR/reference/tier3_scenario_table.md)

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

?runSPM
?plotSPM
?dat2list
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
vignette(topic = "spm_example", package = "spmR")
```

## Website

All of the vignettes and the help pages for each function are bundled
together and published on the website
<https://afsc-assessments.github.io/spmR/>.

## Developers

Developers will want to do things slightly differently. See the
`Model development` vignette.

# Acronyms

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
