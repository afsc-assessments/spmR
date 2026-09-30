# Migrating split-sex projection inputs

Version 0.4.1 requires input format 2 for new ADMB projections. This
guide uses synthetic data to show the change from legacy split-sex files
to explicit recruitment and population-weight metadata. The validation
examples run while this vignette is built; compiling and running ADMB
are separate steps.

## What changes in the input files

Keep the positional layout of `spm.dat`. In each split-sex species file
(for example, `stock.prj`), insert `wt_M` immediately after `wt_F`.
Version 0.4.1 does not read or require `tacpar.dat`. Add a separate
`spm_metadata.json` file with one species record per stock, in the order
listed in `spm.dat`.

- Declare the basis of the complete historical recruitment series:
  `"total"` for both sexes combined or `"per_sex"` for either sex under
  a 50:50 recruitment ratio. Pooled-sex inputs require `"total"`.
- Supply female and male **population** weight-at-age vectors in the
  metadata. Each vector carries its age labels. The positional `wt_F`
  and `wt_M` vectors supply female and male spawning weights, and
  `wt_gear_F` and `wt_gear_M` supply catch weights. Female spawning
  weight continues to determine SSB.
- Keep the initial female and male abundance vectors in the species
  file. The recruitment-basis declaration applies to historical
  recruitment; it leaves these sex-specific initial vectors unchanged.
- Use `N_scalar = 1`. Convert existing scaled data to the declared units
  before creating metadata. Update values as needed while preserving
  field order.

**Keep population-weight vectors in the metadata file.** The new
positional `wt_M` vector is a spawning weight, not a population weight.
Inserting a field anywhere else in a positional species file can shift
subsequent fields. An older executable may ignore additional
information. The R runner validates metadata, creates the native
`spm_input_v2.dat` file, and checks executable compatibility. Recompile
the current `inst/admb/spm.tpl` before running format-2 inputs.

## A reproducible synthetic input folder

The following small example represents two sexes, one fishery, and three
ages. All abundance values are fish, all weights are kg, and catch and
biomass are kg. Replace these synthetic values with assessment inputs
for an actual projection.

``` r

projection_dir <- tempfile("spmr-split-sex-")
dir.create(projection_dir)
ages <- 1:3

# Comments identify fields and make the new split-sex order explicit.
write_fields <- function(fields, path) {
  lines <- unlist(Map(function(name, values) {
    c(paste("#", name), paste(values, collapse = " "))
  }, names(fields), fields), use.names = FALSE)
  writeLines(lines, path)
}

write_fields(list(
  run_name = "synthetic_split_sex", Tier = 3, nalts = 1, alt_list = 5,
  TAC_ABC = 1, SrType = 2, Rec_Gen = 1, Fmsy_F35 = 1, Rec_Cond = 0,
  Write_Big = 1, npro = 3, nsims = 100, styr = 2026, nyrs_catch_in = 1,
  nspp = 1, OY_min = 0, OY_max = 2000000, spp_file_name = "stock.prj",
  ABC_Multiplier = 1, N_scalar = 1, Alt4_SPR = 0.6, ntacspp = 1,
  tac_ind = 1, Obs_Catch = c(2026, 0)
), file.path(projection_dir, "spm.dat"))

write_fields(list(
  spname = "synthetic_split_sex", SSL_spp = 0, Const_Buffer = 0,
  ngear = 1, nsexes = 2, avg_5yrF = 0.2, FABC_Adj = 1,
  SPR_abc = 0.4, SPR_ofl = 0.35, spawnmo = 4, nages = 3, Frat = 1,
  M_F = rep(0.2, 3), M_M = rep(0.2, 3),
  pmature_F = c(0, 0.5, 1), pmature_M = c(0, 0.5, 1),
  wt_F = c(0.1, 0.25, 0.5),
  wt_M = c(0.08, 0.2, 0.4),
  wt_gear_F = c(0.2, 0.4, 0.6), wt_gear_M = c(0.15, 0.3, 0.45),
  sel_F = c(0.1, 0.5, 1), sel_M = c(0.1, 0.5, 1),
  n0_F = c(100, 200, 300), n0_M = c(100, 180, 250),
  nrec = 4, R = c(80, 100, 120, 100), SSB = rep(1000, 4)
), file.path(projection_dir, "stock.prj"))
```

## Add explicit recruitment and population weights

Here the historical values `80, 100, 120, 100` represent **total**
recruitment. Population weights differ from the spawning and fishery
weights above.

``` r

species <- list(list(
  file = "stock.prj",
  recruitment_basis = "total",
  ages = ages,
  units = list(abundance = "fish", weight = "kg", biomass = "kg"),
  population_weights = list(
    female = list(ages = ages, values = c(0.1, 0.2, 0.3)),
    male = list(ages = ages, values = c(0.08, 0.16, 0.24))
  )
))

write_spm_metadata(projection_dir, species)
inputs <- validate_spm_inputs(projection_dir)
stopifnot(inputs$format_version == 2L)
inputs$resolved_species[[1]]
```

    $file
    [1] "stock.prj"

    $stock
    [1] "synthetic_split_sex"

    $nsexes
    [1] 2

    $nages
    [1] 3

    $ages
    [1] 1 2 3

    $recruitment_basis
    [1] "total"

    $units
    $units$abundance
    [1] "fish"

    $units$weight
    [1] "kg"

    $units$biomass
    [1] "kg"


    $population_weights
    $population_weights$female
    [1] 0.1 0.2 0.3

    $population_weights$male
    [1] 0.08 0.16 0.24


    $male_population_substitute
    [1] "none"

[`write_spm_metadata()`](http://afsc-assessments.github.io/spmR/reference/write_spm_metadata.md)
writes `format_version = 2` and validates the complete input folder.
Weight vectors must contain one finite, strictly positive value per age.
Age labels must be consecutive, ascending, nonnegative integers and
match the species file’s age count and abundance ordering, including its
final plus group. The validator checks labels and dimensions; the author
establishes that the original assessment values follow the declared
order.

Supported abundance/weight/biomass combinations are `fish`/`kg`/`kg`,
`thousand_fish`/`kg`/`t`, and `million_fish`/`kg`/`thousand_t`. The
metadata checks the declaration; it performs no unit conversion. All
stocks in a run must share abundance and biomass units.

## How recruitment is normalized

For split-sex inputs, a per-sex history of `40, 50, 60, 50` represents
the same total history as this example. The engine doubles the
**complete per-sex history** before calculating its arithmetic mean,
harmonic mean, variability, recruitment distribution, or
stock–recruitment inputs. It then allocates each projected total equally
to the two sexes once. Doubling only a fitted mean would leave other
calculations on a different basis.

The calculation below illustrates the normalization; the native
regression suite checks the engine’s recruitment moments, reference
points, and trajectories.

``` r

total_history <- c(80, 100, 120, 100)
per_sex_history <- c(40, 50, 60, 50)
stopifnot(identical(total_history, 2 * per_sex_history))
c(arithmetic_mean = mean(total_history),
  harmonic_mean = 1 / mean(1 / total_history))
```

    arithmetic_mean   harmonic_mean
          100.00000        97.95918 

Choose the basis from the assessment’s recruitment definition. If the
input history is already total recruitment, label it `"total"`. Metadata
describes the stored series; setting `"per_sex"` on total values would
double them. Initial abundance at the youngest age is already supplied
in `n0_F` and `n0_M`.

## Missing male weights and explicit substitutes

A split-sex input with missing male population weights stops before
execution, including when `strict = FALSE`. That argument controls
unknown metadata fields; the scientific checks remain active.

``` r

missing_male <- species
missing_male[[1]]$population_weights$male <- NULL
male_error <- tryCatch(
  write_spm_metadata(projection_dir, missing_male),
  error = function(e) conditionMessage(e)
)
stopifnot(grepl("missing male population", male_error, fixed = TRUE))
cat(male_error)
```

    metadata species 1 (stock.prj): missing male population weight-at-age vector; supply it or explicitly declare male_population_substitute.

An author may explicitly select `"female_population"` (use female
population weights for males) or `"mean_male_fishery"` (use the
unweighted arithmetic mean of male fishery weights across fleets, at
each age). Both choices produce a warning and enter the run record.
Record the scientific reason for using a substitute in the assessment
and compare its effect on biomass. Supply either male weights or a
substitute selection.

``` r

substituted <- missing_male
substituted[[1]]$male_population_substitute <- "female_population"
write_spm_metadata(projection_dir, substituted)
```

    Warning: metadata species 1 (stock.prj): male population weights use declared
    female_population.

``` r

# Restore the measured/synthetic sex-specific vectors for the main example.
write_spm_metadata(projection_dir, species)
```

## Verify the starting year and total biomass

The supplied initial abundance must represent the beginning of `styr` in
`spm.dat` (2026 here). If the assessment state is from the preceding
year, advance survival and ageing, accumulate the plus group, and
explicitly choose the youngest-age recruitment for the starting year
before writing the files. The metadata declaration alone performs none
of these boundary calculations.

Total biomass at the beginning of the year is the sum over ages of
female abundance times female population weight plus male abundance
times male population weight. For this synthetic input:

``` r

stock <- inputs$species[[1]]
weights <- inputs$resolved_species[[1]]$population_weights
initial_biomass_kg <- sum(stock$n0_F * weights$female +
                         stock$n0_M * weights$male)
stopifnot(isTRUE(all.equal(initial_biomass_kg, 236.8)))
initial_biomass_kg
```

    [1] 236.8

Holding abundance and population weights fixed while changing fishery
weights preserves this first-year biomass. Fishery weights can change
catch and the fishing mortality required to attain a catch, affecting
later trajectories. Spawning biomass uses female abundance, maturity,
spawning weights, and survival to spawning time.

In detailed output, `Year = t` biomass uses beginning-of-year abundance
in year `t`, while `Rec` is the draw entering the youngest age in **year
`t + 1`**. `Ntot` reports mature abundance. Use all ages and population
weights for the independent total-biomass check above.

## Run and retain the evidence

Build `inst/admb/spm.tpl` with ADMB 13.0 or newer, then place the
resulting `spm` (or `spm.exe` on Windows) in the projection folder or on
`PATH`. The runner checks the executable’s `-spmr-capabilities` response
before execution.

``` r

result <- runSPM(projection_dir, run = TRUE, engine = "admb")
provenance <- jsonlite::read_json(file.path(
  projection_dir, "spm_last_success_provenance.json"
))
provenance$executable_sha256
provenance$species[[1]]$recruitment_basis
```

The run record includes executable and input SHA-256 hashes, declared
units, recruitment basis, resolved population weights, substitutions,
and output hashes. Each attempt has a record in `spm_run_history/`; the
last successful record is retained separately. Archive the input folder
and results together.

Supported new runs use `TAC_ABC = 1`, `Rec_Gen = 1` (historical
recruitment distribution) or `2` (stock–recruitment fit), and
`N_scalar = 1`. Fitted runs require a maximum gradient below `1e-4` and
a positive-definite Hessian. Auxiliary recruitment modes 3 and 4, TAC
fitting, and new experimental RTMB projections require further
implementation.

Read archived output with `runSPM(..., run = FALSE)` without changing
its inputs. Adding metadata leaves those historical results as they
were; produce new results in a separate folder, review recruitment,
biomass and reference points, and then update assessment tables.
