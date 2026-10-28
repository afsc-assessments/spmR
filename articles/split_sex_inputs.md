# Migrating split-sex projection inputs

Current spmR requires input format 2 for new ADMB projections. Version
0.4.3 embeds its declarations as comments in each species file by
default. This guide uses synthetic data to show the change from legacy
split-sex files to explicit recruitment and population-weight
declarations. Validation runs whenever this vignette is built; native
execution in the final worked example is enabled explicitly.

## What changes in the input files

Keep the positional layout of `spm.dat`. In each split-sex species file
(for example, `stock.prj`), insert `wt_M` immediately after `wt_F`. The
current engine does not read or require `tacpar.dat`. Use
[`write_spm_metadata()`](https://afsc-assessments.github.io/spmR/reference/write_spm_metadata.md)
to append a versioned comment block with the declarations for each
stock.

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

**Keep population-weight vectors in the versioned comment block.** The
positional `wt_M` vector supplies spawning weights. Adding another
positional field can shift subsequent fields. The current native engine
reads the appended comment block directly, preserving the existing
positional field order. The R runner validates the declarations and
checks executable compatibility. Recompile the current
`inst/admb/spm.tpl` to use embedded inputs directly.

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

[`write_spm_metadata()`](https://afsc-assessments.github.io/spmR/reference/write_spm_metadata.md)
embeds version-2 declarations in `stock.prj` and validates the complete
input folder. Weight vectors must contain one finite, strictly positive
value per age. Age labels must be consecutive, ascending, nonnegative
integers and match the species file’s age count and abundance ordering,
including its final plus group. The validator checks labels and
dimensions; the author establishes that the original assessment values
follow the declared order.

Supported abundance/weight/biomass combinations are `fish`/`kg`/`kg`,
`thousand_fish`/`kg`/`t`, and `million_fish`/`kg`/`thousand_t`. The
metadata checks the declaration; it performs no unit conversion. All
stocks in a run must share abundance and biomass units.

The appended block for this synthetic stock is:

``` text
# SPMR_INPUT_V2 2 1
# stock.prj 2 3 1 1 1 1 0
# 1 2 3
# 0.1 0.2 0.3
# 0.08 0.16 0.24
# END_SPMR_INPUT_V2
```

The versioned header identifies one stock. Its record gives the
filename, sex count, age count, recruitment basis,
abundance/weight/biomass unit codes, and male-weight substitution code.
The final three rows contain ages, female population weights, and male
population weights. Use the R writer to build and validate the block
from named declarations.

## Existing JSON workflows

The default `metadata = NULL` reads embedded declarations first. When no
species file has a block, it falls back to `spm_metadata.json`. To
select a JSON file explicitly, use the same `metadata` argument
throughout:

``` r

write_spm_metadata(projection_dir, species, metadata = "spm_metadata.json")
validate_spm_inputs(projection_dir, metadata = "spm_metadata.json")
runSPM(projection_dir, run = TRUE, metadata = "spm_metadata.json")
```

The R runner prepares embedded declarations for current engines and
generates the legacy `spm_input_v2.dat` sidecar for older format-2
engines. The current native engine accepts that sidecar when embedded
declarations are absent.

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
c(
  arithmetic_mean = mean(total_history),
  harmonic_mean = 1 / mean(1 / total_history)
)
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

The template includes its format-2 reader and compiles on its own; there
is no separate `spmr_input_v2.hpp` dependency. The example helpers can
compile the installed template with `admb` on `PATH`, or a compiler
selected by `SPMR_ADMB`. After updating the source checkout, reinstall
spmR to refresh that template. The current engine reports both
`SPMR_INPUT_FORMAT=2` and `SPMR_INLINE_INPUT=1`. With the embedded
blocks in place, a standalone run needs `spm.dat` and its species files.
Older format-2 engines require the sidecar generated by the R runner.

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

## Worked example: 2020 Greenland turbot

The repository includes a documented conversion of the 2020 Bering Sea
and Aleutian Islands Greenland turbot projection. The assessment treated
this as a Tier 3a stock and projected ages 1–30, with age 30 as the plus
group. The example retains the archived two-sex species input and writes
all converted inputs and outputs to a temporary directory. It is a
source-checkout example; clone the [spmR
repository](https://github.com/afsc-assessments/spmR) before running the
code below.

The helper embeds its validated declarations as comments in the
generated species file. The current native engine reads that file and
`spm.dat` directly; the R runner supplies a sidecar when using an older
format-2 executable.

The conversion declares total recruitment in `thousand_fish`, weights in
`kg`, and biomass in `t`. The archived female spawning weights are
reused as female population weights to preserve legacy biomass. No
independent male population-weight vector was archived, so the example
explicitly records `mean_male_fishery`; this is a reproducibility
substitute, not a new biological estimate.

``` r

# Locate the example both when Quarto starts in vignettes/ and when it starts at
# the package root.
gt20_candidates <- c(
  file.path("..", "examples", "gt20"),
  file.path("examples", "gt20")
)
gt20_source_dir <- gt20_candidates[file.exists(file.path(
  gt20_candidates, "run_spmR_split_sex.R"
))][1]
if (is.na(gt20_source_dir)) {
  stop("Clone the spmR source repository to run the GT20 worked example.")
}
gt20_source_dir <- normalizePath(gt20_source_dir, mustWork = TRUE)
source(file.path(gt20_source_dir, "run_spmR_split_sex.R"), local = TRUE)

# Input conversion and validation run in every build. Set SPMR_RUN_GT20=true to
# opt in to native execution; the pkgdown workflow sets it after compiling the
# pinned engine.
gt20_run_native <- identical(
  tolower(Sys.getenv("SPMR_RUN_GT20", unset = "false")), "true"
)
gt20_executable <- ""
gt20_admb <- ""
if (gt20_run_native) {
  gt20_executable <- Sys.getenv(
    "SPMR_EXECUTABLE",
    unset = Sys.getenv("SPMR_TEST_EXE", unset = "")
  )
  if (!nzchar(gt20_executable)) gt20_executable <- unname(Sys.which("spm"))
  gt20_admb <- Sys.getenv("SPMR_ADMB", unset = "")
  if (!nzchar(gt20_admb)) gt20_admb <- unname(Sys.which("admb"))
  if (!nzchar(gt20_executable) && !nzchar(gt20_admb)) {
    stop("SPMR_RUN_GT20=true but no SPM executable or ADMB compiler was found.")
  }
}

gt20 <- run_gt20_projection(
  source_dir = gt20_source_dir,
  run_dir = tempfile("spmr-gt20-vignette-"),
  executable = gt20_executable,
  admb = gt20_admb,
  build = TRUE,
  run = gt20_run_native
)
```

    Warning: GT20 uses declared mean_male_fishery population weights for males.

    Completed GT20 projection in /tmp/Rtmpth8pgL/spmr-gt20-vignette-1aa1424d1a3d; starting biomass = 92104.6 t.

``` r

data.frame(
  metric = "Beginning-of-2020 total biomass (t)",
  value = round(gt20$initial_biomass, 1)
)
```

                                   metric   value
    1 Beginning-of-2020 total biomass (t) 92104.6

With `SPMR_RUN_GT20=true`, the same evaluated chunk runs all seven
alternatives and verifies 14,000 rows (100 simulations over 20 years).
The published site build supplies a pinned ADMB 13.2 engine and displays
this concise alternative-1 summary and its provenance record.

``` r

if (gt20_run_native) {
  print(gt20$summary[
    gt20$summary$Alt == 1 & gt20$summary$Year <= 2022,
    c("Alt", "Year", "SSB", "Catch", "ABC", "OFL")
  ])
  data.frame(
    executable_sha256 = gt20$provenance$executable_sha256,
    recruitment_basis = gt20$provenance$species[[1]]$recruitment_basis
  )
} else {
  cat(paste(
    "Format-2 inputs validated; set SPMR_RUN_GT20=true to run the native",
    "projection."
  ))
}
```

       Alt Year     SSB   Catch     ABC     OFL
    1    1 2020 52902.5 3321.27 8049.94 9413.84
    8    1 2021 51913.9 7326.40 7326.40 8567.92
    15   1 2022 47196.8 6139.32 6139.32 7181.26

                                                     executable_sha256
    1 baa5a9e7b7a1fdab932b452efb28ac81a8794275ef45e20c51b2ff1d7b8cd46b
      recruitment_basis
    1             total

The example reproduces beginning-of-2020 total biomass of `92,104.6 t`.
For alternative 1, mean 2021 SSB is `51,913.9 t` and ABC is `7,326.4 t`,
matching the published projection after rounding. The complete source is
`examples/gt20/run_spmR_split_sex.R`.

The separate [2026 Greenland turbot
example](https://afsc-assessments.github.io/spmR/articles/gt2026_inputs.md)
preserves the revised export and audits its unresolved definitions. Its
47 recruitment values and 45 SSB values block staging until matching
source histories are supplied. Diagnostic population weights are
fishery-weight proxies requiring an explicit choice and separate
scientific review.
