# Convert the legacy two-sex Greenland turbot example to spmR input format 2.

if (!requireNamespace("spmR", quietly = TRUE) ||
    utils::packageVersion("spmR") < "0.4.1") {
  stop("Install spmR >= 0.4.1 before running this example.", call. = FALSE)
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
gt_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1])))
} else {
  candidates <- c(file.path(getwd(), "examples", "gt20"), getwd())
  candidates[file.exists(file.path(candidates, "data", "Model_Proj.dat"))][1]
}
if (is.na(gt_dir)) stop("Run from the spmR root or examples/gt20.", call. = FALSE)

run_dir <- Sys.getenv("SPMR_GT20_RUN_DIR", file.path(gt_dir, "spmR_run"))
if (!dir.exists(run_dir) &&
    !dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)) {
  stop("Could not create output directory: ", run_dir, call. = FALSE)
}

number_line <- function(x) paste(format(
  x, digits = 17, scientific = TRUE, trim = TRUE
), collapse = " ")
write_fields <- function(x, path) {
  lines <- unlist(Map(function(name, value) {
    c(paste("#", name), paste(value, collapse = " "))
  }, names(x), x), use.names = FALSE)
  writeLines(lines, path)
}

# Merge legacy setup.dat and spp_catch.dat into the current positional file.
# The 2020 assessment classifies this stock as Tier 3a; SPM encodes that as 3.
write_fields(list(
  run_name = "std", Tier = 3, nalts = 7, alt_list = 1:7,
  TAC_ABC = 1, SrType = 1, Rec_Gen = 1, Fmsy_F35 = 1,
  Rec_Cond = 0, Write_Big = 1, npro = 20, nsims = 100, styr = 2020,
  nyrs_catch_in = 1, nspp = 1, OY_min = 43.248, OY_max = 1943.248,
  spp_file_name = "Model_Proj.prj", ABC_Multiplier = 1, N_scalar = 1,
  Alt4_SPR = 0.75, ntacspp = 1, tac_ind = 1,
  Obs_Catch = c(2020, 3321.27)
), file.path(run_dir, "spm.dat"))

# Insert the male spawning-weight field required by format 2 and trim the two
# extra SSB values that the legacy executable silently ignored.
legacy <- readLines(file.path(gt_dir, "data", "Model_Proj.dat"), warn = FALSE)
where <- function(label) {
  i <- which(trimws(legacy) == label)
  if (length(i) != 1) stop("Could not locate ", label, call. = FALSE)
  i
}
numbers <- function(x) scan(text = sub("#.*$", "", x), quiet = TRUE)
ngear <- as.integer(numbers(legacy[grep("Number of fisheries", legacy, fixed = TRUE)[1]]))
nages <- as.integer(numbers(legacy[grep("number of ages", legacy, fixed = TRUE)[1]]))
nrec <- as.integer(numbers(legacy[where("# No Recruitments") + 1]))
if (nages != 30L) stop("GT20 should contain ages 1-30.", call. = FALSE)

female_spawn_i <- where("# wt spawn females") + 1
female_spawn_wt <- numbers(legacy[female_spawn_i])
# Format 2 requires weights >= machine epsilon; replace the two legacy
# zero-valued weights with that negligible positive value.
female_spawn_wt[female_spawn_wt <= 0] <- .Machine$double.eps
legacy[female_spawn_i] <- number_line(female_spawn_wt)
female_population_wt <- female_spawn_wt

male_gear_i <- where("# WtAge males by fishery")
male_gear_wt <- do.call(rbind, lapply(
  legacy[male_gear_i + seq_len(ngear)], numbers
))
if (ncol(male_gear_wt) != nages) {
  stop("Male fishery weights do not match nages.", call. = FALSE)
}
# GT20 has no male spawning weights. This positive placeholder is required by
# format 2 but does not enter the female-only spawning-biomass calculation.
male_spawn_wt <- colMeans(male_gear_wt)

ssb_i <- grep("^# SSB", trimws(legacy)) + 1
ssb <- numbers(legacy[ssb_i])
if (length(ssb) != nrec + 2L) {
  stop("Expected 41 paired and two trailing SSB values in legacy GT20.",
       call. = FALSE)
}
legacy[ssb_i] <- number_line(ssb[seq_len(nrec)])

species_file <- file.path(run_dir, "Model_Proj.prj")
legacy <- append(
  legacy,
  c("# wt spawn males", number_line(male_spawn_wt)),
  after = female_spawn_i
)
writeLines(legacy, species_file)

# Legacy main.tpl treated R as total recruitment and then split it 50:50.
# The assessment identifies these as ages 1:30, with age 30 a plus group.
# Female spawning weights are also used as population weights for legacy
# biomass parity. Male weights are the legacy mean-across-fisheries substitute.
ages <- seq_len(nages)
suppressWarnings(spmR::write_spm_metadata(run_dir, list(list(
  file = basename(species_file),
  recruitment_basis = "total",
  ages = ages,
  units = list(abundance = "thousand_fish", weight = "kg", biomass = "t"),
  population_weights = list(
    female = list(ages = ages, values = female_population_wt)
  ),
  # This reproduces and explicitly records the legacy biomass convention.
  male_population_substitute = "mean_male_fishery"
))))
inputs <- suppressWarnings(spmR::validate_spm_inputs(run_dir))
if (inputs$species[[1]]$nsexes != 2) {
  stop("Converted species input is not two-sex.", call. = FALSE)
}
stock <- inputs$species[[1]]
population_wt <- inputs$resolved_species[[1]]$population_weights
initial_biomass <- sum(
  stock$n0_F * population_wt$female + stock$n0_M * population_wt$male
)
if (round(initial_biomass, 1) != 92104.6) {
  stop("Converted starting biomass does not reproduce legacy GT20.", call. = FALSE)
}
message("Validated two-sex inputs and legacy starting biomass (",
        round(initial_biomass, 1), " t) in ", normalizePath(run_dir))

exe <- file.path(run_dir, if (.Platform$OS.type == "windows") "spm.exe" else "spm")
if (!file.exists(exe) && !nzchar(Sys.which("spm"))) {
  tpl <- system.file("admb", "spm.tpl", package = "spmR")
  message("Compile ", tpl, " with ADMB 13+ and place the resulting ",
          "executable in ", normalizePath(run_dir), " to run it.")
} else {
  result <- spmR::runSPM(run_dir, run = TRUE, engine = "admb")
  if (nrow(result) != 14000L ||
      !setequal(unique(result$Alt), 1:7) ||
      !identical(range(result$Year), c(2020, 2039))) {
    stop("Projection output dimensions do not match GT20 controls.", call. = FALSE)
  }
  print(result)
}
