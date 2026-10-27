gt2026_locate_source <- function() {
  frame_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) "" else as.character(frame$ofile)[1]
    },
    character(1)
  )
  command_files <- sub(
    "^--file=",
    "",
    grep(
      "^--file=",
      commandArgs(FALSE),
      value = TRUE
    )
  )
  candidates <- unique(c(
    dirname(c(frame_files[nzchar(frame_files)], command_files)),
    file.path(getwd(), "examples", "gt_2026"),
    file.path(getwd(), "..", "..", "examples", "gt_2026"),
    getwd()
  ))
  candidates <- candidates[
    file.exists(file.path(candidates, "ForProjections.dat")) &
      file.exists(file.path(candidates, "run.R"))
  ]
  if (!length(candidates)) {
    stop("Could not locate examples/gt_2026; supply source_dir.", call. = FALSE)
  }
  normalizePath(candidates[1], mustWork = TRUE)
}

.gt2026_source_dir <- gt2026_locate_source()

gt2026_read_source <- function(source_dir) {
  lines <- readLines(file.path(source_dir, "ForProjections.dat"), warn = FALSE)
  field <- function(label) {
    index <- grep(label, trimws(lines))
    if (length(index) != 1L) {
      stop("Expected one source field: ", label, call. = FALSE)
    }
    index
  }
  numbers <- function(index) {
    scan(text = sub("#.*$", "", lines[index]), quiet = TRUE)
  }
  recruitment_heading <- field("^# Recruitment ")
  ssb_heading <- field("^# SSB ")
  recruitment_rows <- seq.int(recruitment_heading + 1L, ssb_heading - 1L)
  recruitment_rows <- recruitment_rows[nzchar(trimws(lines[recruitment_rows]))]
  recruitment <- lapply(recruitment_rows, numbers)
  nrec <- numbers(field("^# No Recruitments$") + 1L)
  if (
    !length(recruitment) %in% 1:2 ||
      any(lengths(recruitment) != nrec) ||
      (length(recruitment) == 2L &&
        !identical(recruitment[[1]], recruitment[[2]]))
  ) {
    stop(
      "Expected one per-sex recruitment series or two identical copies; review the source export.",
      call. = FALSE
    )
  }
  female_spawn <- field("^# wt spawn females$") + 1L
  male_spawn <- female_spawn + 1L
  female_fishery <- field("^# WtAge females by fishery$") + 1:2
  male_fishery <- field("^# WtAge males by fishery$") + 1:2
  abundance <- field("^# Numbers at age") + 1:2
  age_rows <- c(
    female_spawn,
    male_spawn,
    female_fishery,
    male_fishery,
    abundance
  )
  if (
    any(
      vapply(age_rows, function(index) length(numbers(index)), integer(1)) !=
        30L
    )
  ) {
    stop(
      "This example requires the supplied two-sex, two-fleet, 30-age layout.",
      call. = FALSE
    )
  }
  ssb <- numbers(ssb_heading + 1L)
  if (length(ssb) != nrec) {
    stop("Recruitment and SSB input lengths differ.", call. = FALSE)
  }
  list(
    lines = lines,
    numbers = numbers,
    recruitment_rows = recruitment_rows,
    recruitment = recruitment[[1]],
    ssb_heading = ssb_heading,
    ssb = ssb,
    female_spawn = female_spawn,
    male_spawn = male_spawn,
    female_fishery = do.call(rbind, lapply(female_fishery, numbers)),
    male_fishery = do.call(rbind, lapply(male_fishery, numbers)),
    female_abundance = numbers(abundance[1]),
    male_abundance = numbers(abundance[2]),
    female_maturity = numbers(field("^# Maturity females$") + 1L),
    male_maturity = numbers(field("^# Maturity males$") + 1L),
    fleet_fraction = numbers(field("# Fratio$"))
  )
}

gt2026_supported_controls <- function(controls) {
  isTRUE(
    controls$Tier == 3 &&
      controls$srr_form == 1 &&
      controls$tac_flag == 1 &&
      controls$beg_yr == 2026 &&
      identical(controls$datafile, "ForProjections.dat")
  )
}

check_gt2026_inputs <- function(source_dir = .gt2026_source_dir) {
  raw <- gt2026_read_source(source_dir)
  controls <- spmR::dat2list(file.path(source_dir, "spm.dat"))
  sidecar_path <- file.path(source_dir, "spm_input_v2.dat")
  sidecar <- if (file.exists(sidecar_path)) {
    readLines(sidecar_path, warn = FALSE)
  } else {
    character()
  }
  zeros <- sum(raw$numbers(raw$female_spawn) == 0) +
    sum(raw$numbers(raw$male_spawn) == 0)
  metadata_present <- file.exists(file.path(source_dir, "spm_metadata.json"))
  sidecar_incomplete <- !length(sidecar) || any(grepl("SET_", sidecar))
  all_one_maturity <- all(c(raw$female_maturity, raw$male_maturity) == 1)
  tibble::tibble(
    item = c(
      "Native metadata",
      "R metadata",
      "Recruitment rows",
      "Spawning weights",
      "Population weights",
      "Units and ages",
      "SSB history",
      "Maturity fields",
      "Controls",
      "TAC parameters"
    ),
    status = c(
      if (sidecar_incomplete) "missing" else "review",
      if (metadata_present) "review" else "missing",
      if (length(raw$recruitment_rows) == 2L) "repair" else "ok",
      if (zeros > 0) "repair" else "ok",
      if (metadata_present || !sidecar_incomplete) "review" else "missing",
      "review",
      "review",
      "info",
      if (gt2026_supported_controls(controls)) "ok" else "review",
      "info"
    ),
    detail = c(
      "spm_input_v2.dat must contain numeric declarations and two population-weight vectors; the runner generates it.",
      "The runner archives existing spm_metadata.json and writes staged metadata from explicit arguments before calling runSPM().",
      paste(
        length(raw$recruitment_rows),
        "copies of a", length(raw$recruitment),
        "value per-sex series; retain one. A second row would be read as SSB."
      ),
      paste(
        zeros,
        "zeros in effective spawning weights; stage them as 1e-12 for the current positive-weight contract."
      ),
      paste(
        "Population body weights require explicit runner arguments and SS3 verification.",
        "Diagnostic defaults use Fratio-weighted catch-weight proxies, with fleet fractions",
        paste(raw$fleet_fraction, collapse = ", ")
      ),
      "Thousand fish, kg, tonnes and ages 1:30/30+ are working assumptions requiring SS3 export verification.",
      paste(
        length(raw$ssb), "positions; source label:",
        trimws(raw$lines[raw$ssb_heading]),
        ". Calendar pairing remains unresolved; restrict to Tier 3, Rec_Gen=1."
      ),
      if (all_one_maturity) {
        "All-one maturity retains effective spawning weights. Ntot and SexRatio describe all fish and the all-fish female fraction. Confirm the SS3 spawning-weight definition."
      } else {
        "Maturity vectors contain values other than one; confirm that spawning weights and maturity avoid applying maturity twice."
      },
      paste0(
        controls$beg_yr, "-", controls$beg_yr + controls$nprj_yrs - 1L,
        "; ", controls$nalts, " alternatives; ", controls$nsims,
        " simulations. Adapter requires Tier 3, Rec_Gen=1, TAC_ABC=1,",
        " beginning year 2026 and ForProjections.dat."
      ),
      if (controls$tac_flag == 1) {
        "tacpar.dat is archived when supplied and unused with TAC_ABC=1."
      } else {
        "The configured TAC_ABC mode is outside this adapter's supported controls."
      }
    )
  )
}

run_gt2026_projection <- function(
  source_dir = .gt2026_source_dir,
  run_dir = NULL,
  diagnostic = FALSE,
  population_weights = NULL,
  units = NULL,
  ages = NULL,
  executable = "",
  admb = "",
  cxx = "",
  run = TRUE
) {
  source_dir <- normalizePath(source_dir, mustWork = TRUE)
  raw <- gt2026_read_source(source_dir)
  controls <- spmR::dat2list(file.path(source_dir, "spm.dat"))
  if (!gt2026_supported_controls(controls)) {
    stop(
      "This adapter requires Tier 3, Rec_Gen=1, TAC_ABC=1 and the supplied beginning-of-2026 controls.",
      call. = FALSE
    )
  }
  if (
    !diagnostic &&
      (is.null(population_weights) || is.null(units) || is.null(ages))
  ) {
    stop(
      "Supply population_weights, units and ages from the SS3 export, or explicitly use diagnostic = TRUE for the documented working assumptions.",
      call. = FALSE
    )
  }
  using_proxy <- is.null(population_weights)
  if (using_proxy) {
    population_weights <- list(
      female = as.numeric(crossprod(raw$fleet_fraction, raw$female_fishery)),
      male = as.numeric(crossprod(raw$fleet_fraction, raw$male_fishery))
    )
  }
  if (is.null(units)) {
    units <- list(abundance = "thousand_fish", weight = "kg", biomass = "t")
  }
  if (is.null(ages)) {
    ages <- 1:30
  }
  if (
    !is.list(population_weights) ||
      !setequal(names(population_weights), c("female", "male"))
  ) {
    stop(
      "population_weights must be a list with female and male numeric vectors.",
      call. = FALSE
    )
  }
  helpers <- new.env(parent = globalenv())
  sys.source(
    file.path(.gt2026_source_dir, "..", "gt20", "run_spmR_split_sex.R"),
    envir = helpers
  )
  if (is.null(run_dir)) {
    parent <- file.path(source_dir, "spmR_runs")
    dir.create(parent, showWarnings = FALSE)
    attempt <- 1L
    repeat {
      run_dir <- file.path(parent, sprintf("attempt-%03d", attempt))
      if (!file.exists(run_dir)) {
        break
      }
      attempt <- attempt + 1L
    }
  }
  if (file.exists(run_dir)) {
    stop(
      "Use a new run_dir to preserve earlier attempts: ",
      run_dir,
      call. = FALSE
    )
  }
  if (!dir.create(run_dir, recursive = TRUE)) {
    stop("Could not create run_dir.", call. = FALSE)
  }
  run_dir <- normalizePath(run_dir, mustWork = TRUE)
  source_archive <- file.path(run_dir, "source")
  dir.create(source_archive)
  supplied <- c(
    "spm.dat",
    "ForProjections.dat",
    intersect(
      c("spm_input_v2.dat", "spm_metadata.json", "tacpar.dat"),
      list.files(source_dir)
    )
  )
  source_paths <- file.path(source_dir, supplied)
  source_hashes <- vapply(
    source_paths,
    digest::digest,
    character(1),
    algo = "sha256",
    file = TRUE
  )
  if (!all(file.copy(source_paths, source_archive))) {
    stop("Could not archive source files.", call. = FALSE)
  }
  archive_hashes <- vapply(
    file.path(source_archive, supplied),
    digest::digest,
    character(1),
    algo = "sha256",
    file = TRUE
  )
  stopifnot(identical(unname(source_hashes), unname(archive_hashes)))
  stopifnot(file.copy(file.path(source_dir, "spm.dat"), run_dir))
  lines <- raw$lines
  for (index in c(raw$female_spawn, raw$male_spawn)) {
    values <- raw$numbers(index)
    values[values == 0] <- 1e-12
    lines[index] <- helpers$gt20_number_line(values)
  }
  lines[
    raw$ssb_heading
  ] <- "# SSB input positions; calendar years unresolved"
  if (length(raw$recruitment_rows) == 2L) {
    lines <- lines[-raw$recruitment_rows[2]]
  }
  writeLines(lines, file.path(run_dir, "ForProjections.dat"))
  spmR::write_spm_metadata(
    run_dir,
    list(list(
      file = "ForProjections.dat",
      recruitment_basis = "per_sex",
      ages = ages,
      units = units,
      population_weights = list(
        female = list(ages = ages, values = population_weights$female),
        male = list(ages = ages, values = population_weights$male)
      )
    ))
  )
  inputs <- spmR::validate_spm_inputs(run_dir)
  decisions <- list(
    status = "Conditional computational projection; SS3 export review remains open",
    diagnostic = diagnostic,
    population_weights = if (using_proxy) {
      "Fratio-weighted fishery proxies"
    } else {
      "Caller supplied"
    },
    units = units,
    ages = ages,
    terminal_age = "Plus group; confirm SS3 alignment",
    recruitment_basis = "per_sex; confirm SS3 recruitment export definition",
    recruitment_rows_removed = length(raw$recruitment_rows) - 1L,
    initial_state = "Beginning of 2026; confirm SS3 export timing",
    spawning_weights = "Retain supplied maturity multipliers and spawning weights; floor zero weights to 1e-12. Confirm SS3 spawning-weight definition.",
    ssb_history = paste(
      "Preserve", length(raw$ssb),
      "input positions. Calendar pairing unresolved; historical-recruitment mode only."
    ),
    population_review = "Require SS3 population body weights, units, age/plus-group alignment and export-field mapping for assessment use.",
    sex_ratio = if (all(c(raw$female_maturity, raw$male_maturity) == 1)) {
      "All-fish female fraction under the supplied all-one maturity fields"
    } else {
      "Maturity-weighted female fraction under the supplied maturity fields"
    },
    source_sha256 = as.list(stats::setNames(source_hashes, supplied))
  )
  jsonlite::write_json(
    decisions,
    file.path(run_dir, "staging-decisions.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  initial_biomass <- sum(
    raw$female_abundance *
      population_weights$female +
      raw$male_abundance * population_weights$male
  )
  initial_ssb <- sum(
    raw$female_abundance *
      inputs$species[[1]]$pmature_F *
      inputs$species[[1]]$wt_F
  )
  output <- list(
    run_dir = run_dir,
    inputs = inputs,
    initial_biomass = initial_biomass,
    initial_ssb = initial_ssb,
    result = NULL,
    summary = NULL,
    provenance = NULL
  )
  message(
    decisions$status,
    "; population weights: ",
    decisions$population_weights,
    "."
  )
  if (!run) {
    return(invisible(output))
  }
  executable <- helpers$gt20_find_executable(run_dir, executable)
  if (!nzchar(executable)) {
    helpers$gt20_build_executable(run_dir, admb, cxx)
  } else {
    helpers$gt20_stage_executable(executable, run_dir)
  }
  result <- spmR::runSPM(run_dir, run = TRUE, engine = "admb")
  control <- inputs$spm
  required <- c(
    "SSB",
    "Rec",
    "Tot_biom",
    "F",
    "Catch",
    "ABC",
    "OFL",
    "B100",
    "B40",
    "B35"
  )
  first <- result[result$Year == control$styr, ]
  near <- function(values, expected) {
    all(abs(values - expected) <= max(1e-5, abs(expected) * 6e-6))
  }
  stopifnot(
    nrow(result) == control$nalts * control$npro * control$nsims,
    !anyDuplicated(result[c("Alt", "Sim", "Year")]),
    setequal(unique(result$Alt), control$alt_list),
    setequal(unique(result$Sim), seq_len(control$nsims)),
    setequal(
      unique(result$Year),
      seq.int(control$styr, length.out = control$npro)
    ),
    all(is.finite(as.matrix(result[required]))),
    all(as.matrix(result[required]) >= 0),
    near(first$SSB, initial_ssb),
    near(first$Tot_biom, initial_biomass)
  )
  fixed <- result[result$Alt <= 5 & result$Year %in% control$Obs_Catch[, 1], ]
  stopifnot(all(
    abs(
      fixed$Catch -
        control$Obs_Catch[match(fixed$Year, control$Obs_Catch[, 1]), 2]
    ) <
      1e-6
  ))
  provenance <- jsonlite::read_json(
    file.path(run_dir, "spm_last_success_provenance.json"),
    simplifyVector = TRUE
  )
  stopifnot(
    provenance$status == "success",
    provenance$exit_status == 0L,
    identical(provenance$convergence$applicable, FALSE)
  )
  summary <- result |>
    dplyr::group_by(.data$Alt, .data$Year) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(c("SSB", "Tot_biom", "Catch", "ABC", "OFL")),
        mean
      ),
      .groups = "drop"
    )
  readr::write_csv(summary, file.path(run_dir, "annual-summary.csv"))
  output$result <- result
  output$summary <- summary
  output$provenance <- provenance
  message(
    "Verified ",
    nrow(result),
    " conditional projection rows in ",
    run_dir
  )
  invisible(output)
}

if (sys.nframe() == 0L) {
  arguments <- commandArgs(trailingOnly = TRUE)
  action <- if (length(arguments)) arguments[1] else "check"
  if (action == "check") {
    print(as.data.frame(check_gt2026_inputs()), row.names = FALSE)
  } else if (action == "diagnostic") {
    gt2026 <- run_gt2026_projection(diagnostic = TRUE)
    print(gt2026$summary[gt2026$summary$Year <= 2030, ], n = Inf)
  } else {
    stop(
      "Usage: Rscript examples/gt_2026/run.R [check|diagnostic]",
      call. = FALSE
    )
  }
}
