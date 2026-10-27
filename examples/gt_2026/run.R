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
  recruitment_rows <- recruitment_rows[
    nzchar(trimws(sub("#.*$", "", lines[recruitment_rows])))
  ]
  nrec <- numbers(field("^# No Recruitments$") + 1L)
  if (
    length(nrec) != 1L || !is.finite(nrec) || nrec < 2 || nrec != trunc(nrec)
  ) {
    stop("No Recruitments must be one positive integer of at least two.", call. = FALSE)
  }
  recruitment_values <- unlist(lapply(recruitment_rows, numbers))
  recruitment_copies <- length(recruitment_values) / nrec
  if (
    !recruitment_copies %in% 1:2 ||
      (recruitment_copies == 2L &&
        !identical(recruitment_values[seq_len(nrec)], recruitment_values[nrec + seq_len(nrec)]))
  ) {
    stop(
      "Expected one recruitment series or two identical copies of No Recruitments values; line wrapping is allowed. Review the source export.",
      call. = FALSE
    )
  }
  declarations <- tolower(trimws(sub("#.*$", "", lines)))
  recruitment_declaration <- grep("^(per[ _-]+sex|total)$", declarations)
  inline_heading <- grep(
    "^[[:space:]]*#[[:space:]]*SPMR_INPUT_V2[[:space:]]+2[[:space:]]+1[[:space:]]*$",
    lines
  )
  inline_basis <- NULL
  if (length(inline_heading)) {
    if (length(inline_heading) != 1L || inline_heading >= length(lines)) {
      stop("Expected one complete inline metadata block.", call. = FALSE)
    }
    inline_header <- strsplit(
      trimws(sub("^[[:space:]]*#[[:space:]]*", "", lines[inline_heading + 1L])),
      "[[:space:]]+"
    )[[1]]
    if (length(inline_header) != 8L || !inline_header[4] %in% c("1", "2")) {
      stop("Invalid inline recruitment-basis declaration.", call. = FALSE)
    }
    inline_basis <- c("total", "per_sex")[as.integer(inline_header[4])]
  }
  if (length(recruitment_declaration) > 1L) {
    stop("Expected at most one recruitment-basis declaration.", call. = FALSE)
  }
  recruitment_basis <- if (length(recruitment_declaration)) {
    if (declarations[recruitment_declaration] == "total") "total" else "per_sex"
  } else if (!is.null(inline_basis)) {
    inline_basis
  } else {
    "per_sex"
  }
  if (!is.null(inline_basis) && recruitment_basis != inline_basis) {
    stop("Recruitment-basis declarations conflict.", call. = FALSE)
  }
  if (recruitment_copies == 2L && recruitment_basis != "per_sex") {
    stop("Two recruitment copies conflict with a total-recruitment basis.", call. = FALSE)
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
  ssb_rows <- if (ssb_heading < length(lines)) {
    seq.int(ssb_heading + 1L, length(lines))
  } else {
    integer()
  }
  ssb_rows <- ssb_rows[nzchar(trimws(sub("#.*$", "", lines[ssb_rows])))]
  ssb <- unlist(lapply(ssb_rows, numbers))
  list(
    lines = lines,
    numbers = numbers,
    recruitment_rows = recruitment_rows,
    recruitment = recruitment_values[seq_len(nrec)],
    recruitment_copies = recruitment_copies,
    recruitment_basis = recruitment_basis,
    recruitment_declaration = recruitment_declaration,
    recruitment_basis_declared = length(recruitment_declaration) > 0L || !is.null(inline_basis),
    ssb_heading = ssb_heading,
    ssb_rows = ssb_rows,
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
  inline_metadata_present <- any(grepl(
    "^[[:space:]]*#[[:space:]]*SPMR_INPUT_V2[[:space:]]+2[[:space:]]+1[[:space:]]*$",
    raw$lines
  ))
  sidecar_incomplete <- !length(sidecar) || any(grepl("SET_", sidecar))
  all_one_maturity <- all(c(raw$female_maturity, raw$male_maturity) == 1)
  tibble::tibble(
    item = c(
      "Native metadata",
      "R metadata",
      "Recruitment rows",
      "Recruitment basis",
      "Spawning weights",
      "Population weights",
      "Units and ages",
      "SSB history",
      "Maturity fields",
      "Controls",
      "TAC parameters"
    ),
    status = c(
      if (inline_metadata_present || !sidecar_incomplete) "review" else "info",
      if (metadata_present) "review" else "info",
      if (raw$recruitment_copies == 2L) "repair" else "ok",
      if (raw$recruitment_basis_declared) "ok" else "review",
      if (zeros > 0) "repair" else "ok",
      if (metadata_present || inline_metadata_present || !sidecar_incomplete) "review" else "missing",
      "review",
      if (length(raw$ssb) == length(raw$recruitment)) "review" else "missing",
      "info",
      if (gt2026_supported_controls(controls)) "ok" else "review",
      "info"
    ),
    detail = c(
      "A separate spm_input_v2.dat is optional; the runner embeds validated declarations in the staged species file.",
      "A separate spm_metadata.json is optional. Existing files are archived; staged declarations come from explicit arguments or diagnostic defaults.",
      paste(
        raw$recruitment_copies,
        "copies of a", length(raw$recruitment),
        "value series across", length(raw$recruitment_rows),
        "lines; retain one complete series. A second copy would be read as SSB."
      ),
      if (raw$recruitment_basis_declared) {
        paste("Source explicitly declares", raw$recruitment_basis, "recruitment.")
      } else {
        "Working per_sex basis requires confirmation; add a per sex or total declaration."
      },
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
      if (length(raw$ssb) != length(raw$recruitment)) {
        paste0(
          length(raw$recruitment), " recruitment values but ", length(raw$ssb),
          " SSB values. Supply matching source histories before staging; values are never padded or discarded."
        )
      } else {
        paste(
          length(raw$ssb), "positions; source label:",
          trimws(raw$lines[raw$ssb_heading]),
          ". Calendar pairing remains unresolved; restrict to Tier 3, Rec_Gen=1."
        )
      },
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
  if (length(raw$ssb) != length(raw$recruitment)) {
    stop(
      length(raw$recruitment), " recruitment values but ", length(raw$ssb),
      " SSB values. Supply matching source histories before staging; values are never padded or discarded.",
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
  lines[raw$recruitment_rows[1]] <- helpers$gt20_number_line(raw$recruitment)
  removed_lines <- c(raw$recruitment_rows[-1], raw$recruitment_declaration)
  if (length(removed_lines)) lines <- lines[-removed_lines]
  writeLines(lines, file.path(run_dir, "ForProjections.dat"))
  spmR::write_spm_metadata(
    run_dir,
    list(list(
      file = "ForProjections.dat",
      recruitment_basis = raw$recruitment_basis,
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
    recruitment_basis = raw$recruitment_basis,
    recruitment_copies_removed = raw$recruitment_copies - 1L,
    recruitment_lines_removed = length(raw$recruitment_rows) - 1L,
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
