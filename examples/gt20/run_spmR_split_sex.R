# Greenland turbot 2020 split-sex projection
#
# Purpose
# -------
# Convert the archived two-sex Greenland turbot input to spmR input format 2,
# validate the scientific declarations, and optionally run the ADMB projection.
# The source example retains only data/Model_Proj.dat; all current inputs,
# executable files, results, and provenance are generated in `run_dir`.
#
# Requirements
# ------------
# - spmR >= 0.4.1
# - ADMB >= 13 and a matching C++ toolchain when `build = TRUE`
#
# Usage
# -----
# source("examples/gt20/run_spmR_split_sex.R")
# gt20 <- run_gt20_projection()
# gt20$summary
#
# To validate without compiling or running ADMB:
# gt20 <- run_gt20_projection(run = FALSE)
#
# A compatible executable can be passed with `executable` or declared through
# `SPMR_EXECUTABLE` (the native test suite also supports `SPMR_TEST_EXE`). To
# compile automatically, put `admb` on PATH or declare it with `SPMR_ADMB`.
# `SPMR_CXX` can select a compiler compatible with the installed ADMB libraries
# when several toolchains are installed. Running this file directly with
# Rscript calls `run_gt20_projection()`.
#
# Scientific assumptions
# ----------------------
# Recruitment is total across sexes; ages are 1:30 with age 30 a plus group;
# abundance, weight, and biomass use thousand fish, kg, and t. The archived
# female spawning weights are reused as population weights to preserve the
# legacy biomass convention. Because the archived input has no independent
# male population or spawning weights, the unweighted
# mean of its two male fishery-weight vectors is the explicit recorded
# substitute. The two zero female weights are replaced by machine epsilon to
# satisfy the positive-weight format-2 contract.

if (!requireNamespace("spmR", quietly = TRUE) ||
    utils::packageVersion("spmR") < "0.4.1") {
  stop("Install spmR >= 0.4.1 before running this example.", call. = FALSE)
}

gt20_locate_source <- function() {
  frame_files <- vapply(rev(sys.frames()), function(frame) {
    if (is.null(frame$ofile)) "" else as.character(frame$ofile)[1]
  }, character(1))
  command_file <- sub(
    "^--file=", "",
    grep("^--file=", commandArgs(FALSE), value = TRUE)
  )
  candidates <- unique(c(command_file, frame_files[nzchar(frame_files)]))
  candidates <- candidates[
    basename(candidates) == "run_spmR_split_sex.R" & file.exists(candidates)
  ]
  if (length(candidates)) {
    return(dirname(normalizePath(candidates[1], mustWork = TRUE)))
  }

  candidates <- c(file.path(getwd(), "examples", "gt20"), getwd())
  candidates <- candidates[file.exists(file.path(
    candidates, "data", "Model_Proj.dat"
  ))]
  if (!length(candidates)) {
    stop(
      "Could not locate examples/gt20; supply `source_dir` explicitly.",
      call. = FALSE
    )
  }
  normalizePath(candidates[1], mustWork = TRUE)
}

gt20_number_line <- function(x) {
  paste(format(x, digits = 17, scientific = TRUE, trim = TRUE), collapse = " ")
}

gt20_write_fields <- function(x, path) {
  lines <- unlist(Map(function(name, value) {
    c(paste("#", name), paste(value, collapse = " "))
  }, names(x), x), use.names = FALSE)
  writeLines(lines, path)
}

gt20_muffle_declared_substitution <- function(expr) {
  withCallingHandlers(expr, warning = function(warning) {
    if (grepl(
      "male population weights use declared mean_male_fishery",
      conditionMessage(warning), fixed = TRUE
    )) {
      invokeRestart("muffleWarning")
    }
  })
}

gt20_executable_name <- function() {
  if (.Platform$OS.type == "windows") "spm.exe" else "spm"
}

gt20_command_path <- function(command, label) {
  if (!nzchar(command)) return("")
  if (file.exists(command)) {
    return(normalizePath(command, winslash = "/", mustWork = TRUE))
  }
  resolved <- unname(Sys.which(command))
  if (!nzchar(resolved)) {
    stop(label, " does not exist and is not on PATH: ", command, call. = FALSE)
  }
  normalizePath(resolved, winslash = "/", mustWork = TRUE)
}

gt20_verify_executable <- function(executable) {
  executable <- gt20_command_path(executable, "SPM executable")
  output <- suppressWarnings(system2(
    executable, "-spmr-capabilities", stdout = TRUE, stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L || !any(trimws(output) == "SPMR_INPUT_FORMAT=2")) {
    stop(
      "The SPM executable is not compatible with input format 2: ",
      executable,
      call. = FALSE
    )
  }
  executable
}

gt20_find_executable <- function(run_dir, executable = "") {
  configured <- c(
    executable,
    Sys.getenv("SPMR_EXECUTABLE", unset = ""),
    Sys.getenv("SPMR_TEST_EXE", unset = "")
  )
  configured <- configured[nzchar(configured)]
  if (length(configured)) {
    return(gt20_verify_executable(configured[1]))
  }

  candidates <- c(
    file.path(run_dir, gt20_executable_name()),
    unname(Sys.which("spm"))
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(candidates)) return("")
  for (candidate in unique(candidates)) {
    verified <- tryCatch(
      gt20_verify_executable(candidate),
      error = function(...) ""
    )
    if (nzchar(verified)) return(verified)
  }
  ""
}

gt20_stage_executable <- function(executable, run_dir) {
  destination <- file.path(run_dir, gt20_executable_name())
  source_path <- normalizePath(executable, winslash = "/", mustWork = TRUE)
  destination_path <- normalizePath(
    destination, winslash = "/", mustWork = FALSE
  )
  same_path <- if (.Platform$OS.type == "windows") {
    identical(tolower(source_path), tolower(destination_path))
  } else {
    identical(source_path, destination_path)
  }
  if (!same_path) {
    if (!file.copy(source_path, destination, overwrite = TRUE, copy.mode = TRUE)) {
      stop("Could not copy the SPM executable into ", run_dir, call. = FALSE)
    }
  }
  Sys.chmod(destination, mode = "0755")
  gt20_verify_executable(destination)
}

gt20_build_executable <- function(run_dir, admb = "", cxx = "") {
  if (!nzchar(admb)) admb <- Sys.getenv("SPMR_ADMB", unset = "")
  if (!nzchar(admb)) admb <- unname(Sys.which("admb"))
  if (!nzchar(admb)) {
    stop(
      "No format-2 SPM executable or ADMB compiler was found. Supply ",
      "`executable`, set SPMR_EXECUTABLE, put `spm` on PATH, or install ",
      "ADMB >= 13 and set SPMR_ADMB.",
      call. = FALSE
    )
  }
  admb <- gt20_command_path(admb, "ADMB compiler")

  if (!nzchar(cxx)) cxx <- Sys.getenv("SPMR_CXX", unset = "")
  if (!nzchar(cxx) && .Platform$OS.type == "windows") {
    admb_roots <- unique(c(dirname(admb), dirname(dirname(admb))))
    bundled <- file.path(
      admb_roots, "utilities", "mingw", "bin", "g++.exe"
    )
    bundled <- bundled[file.exists(bundled)]
    if (length(bundled)) cxx <- bundled[1]
  }
  if (nzchar(cxx)) {
    cxx <- gt20_command_path(cxx, "C++ compiler")
    previous_path <- Sys.getenv("PATH")
    previous_cxx <- Sys.getenv("CXX", unset = NA_character_)
    on.exit({
      Sys.setenv(PATH = previous_path)
      if (is.na(previous_cxx)) {
        Sys.unsetenv("CXX")
      } else {
        Sys.setenv(CXX = previous_cxx)
      }
    }, add = TRUE)
    Sys.setenv(
      PATH = paste(dirname(cxx), previous_path, sep = .Platform$path.sep),
      CXX = tools::file_path_sans_ext(basename(cxx))
    )
  }

  build_dir <- tempfile("spmr-gt20-build-")
  if (!dir.create(build_dir)) {
    stop("Could not create the temporary ADMB build directory.", call. = FALSE)
  }
  previous <- getwd()
  on.exit({
    setwd(previous)
    unlink(build_dir, recursive = TRUE)
  }, add = TRUE)

  admb_dir <- system.file("admb", package = "spmR", mustWork = TRUE)
  source <- file.path(admb_dir, "spm.tpl")
  if (!file.copy(source, build_dir, overwrite = TRUE)) {
    stop("Could not stage the installed ADMB template.", call. = FALSE)
  }

  setwd(build_dir)
  output <- suppressWarnings(system2(
    admb, c("-f", "spm.tpl"), stdout = TRUE, stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  built <- file.path(build_dir, gt20_executable_name())
  if (status != 0L || !file.exists(built)) {
    hint <- if (.Platform$OS.type == "windows") {
      paste0(
        "\nOn Windows, set `cxx` or SPMR_CXX to a g++.exe compatible ",
        "with the installed ADMB libraries."
      )
    } else {
      ""
    }
    stop(
      "ADMB compilation did not produce ", gt20_executable_name(), ".\n",
      paste(output, collapse = "\n"), hint,
      call. = FALSE
    )
  }

  gt20_verify_executable(built)
  gt20_stage_executable(built, run_dir)
}

#' Run or validate the 2020 Greenland turbot projection example.
#'
#' @param source_dir Directory containing this script and data/Model_Proj.dat;
#'   `NULL` discovers it from a source checkout.
#' @param run_dir Writable directory for converted inputs and generated results;
#'   `NULL` uses SPMR_GT20_RUN_DIR or `spmR_run` below `source_dir`.
#' @param executable Optional compatible `spm` or `spm.exe` path.
#' @param admb Optional ADMB compiler path or command used when building.
#' @param cxx Optional C++ compiler path compatible with the ADMB libraries.
#' @param build Build the engine when `run = TRUE` and no executable is found.
#' @param run Execute ADMB; use `FALSE` to only create and validate inputs.
#'
#' @return Invisibly, a list containing `run_dir`, validated `inputs`,
#'   `initial_biomass`, and, after execution, `result`, `summary`, and
#'   `provenance`.
run_gt20_projection <- function(
    source_dir = NULL,
    run_dir = NULL,
    executable = "",
    admb = "",
    cxx = "",
    build = TRUE,
    run = TRUE) {
  if (is.null(source_dir)) source_dir <- gt20_locate_source()
  source_dir <- normalizePath(source_dir, mustWork = TRUE)
  if (is.null(run_dir)) {
    run_dir <- Sys.getenv(
      "SPMR_GT20_RUN_DIR", file.path(source_dir, "spmR_run")
    )
  }
  source_file <- file.path(source_dir, "data", "Model_Proj.dat")
  if (!file.exists(source_file)) {
    stop("Missing archived input: ", source_file, call. = FALSE)
  }
  if (!dir.exists(run_dir) &&
      !dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create output directory: ", run_dir, call. = FALSE)
  }
  run_dir <- normalizePath(run_dir, mustWork = TRUE)

  # The current control file consolidates values formerly spread across three
  # legacy control files. Tier 3 encodes the assessment's Tier 3a status; the
  # control rule selects the `a` condition from projected stock status.
  gt20_write_fields(list(
    run_name = "std", Tier = 3, nalts = 7, alt_list = 1:7,
    TAC_ABC = 1, SrType = 1, Rec_Gen = 1, Fmsy_F35 = 1,
    Rec_Cond = 0, Write_Big = 1, npro = 20, nsims = 100, styr = 2020,
    nyrs_catch_in = 1, nspp = 1, OY_min = 43.248, OY_max = 1943.248,
    spp_file_name = "Model_Proj.prj", ABC_Multiplier = 1, N_scalar = 1,
    Alt4_SPR = 0.75, ntacspp = 1, tac_ind = 1,
    Obs_Catch = c(2020, 3321.27)
  ), file.path(run_dir, "spm.dat"))

  legacy <- readLines(source_file, warn = FALSE)
  find_field <- function(label) {
    index <- which(trimws(legacy) == label)
    if (length(index) != 1L) stop("Could not locate ", label, call. = FALSE)
    index
  }
  numbers <- function(x) scan(text = sub("#.*$", "", x), quiet = TRUE)
  ngear <- as.integer(numbers(legacy[
    grep("Number of fisheries", legacy, fixed = TRUE)[1]
  ]))
  nages <- as.integer(numbers(legacy[
    grep("number of ages", legacy, fixed = TRUE)[1]
  ]))
  nrec <- as.integer(numbers(legacy[find_field("# No Recruitments") + 1]))
  if (ngear != 2L || nages != 30L) {
    stop("GT20 must contain two fisheries and ages 1-30.", call. = FALSE)
  }

  female_spawn_i <- find_field("# wt spawn females") + 1
  female_spawn_wt <- numbers(legacy[female_spawn_i])
  if (length(female_spawn_wt) != nages || sum(female_spawn_wt <= 0) != 2L) {
    stop("Expected 30 female weights with two legacy zeros.", call. = FALSE)
  }
  female_spawn_wt[female_spawn_wt <= 0] <- .Machine$double.eps
  legacy[female_spawn_i] <- gt20_number_line(female_spawn_wt)

  male_gear_i <- find_field("# WtAge males by fishery")
  male_gear_wt <- do.call(rbind, lapply(
    legacy[male_gear_i + seq_len(ngear)], numbers
  ))
  if (!identical(dim(male_gear_wt), c(ngear, nages))) {
    stop("Male fishery weights do not match fisheries and ages.", call. = FALSE)
  }
  male_spawn_wt <- colMeans(male_gear_wt)

  ssb_i <- grep("^# SSB", trimws(legacy)) + 1
  ssb <- numbers(legacy[ssb_i])
  if (length(ssb) != nrec + 2L) {
    stop(
      "Expected 41 paired and two trailing SSB values in legacy GT20.",
      call. = FALSE
    )
  }
  legacy[ssb_i] <- gt20_number_line(ssb[seq_len(nrec)])
  legacy <- append(
    legacy,
    c("# wt spawn males", gt20_number_line(male_spawn_wt)),
    after = female_spawn_i
  )
  species_file <- file.path(run_dir, "Model_Proj.prj")
  writeLines(legacy, species_file)

  ages <- seq_len(nages)
  gt20_muffle_declared_substitution(spmR::write_spm_metadata(
    run_dir, list(list(
      file = basename(species_file),
      recruitment_basis = "total",
      ages = ages,
      units = list(
        abundance = "thousand_fish", weight = "kg", biomass = "t"
      ),
      population_weights = list(
        female = list(ages = ages, values = female_spawn_wt)
      ),
      male_population_substitute = "mean_male_fishery"
    ))
  ))
  inputs <- gt20_muffle_declared_substitution(
    spmR::validate_spm_inputs(run_dir)
  )
  warning(
    "GT20 uses declared mean_male_fishery population weights for males.",
    call. = FALSE
  )
  stock <- inputs$species[[1]]
  weights <- inputs$resolved_species[[1]]$population_weights
  initial_biomass <- sum(
    stock$n0_F * weights$female + stock$n0_M * weights$male
  )
  if (stock$nsexes != 2L ||
      !isTRUE(all.equal(initial_biomass, 92104.6136543, tolerance = 1e-10))) {
    stop("Converted inputs do not reproduce the GT20 baseline.", call. = FALSE)
  }

  output <- list(
    run_dir = run_dir,
    inputs = inputs,
    initial_biomass = initial_biomass,
    result = NULL,
    summary = NULL,
    provenance = NULL
  )
  if (!isTRUE(run)) return(invisible(output))

  executable <- gt20_find_executable(run_dir, executable)
  if (!nzchar(executable)) {
    if (!isTRUE(build)) {
      stop(
        "No format-2 SPM executable was found. Supply `executable`, set ",
        "SPMR_EXECUTABLE, or use `build = TRUE` with ADMB >= 13.",
        call. = FALSE
      )
    }
    executable <- gt20_build_executable(run_dir, admb, cxx)
  } else {
    executable <- gt20_stage_executable(executable, run_dir)
  }

  result <- gt20_muffle_declared_substitution(
    spmR::runSPM(run_dir, run = TRUE, engine = "admb")
  )
  if (nrow(result) != 14000L ||
      !setequal(unique(result$Alt), 1:7) ||
      !setequal(unique(result$Sim), 1:100) ||
      !identical(range(result$Year), c(2020, 2039))) {
    stop("Projection output dimensions do not match GT20 controls.", call. = FALSE)
  }
  summary <- stats::aggregate(
    result[c("SSB", "Catch", "ABC", "OFL")],
    result[c("Alt", "Year")],
    mean
  )
  benchmark <- summary[summary$Alt == 1 & summary$Year == 2021, ]
  if (nrow(benchmark) != 1L ||
      abs(benchmark$SSB - 51913.9) > 0.1 ||
      abs(benchmark$ABC - 7326.4) > 0.1) {
    stop("Projection does not reproduce the published 2021 benchmark.",
         call. = FALSE)
  }

  provenance <- jsonlite::read_json(
    file.path(run_dir, "spm_last_success_provenance.json"),
    simplifyVector = FALSE
  )
  if (!identical(provenance$status, "success") ||
      !identical(provenance$mode, "version_2") ||
      !identical(provenance$species[[1]]$recruitment_basis, "total")) {
    stop("Successful format-2 provenance was not recorded.", call. = FALSE)
  }

  output$result <- result
  output$summary <- summary
  output$provenance <- provenance
  message(
    "Completed GT20 projection in ", run_dir,
    "; starting biomass = ", round(initial_biomass, 1), " t."
  )
  invisible(output)
}

if (sys.nframe() == 0L) {
  gt20 <- run_gt20_projection()
  print(gt20$summary[gt20$summary$Alt == 1 & gt20$summary$Year <= 2022, ])
}
