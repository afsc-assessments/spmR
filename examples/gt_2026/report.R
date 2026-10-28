gt2026_report_hashes <- function(hashes, directory, label, required) {
  if (
    !is.list(hashes) ||
      !length(hashes) ||
      is.null(names(hashes)) ||
      anyDuplicated(names(hashes)) ||
      !all(required %in% names(hashes)) ||
      any(!nzchar(names(hashes))) ||
      any(basename(names(hashes)) != names(hashes)) ||
      !all(vapply(
        hashes,
        function(value) {
          is.character(value) &&
            length(value) == 1L &&
            !is.na(value) &&
            grepl("^[[:xdigit:]]{64}$", value)
        },
        logical(1)
      ))
  ) {
    stop("Missing or invalid ", label, " hash map.", call. = FALSE)
  }
  for (filename in names(hashes)) {
    path <- file.path(directory, filename)
    if (!file.exists(path) || dir.exists(path)) {
      stop("Missing ", label, " file: ", filename, call. = FALSE)
    }
    actual <- digest::digest(path, file = TRUE, algo = "sha256")
    if (!identical(actual, tolower(hashes[[filename]]))) {
      stop("Hash mismatch for ", label, ": ", filename, call. = FALSE)
    }
  }
  invisible(hashes)
}

gt2026_read_report_run <- function(run_dir, source_dir) {
  if (
    !is.character(run_dir) ||
      length(run_dir) != 1L ||
      is.na(run_dir) ||
      !nzchar(run_dir) ||
      !dir.exists(run_dir)
  ) {
    stop(
      "Select an existing saved attempt directory explicitly.",
      call. = FALSE
    )
  }
  run_dir <- normalizePath(run_dir, mustWork = TRUE)
  source_dir <- normalizePath(source_dir, mustWork = TRUE)
  required <- c("staging-decisions.json", "spm_last_success_provenance.json")
  if (!all(file.exists(file.path(run_dir, required)))) {
    stop(
      "Saved attempt requires staging decisions and successful provenance.",
      call. = FALSE
    )
  }
  decisions <- jsonlite::read_json(
    file.path(run_dir, required[1]),
    simplifyVector = FALSE
  )
  provenance <- jsonlite::read_json(
    file.path(run_dir, required[2]),
    simplifyVector = FALSE
  )
  scalar_text <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }
  if (
    !identical(provenance$status, "success") ||
      !identical(provenance$engine, "admb") ||
      !isTRUE(provenance$exit_status == 0) ||
      !all(vapply(
        list(
          provenance$run_id,
          provenance$completed_utc,
          provenance$package_version,
          provenance$executable_sha256,
          decisions$status,
          decisions$population_weights
        ),
        scalar_text,
        logical(1)
      )) ||
      !grepl("^[[:xdigit:]]{64}$", provenance$executable_sha256)
  ) {
    stop(
      "Saved attempt lacks completed, successful ADMB provenance.",
      call. = FALSE
    )
  }
  gt2026_report_hashes(
    provenance$input_hashes,
    run_dir,
    "staged inputs",
    "spm.dat"
  )
  gt2026_report_hashes(
    provenance$output_hashes,
    run_dir,
    "outputs",
    c(
      "spm_detail.csv",
      "spm_stdout.log",
      "spm_stderr.log",
      "spm_input_receipt.tsv"
    )
  )
  gt2026_report_hashes(
    decisions$source_sha256,
    file.path(run_dir, "source"),
    "archived source",
    c("spm.dat", "ForProjections.dat")
  )
  controls <- spmR::validate_spm_inputs(run_dir)$spm
  dimensions <- unlist(controls[c(
    "nspp",
    "nalts",
    "nsims",
    "npro",
    "styr",
    "Rec_Gen"
  )])
  if (
    length(dimensions) != 6L ||
      any(!is.finite(dimensions)) ||
      any(dimensions < 1 | dimensions != floor(dimensions)) ||
      controls$nspp != 1L ||
      controls$Rec_Gen != 1L ||
      length(controls$alt_list) != controls$nalts ||
      anyDuplicated(controls$alt_list) ||
      length(controls$spp_files) != 1L ||
      !all(controls$spp_files %in% names(provenance$input_hashes))
  ) {
    stop(
      "Saved attempt has incomplete or unsupported projection controls.",
      call. = FALSE
    )
  }
  if (
    length(provenance$species) != 1L ||
      !identical(provenance$species[[1]]$file, controls$spp_files) ||
      !scalar_text(provenance$species[[1]]$stock)
  ) {
    stop(
      "Saved species declaration differs from the projection controls.",
      call. = FALSE
    )
  }
  biomass_units <- provenance$species[[1]]$units$biomass
  if (
    !is.character(biomass_units) ||
      length(biomass_units) != 1L ||
      is.na(biomass_units) ||
      !nzchar(biomass_units)
  ) {
    stop("Saved attempt lacks declared biomass units.", call. = FALSE)
  }
  marker <- "Finished simulations using standard (avg, var) stochastic approach"
  diagnostic_valid <- identical(provenance$convergence$applicable, FALSE)
  if (
    !diagnostic_valid ||
      !any(grepl(
        marker,
        readLines(file.path(run_dir, "spm_stdout.log"), warn = FALSE),
        fixed = TRUE
      ))
  ) {
    stop(
      "Saved attempt lacks verified completion or applicable fit diagnostics.",
      call. = FALSE
    )
  }
  result <- spmR::runSPM(run_dir, run = FALSE, engine = "admb")
  metrics <- c(
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
  if (
    !all(c("Alt", metrics) %in% names(result)) ||
      !all(vapply(result[metrics], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(result[metrics]))) ||
      any(as.matrix(result[metrics]) < 0) ||
      nrow(result) != controls$nalts * controls$nsims * controls$npro ||
      anyDuplicated(result[c("Stock", "Alt", "Sim", "Year")]) ||
      !setequal(result$Stock, provenance$species[[1]]$stock) ||
      !setequal(result$Alt, controls$alt_list) ||
      !setequal(result$Sim, seq_len(controls$nsims)) ||
      !setequal(result$Year, seq.int(controls$styr, length.out = controls$npro))
  ) {
    stop(
      "Saved detail output differs from the declared projection dimensions or valid values.",
      call. = FALSE
    )
  }
  source_comparison <- tibble::tibble(
    file = names(decisions$source_sha256),
    matches_current = vapply(
      names(decisions$source_sha256),
      function(filename) {
        path <- file.path(source_dir, filename)
        file.exists(path) &&
          !dir.exists(path) &&
          identical(
            digest::digest(path, file = TRUE, algo = "sha256"),
            tolower(decisions$source_sha256[[filename]])
          )
      },
      logical(1),
      USE.NAMES = FALSE
    )
  )
  list(
    result = result,
    provenance = provenance,
    decisions = decisions,
    controls = controls,
    source_comparison = source_comparison,
    biomass_units = biomass_units
  )
}

gt2026_report_series <- function(result, n_worms = 15L) {
  result <- spmR::as_spm_result(result)
  metrics <- c("SSB", "Tot_biom", "Catch", "ABC", "OFL")
  if (
    length(n_worms) != 1L ||
      !is.numeric(n_worms) ||
      is.na(n_worms) ||
      !is.finite(n_worms) ||
      n_worms < 1L ||
      n_worms != floor(n_worms)
  ) {
    stop("n_worms must be a positive integer.", call. = FALSE)
  }
  if (
    length(unique(result$Stock)) != 1L ||
      !"Alt" %in% names(result) ||
      !all(metrics %in% names(result)) ||
      !all(vapply(result[metrics], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(result[metrics]))) ||
      any(as.matrix(result[metrics]) < 0)
  ) {
    stop(
      "Report series require one stock with finite nonnegative projection metrics.",
      call. = FALSE
    )
  }
  simulation_ids <- sort(unique(result$Sim))
  if (length(simulation_ids) < n_worms) {
    stop(
      "Requested ",
      n_worms,
      " worms but only ",
      length(simulation_ids),
      " simulation IDs are available.",
      call. = FALSE
    )
  }
  if (
    !all(vapply(
      split(result$Sim, interaction(result$Alt, result$Year, drop = TRUE)),
      function(values) setequal(values, simulation_ids),
      logical(1)
    ))
  ) {
    stop(
      "Every alternative and year must contain the same simulation IDs.",
      call. = FALSE
    )
  }
  long <- result |>
    dplyr::select(dplyr::all_of(c("Alt", "Sim", "Year", metrics))) |>
    tidyr::pivot_longer(
      dplyr::all_of(metrics),
      names_to = "metric",
      values_to = "value"
    )
  summary <- long |>
    dplyr::group_by(.data$Alt, .data$Year, .data$metric) |>
    dplyr::summarise(
      mean = mean(.data$value),
      median = stats::median(.data$value),
      lower = unname(stats::quantile(.data$value, 0.05)),
      upper = unname(stats::quantile(.data$value, 0.95)),
      .groups = "drop"
    )
  worm_ids <- head(simulation_ids, n_worms)
  worms <- long |>
    dplyr::filter(.data$Sim %in% worm_ids) |>
    dplyr::arrange(.data$Alt, .data$Sim, .data$Year, .data$metric)
  list(summary = summary, worms = worms, worm_ids = worm_ids)
}
