new_spm_adapter <- function(name, execute, read_output) {
  stopifnot(
    is.character(name),
    length(name) == 1,
    is.function(execute),
    is.function(read_output)
  )
  structure(
    list(name = name, execute = execute, read_output = read_output),
    class = "spm_adapter"
  )
}

spm_sha256 <- function(paths, root = NULL) {
  paths <- paths[file.exists(paths) & !dir.exists(paths)]
  values <- vapply(
    paths,
    digest::digest,
    character(1),
    algo = "sha256",
    file = TRUE
  )
  names(values) <- if (is.null(root)) {
    basename(paths)
  } else {
    substring(paths, nchar(root) + 2L)
  }
  as.list(values)
}

spm_execution_provenance <- function(dirname, metadata, strict) {
  parsed_files <- tryCatch(
    spm_read_legacy(dirname)$input_files,
    error = function(e) "spm.dat"
  )
  list(
    format_version = 2L,
    run_id = paste0(
      format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"),
      "-",
      basename(tempfile("run-"))
    ),
    status = "started",
    engine = "admb",
    strict = strict,
    started_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    package_version = as.character(utils::packageVersion("spmR")),
    input_hashes = spm_sha256(file.path(dirname, c(parsed_files, metadata))),
    executable_sha256 = NULL,
    mode = "version_2",
    species = NULL,
    output_hashes = list()
  )
}

spm_write_provenance <- function(value, dirname) {
  history <- file.path(dirname, "spm_run_history")
  dir.create(history, showWarnings = FALSE)
  atomic_json <- function(value, destination) {
    temporary <- tempfile(
      ".spm-provenance-",
      tmpdir = dirname,
      fileext = ".json"
    )
    on.exit(unlink(temporary), add = TRUE)
    jsonlite::write_json(
      value,
      temporary,
      pretty = TRUE,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    )
    if (!file.rename(temporary, destination)) {
      stop("Could not save projection provenance.", call. = FALSE)
    }
  }
  latest <- file.path(dirname, "spm_run_provenance.json")
  if (file.exists(latest)) {
    previous <- tryCatch(jsonlite::read_json(latest), error = function(e) NULL)
    if (!is.null(previous) && identical(previous$status, "success")) {
      previous_id <- if (is.null(previous$run_id)) {
        paste0("legacy-", digest::digest(latest, algo = "sha256", file = TRUE))
      } else {
        previous$run_id
      }
      archive <- file.path(history, paste0(previous_id, ".json"))
      if (!file.exists(archive) && !file.copy(latest, archive)) {
        stop("Could not preserve prior successful provenance.", call. = FALSE)
      }
    }
  }
  atomic_json(value, file.path(history, paste0(value$run_id, ".json")))
  if (identical(value$status, "success")) {
    atomic_json(value, file.path(dirname, "spm_last_success_provenance.json"))
  }
  atomic_json(value, latest)
}

spm_find_executable <- function(dirname) {
  candidate <- file.path(
    dirname,
    if (.Platform$OS.type == "windows") "spm.exe" else "spm"
  )
  if (!file.exists(candidate)) {
    candidate <- Sys.which("spm")
  }
  if (!nzchar(candidate) || !file.exists(candidate)) {
    stop(
      "Could not find an SPM executable in the run directory or on PATH.",
      call. = FALSE
    )
  }
  normalizePath(candidate, winslash = "/", mustWork = TRUE)
}

spm_call_executable <- function(executable, args, directory, stdout, stderr) {
  previous <- getwd()
  on.exit(setwd(previous), add = TRUE)
  setwd(directory)
  suppressWarnings(system2(
    executable,
    args = args,
    stdout = stdout,
    stderr = stderr
  ))
}

spm_verify_capabilities <- function(executable) {
  folder <- tempfile("spmr-capabilities-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  out <- file.path(folder, "stdout.log")
  err <- file.path(folder, "stderr.log")
  status <- spm_call_executable(
    executable,
    "-spmr-capabilities",
    folder,
    out,
    err
  )
  lines <- if (file.exists(out)) readLines(out, warn = FALSE) else character()
  if (status != 0L || !any(grepl("^SPMR_INPUT_FORMAT=2$", lines))) {
    stop(
      "SPM executable lacks input-format-2 support. Compile the package's current ",
      "inst/admb/spm.tpl; legacy binaries cannot consume explicit recruitment and population-weight metadata.",
      call. = FALSE
    )
  }
  list(
    exit_status = status,
    stdout = lines,
    stderr = if (file.exists(err)) readLines(err, warn = FALSE) else character()
  )
}

spm_verify_convergence <- function(folder, recruitment_mode) {
  if (recruitment_mode == 1) {
    return(list(
      applicable = FALSE,
      reason = "Standard historical recruitment simulations fit no parameters."
    ))
  }
  parameter_file <- file.path(folder, "spm.par")
  if (!file.exists(parameter_file)) {
    stop(
      "Stock-recruitment fit produced no spm.par diagnostics.",
      call. = FALSE
    )
  }
  header <- readLines(parameter_file, n = 1L, warn = FALSE)
  pattern <- "Maximum gradient component[[:space:]]*=[[:space:]]*([0-9.eE+\\-]+)"
  matched <- regmatches(header, regexec(pattern, header))[[1L]]
  gradient <- if (length(matched) == 2) {
    suppressWarnings(as.numeric(matched[2L]))
  } else {
    NA_real_
  }
  if (!is.finite(gradient) || abs(gradient) >= 1e-4) {
    stop(
      "Stock-recruitment fit requires a finite maximum gradient below 1e-4.",
      call. = FALSE
    )
  }
  hessian_file <- file.path(folder, "admodel.hes")
  if (!file.exists(hessian_file)) {
    stop(
      "Stock-recruitment fit produced no admodel.hes for the Hessian check.",
      call. = FALSE
    )
  }
  con <- file(hessian_file, "rb")
  on.exit(close(con), add = TRUE)
  dimension <- readBin(
    con,
    "integer",
    n = 1L,
    size = 4L,
    endian = .Platform$endian
  )
  if (length(dimension) != 1L || dimension < 1L || dimension > 10000L) {
    stop("Invalid ADMB Hessian dimensions.", call. = FALSE)
  }
  values <- readBin(
    con,
    "double",
    n = dimension * dimension,
    size = 8L,
    endian = .Platform$endian
  )
  if (length(values) != dimension * dimension || any(!is.finite(values))) {
    stop("Incomplete or nonfinite ADMB Hessian.", call. = FALSE)
  }
  hessian <- matrix(values, nrow = dimension, byrow = TRUE)
  tolerance <- 1e-8 * max(1, abs(hessian))
  positive <- max(abs(hessian - t(hessian))) <= tolerance &&
    !inherits(try(chol(hessian), silent = TRUE), "try-error")
  if (!positive) {
    stop(
      "Stock-recruitment Hessian must be symmetric and positive definite.",
      call. = FALSE
    )
  }
  list(
    applicable = TRUE,
    maximum_gradient = abs(gradient),
    threshold = 1e-4,
    hessian_positive_definite = TRUE,
    parameter_count = dimension,
    parameter_file = "spm.par",
    hessian_file = "admodel.hes"
  )
}

spm_verify_receipt <- function(path, inputs) {
  if (!file.exists(path)) {
    stop("SPM produced no input-format-2 receipt.", call. = FALSE)
  }
  receipt <- readr::read_tsv(path, show_col_types = FALSE)
  required <- c(
    "format_version",
    "stock_file",
    "nsexes",
    "nages",
    "recruitment_basis",
    "mean_recruitment_total",
    "mean_recruitment_female",
    "recruitment_cv",
    "abundance_unit",
    "weight_unit",
    "biomass_unit",
    "male_substitute"
  )
  if (!all(required %in% names(receipt)) || nrow(receipt) != inputs$spm$nspp) {
    stop(
      "SPM input receipt has unexpected fields or species count.",
      call. = FALSE
    )
  }
  for (i in seq_along(inputs$resolved_species)) {
    x <- inputs$resolved_species[[i]]
    raw <- inputs$species[[i]]
    basis <- match(x$recruitment_basis, c("total", "per_sex"))
    total <- raw$R * if (basis == 2) 2 else 1
    cv_squared <- mean(total) * mean(1 / total) - 1
    receipt_cv <- if (cv_squared < 1e-12) 0 else sqrt(cv_squared)
    expected <- c(
      format_version = 2,
      nsexes = x$nsexes,
      nages = x$nages,
      recruitment_basis = basis,
      mean_recruitment_total = mean(total),
      mean_recruitment_female = mean(total) / 2,
      recruitment_cv = receipt_cv,
      abundance_unit = match(
        x$units$abundance,
        c("fish", "thousand_fish", "million_fish")
      ),
      weight_unit = 1,
      biomass_unit = match(x$units$biomass, c("kg", "t", "thousand_t")),
      male_substitute = match(
        x$male_population_substitute,
        c("none", "female_population", "mean_male_fishery")
      ) -
        1L
    )
    actual <- unlist(receipt[i, names(expected)], use.names = FALSE)
    if (
      !identical(receipt$stock_file[i], x$file) ||
        !is.numeric(actual) ||
        any(!is.finite(actual)) ||
        any(abs(actual - expected) > 1e-8 * pmax(1, abs(expected)))
    ) {
      stop(
        "SPM receipt disagrees with declared inputs for ",
        x$file,
        ".",
        call. = FALSE
      )
    }
  }
  receipt
}

spm_verify_fresh_output <- function(folder, inputs, exit_status) {
  stdout <- readLines(file.path(folder, "spm_stdout.log"), warn = FALSE)
  stderr <- readLines(file.path(folder, "spm_stderr.log"), warn = FALSE)
  if (exit_status != 0L) {
    stop(
      "SPM execution failed with exit status ",
      exit_status,
      if (length(stderr)) paste0(": ", paste(stderr, collapse = " ")) else ".",
      call. = FALSE
    )
  }
  marker <- if (inputs$spm$Rec_Gen == 1) {
    "Finished simulations using standard (avg, var) stochastic approach"
  } else {
    "Finished simulations using stochastic stock-recruitment relationship"
  }
  if (!any(grepl(marker, stdout, fixed = TRUE))) {
    stop(
      "SPM did not report completion for the selected recruitment mode.",
      call. = FALSE
    )
  }
  file <- file.path(folder, "spm_detail.csv")
  if (!file.exists(file) || file.info(file)$size == 0) {
    stop(
      "SPM produced no fresh spm_detail.csv; existing results have been preserved.",
      call. = FALSE
    )
  }
  result <- as_spm_result(readr::read_csv(file, show_col_types = FALSE))
  expected <- inputs$spm$nspp *
    inputs$spm$nalts *
    inputs$spm$nsims *
    inputs$spm$npro
  if (nrow(result) != expected) {
    stop(
      "SPM output row count differs from the requested projection dimensions.",
      call. = FALSE
    )
  }
  columns <- c("SSB", "Rec", "Tot_biom", "F", "Catch", "ABC", "OFL")
  if (
    !all(columns %in% names(result)) ||
      any(!vapply(result[columns], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(result[columns]))) ||
      any(as.matrix(result[columns]) < 0)
  ) {
    stop(
      "SPM produced incomplete or invalid biomass, recruitment, catch, or F output.",
      call. = FALSE
    )
  }
  if (
    !setequal(result$Stock, vapply(inputs$species, `[[`, "", "spname")) ||
      !setequal(result$Alt, inputs$spm$alt_list) ||
      !setequal(result$Year, inputs$spm$styr + seq_len(inputs$spm$npro) - 1L) ||
      !setequal(result$Sim, seq_len(inputs$spm$nsims))
  ) {
    stop(
      "SPM output identifiers differ from requested stocks, alternatives, simulations, or years.",
      call. = FALSE
    )
  }
  receipt <- file.path(folder, "spm_input_receipt.tsv")
  verified_receipt <- spm_verify_receipt(receipt, inputs)
  convergence <- spm_verify_convergence(folder, inputs$spm$Rec_Gen)
  invisible(list(
    result = result,
    receipt = verified_receipt,
    convergence = convergence
  ))
}

spm_copy_output <- function(from, to) file.copy(from, to, overwrite = TRUE)

spm_publish_outputs <- function(folder, dirname, generated, provenance) {
  history <- file.path(dirname, "spm_run_history")
  dir.create(history, showWarnings = FALSE)
  backup <- tempfile("publication-backup-", tmpdir = history)
  dir.create(backup)
  keep_backup <- FALSE
  on.exit(if (!keep_backup) unlink(backup, recursive = TRUE), add = TRUE)
  canonical <- unique(c(
    generated,
    "spm_run_provenance.json",
    "spm_last_success_provenance.json"
  ))
  existed <- file.exists(file.path(dirname, canonical))
  for (file in canonical[existed]) {
    if (!file.copy(file.path(dirname, file), file.path(backup, file))) {
      stop(
        "Could not preserve previous output before publication: ",
        file,
        call. = FALSE
      )
    }
  }
  tryCatch(
    {
      for (file in generated) {
        if (
          !spm_copy_output(file.path(folder, file), file.path(dirname, file))
        ) {
          stop("Could not publish validated output: ", file, call. = FALSE)
        }
      }
      spm_write_provenance(provenance, dirname)
    },
    error = function(e) {
      failed <- character()
      for (i in seq_along(canonical)) {
        file <- canonical[i]
        if (existed[i]) {
          if (
            !file.copy(
              file.path(backup, file),
              file.path(dirname, file),
              overwrite = TRUE
            )
          ) {
            failed <- c(failed, file)
          }
        } else if (
          file.exists(file.path(dirname, file)) &&
            unlink(file.path(dirname, file)) != 0L
        ) {
          failed <- c(failed, file)
        }
      }
      if (length(failed)) {
        keep_backup <<- TRUE
        stop(
          conditionMessage(e),
          "; automatic restoration failed for ",
          paste(failed, collapse = ", "),
          ". Preserved previous files in ",
          backup,
          ".",
          call. = FALSE
        )
      }
      stop(
        conditionMessage(e),
        "; previous outputs and their successful provenance were restored.",
        call. = FALSE
      )
    }
  )
  invisible(NULL)
}

admb_adapter <- function(metadata = "spm_metadata.json", strict = TRUE) {
  execute <- function(dirname) {
    provenance <- spm_execution_provenance(dirname, metadata, strict)
    folder <- tempfile("spmr-run-")
    dir.create(folder)
    on.exit(unlink(folder, recursive = TRUE), add = TRUE)
    tryCatch(
      {
        inputs <- validate_spm_inputs(dirname, metadata, strict)
        provenance$input_hashes <- spm_sha256(file.path(
          dirname,
          inputs$input_files
        ))
        provenance$species <- inputs$resolved_species
        executable <- spm_find_executable(dirname)
        provenance$executable_sha256 <- digest::digest(
          executable,
          algo = "sha256",
          file = TRUE
        )
        provenance$executable_name <- basename(executable)
        provenance$capabilities <- spm_verify_capabilities(executable)
        for (input in inputs$input_files) {
          if (!file.copy(file.path(dirname, input), file.path(folder, input))) {
            stop("Could not stage input file: ", input, call. = FALSE)
          }
        }
        staged_executable <- file.path(
          folder,
          if (.Platform$OS.type == "windows") "spm.exe" else "spm"
        )
        if (!file.copy(executable, staged_executable)) {
          stop("Could not stage SPM executable.", call. = FALSE)
        }
        Sys.chmod(staged_executable, mode = "0755")
        spm_write_native_input(inputs, file.path(folder, "spm_input_v2.dat"))
        provenance$native_input_sha256 <- digest::digest(
          file.path(folder, "spm_input_v2.dat"),
          algo = "sha256",
          file = TRUE
        )
        status <- spm_call_executable(
          staged_executable,
          character(),
          folder,
          file.path(folder, "spm_stdout.log"),
          file.path(folder, "spm_stderr.log")
        )
        provenance$exit_status <- status
        verification <- spm_verify_fresh_output(folder, inputs, status)
        provenance$convergence <- verification$convergence
        provenance$input_receipt <- verification$receipt
        generated <- setdiff(
          list.files(folder),
          c(inputs$input_files, basename(staged_executable))
        )
        provenance$output_hashes <- spm_sha256(file.path(folder, generated))
        provenance$status <- "success"
        provenance$completed_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
        spm_publish_outputs(folder, dirname, generated, provenance)
        invisible(status)
      },
      error = function(e) {
        provenance$status <- "failure"
        provenance$error <- conditionMessage(e)
        provenance$completed_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
        generated <- setdiff(list.files(folder), c("spm", "spm.exe"))
        provenance$staged_file_hashes <- spm_sha256(file.path(
          folder,
          generated
        ))
        for (file in intersect(
          c("spm_stdout.log", "spm_stderr.log"),
          generated
        )) {
          file.copy(
            file.path(folder, file),
            file.path(dirname, paste0("failed_", file)),
            overwrite = TRUE
          )
        }
        spm_write_provenance(provenance, dirname)
        stop(conditionMessage(e), call. = FALSE)
      }
    )
  }
  read_output <- function(dirname, run) {
    readr::read_csv(
      file.path(dirname, "spm_detail.csv"),
      show_col_types = FALSE
    )
  }
  new_spm_adapter("admb", execute, read_output)
}

rtmb_adapter <- function() {
  execute <- function(dirname) {
    stop(
      "The experimental RTMB adapter does not implement validated version-2 scientific projections. Use engine='admb' with a version-2 executable.",
      call. = FALSE
    )
  }
  read_output <- function(dirname, run) {
    runSPM_rtmb(dirname = dirname, run = FALSE)
  }
  new_spm_adapter("rtmb", execute, read_output)
}

spm_adapter <- function(engine, metadata = "spm_metadata.json", strict = TRUE) {
  switch(
    engine,
    admb = admb_adapter(metadata, strict),
    rtmb = rtmb_adapter(),
    stop("Unknown model engine: ", engine, ".", call. = FALSE)
  )
}

run_spm_adapter <- function(adapter, dirname, run) {
  stopifnot(inherits(adapter, "spm_adapter"))
  if (run) {
    adapter$execute(dirname)
  }
  adapter$read_output(dirname, run = run) |> as_spm_result()
}
