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

admb_adapter <- function() {
  execute <- function(dirname) {
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(dirname)

    executable <- if (.Platform$OS.type == "windows") "spm.exe" else "./spm"
    if (!file.exists(executable)) {
      executable <- Sys.which("spm")
    }
    if (!nzchar(executable)) {
      stop(
        "Could not find an SPM executable in `",
        dirname,
        "` or on PATH.",
        call. = FALSE
      )
    }

    status <- system2(executable)
    if (!identical(status, 0L)) {
      stop("SPM execution failed with exit status ", status, ".", call. = FALSE)
    }
    invisible(status)
  }

  read_output <- function(dirname, run) {
    readr::read_csv(file.path(dirname, "spm_detail.csv"), show_col_types = FALSE)
  }

  new_spm_adapter("admb", execute, read_output)
}

rtmb_adapter <- function() {
  execute <- function(dirname) {
    invisible(runSPM_rtmb(dirname = dirname, run = TRUE))
  }
  read_output <- function(dirname, run) {
    if (run) {
      output <- file.path(dirname, "spm_detail_rtmb.csv")
      return(readr::read_csv(output, show_col_types = FALSE))
    }
    runSPM_rtmb(dirname = dirname, run = FALSE)
  }

  new_spm_adapter("rtmb", execute, read_output)
}

spm_adapter <- function(engine) {
  switch(engine,
    admb = admb_adapter(),
    rtmb = rtmb_adapter(),
    stop("Unknown model engine: ", engine, ".", call. = FALSE)
  )
}

run_spm_adapter <- function(adapter, dirname, run) {
  stopifnot(inherits(adapter, "spm_adapter"))
  if (run) {
    adapter$execute(dirname)
  }
  adapter$read_output(dirname, run = run) |>
    as_spm_result()
}
