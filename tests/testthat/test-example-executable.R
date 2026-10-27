local_example_executable_runner <- function(env = parent.frame()) {
  script <- testthat::test_path(
    "..",
    "..",
    "examples",
    "gt20",
    "run_spmR_split_sex.R"
  )
  testthat::skip_if_not(file.exists(script), "GT20 example is unavailable")
  script <- normalizePath(script, mustWork = TRUE)
  withr::local_dir(dirname(dirname(dirname(script))), .local_envir = env)
  runner <- new.env(parent = globalenv())
  sys.source(script, envir = runner)
  runner
}

testthat::test_that("example capability probes preserve the caller directory", {
  executable <- Sys.getenv("SPMR_TEST_EXE", unset = "")
  testthat::skip_if(
    !nzchar(executable),
    "Set SPMR_TEST_EXE for the native probe"
  )
  executable <- normalizePath(executable, mustWork = TRUE)
  runner <- local_example_executable_runner()
  caller_dir <- withr::local_tempdir()
  withr::local_dir(caller_dir)
  observed <- new.env(parent = emptyenv())
  runner$system2 <- function(...) {
    observed$probe_dir <- getwd()
    base::system2(...)
  }

  verified <- runner$gt20_verify_executable(executable)

  testthat::expect_identical(verified, executable)
  testthat::expect_identical(getwd(), normalizePath(caller_dir))
  testthat::expect_length(
    list.files(caller_dir, all.files = TRUE, no.. = TRUE),
    0L
  )
  testthat::expect_false(dir.exists(observed$probe_dir))
})

testthat::test_that("failed example capability probes clean up their files", {
  runner <- local_example_executable_runner()
  caller_dir <- withr::local_tempdir()
  executable <- file.path(caller_dir, "legacy-spm")
  writeLines("placeholder", executable)
  withr::local_dir(caller_dir)
  observed <- new.env(parent = emptyenv())
  runner$system2 <- function(...) {
    observed$probe_dir <- getwd()
    writeLines("probe output", "Input_Log.rep")
    writeLines("probe output", "fmin.log")
    structure("legacy engine", status = 1L)
  }

  testthat::expect_error(
    runner$gt20_verify_executable(executable),
    "not compatible with input format 2",
    fixed = TRUE
  )
  testthat::expect_identical(getwd(), normalizePath(caller_dir))
  testthat::expect_identical(
    list.files(caller_dir, all.files = TRUE, no.. = TRUE),
    "legacy-spm"
  )
  testthat::expect_false(dir.exists(observed$probe_dir))
})
