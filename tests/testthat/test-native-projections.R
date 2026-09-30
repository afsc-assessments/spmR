test_that("the compiled engine preserves scientific input equivalence", {
  executable <- Sys.getenv("SPMR_TEST_EXE")
  skip_if(
    !nzchar(executable),
    "Set SPMR_TEST_EXE to run the native regression suite."
  )
  expect_true(file.exists(executable))
  python <- Sys.which("python3")
  expect_true(
    nzchar(python),
    info = "Python 3 is required for native regression tests."
  )
  output <- tempfile("spmr-native-validation-")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  script <- normalizePath(test_path("..", "native", "run_validation.py"))
  log <- file.path(output, "suite.log")
  status <- suppressWarnings(system2(
    python,
    c(
      shQuote(script),
      "--executable",
      shQuote(executable),
      "--output",
      shQuote(output)
    ),
    stdout = log,
    stderr = log
  ))
  expect_equal(
    status,
    0L,
    info = paste(readLines(log, warn = FALSE), collapse = "\n")
  )
  if (status != 0L) {
    return(invisible(NULL))
  }
  evidence <- jsonlite::read_json(file.path(output, "validation.json"))
  expect_identical(evidence$status, "passed")
  expect_gte(length(evidence$checks), 31L)
  for (name in c("split_per_sex", "split_total", "pooled_total")) {
    expect_equal(evidence$checks[[name]]$rows, 15000L)
    expect_equal(
      evidence$checks[[name]]$mean_rec,
      evidence$checks$split_total$mean_rec
    )
    expect_equal(
      evidence$checks[[name]]$cv_rec,
      evidence$checks$split_total$cv_rec
    )
  }
  for (name in c("sr2_split", "sr2_total", "sr2_pooled")) {
    expect_lt(evidence$checks[[name]]$convergence$maximum_gradient, 1e-4)
    expect_true(evidence$checks[[name]]$convergence$hessian_positive_definite)
  }
  expect_equal(evidence$checks$zero_variance$cv_rec, 0)
})
