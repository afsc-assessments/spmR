testthat::test_that("dat2list parses files written by list2dat", {
  tmp <- tempfile(fileext = ".dat")
  on.exit(unlink(tmp), add = TRUE)

  x <- list(
    vec = c(1, 2, 3),
    mat = matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
  )
  spmR::list2dat(x, tmp, hdr = "unit-test")
  y <- spmR::dat2list(tmp)

  testthat::expect_named(y, c("vec", "mat"))
  testthat::expect_equal(as.numeric(y$vec), c(1, 2, 3))
  testthat::expect_equal(as.numeric(y$mat), as.numeric(x$mat))
})

testthat::test_that("runSPM reads canonical detail output", {
  td <- tempfile("spm-run-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  detail <- data.frame(
    Stock = "test",
    Alt = c(1, 1, 2, 2),
    Sim = c(1, 2, 1, 2),
    Year = c(2022, 2022, 2022, 2022),
    SSB = c(100, 110, 120, 130),
    Rec = c(10, 11, 12, 13),
    Tot_biom = c(200, 210, 220, 230),
    SPR_Implied = NA_real_,
    F = c(0.1, 0.1, 0.2, 0.2),
    Ntot = NA_real_,
    Catch = c(50, 55, 60, 65),
    ABC = c(50, 55, 60, 65),
    OFL = c(55, 60, 66, 70),
    AvgAge = NA_real_,
    AvgAgeTot = NA_real_,
    SexRatio = NA_real_,
    B100 = 100,
    B40 = 40,
    B35 = 35
  )
  readr::write_csv(detail, file.path(td, "spm_detail.csv"))

  res <- spmR::runSPM(td, run = FALSE, engine = "admb")

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), 4)
  testthat::expect_true(all(c("Year", "Alt", "Sim", "SSB", "ABC", "OFL") %in% names(res)))
})

testthat::test_that("plotSPM returns a ggplot object on summary-format data", {
  df <- data.frame(
    Year = rep(2022:2024, times = 4),
    Alt = rep(c(1, 3, 5, 7), each = 3),
    variable = rep(c("C_mean", "C_lb", "C_ub"), times = 4),
    value = c(100, 90, 110, 95, 85, 105, 80, 70, 90, 75, 65, 85)
  )
  p <- spmR::plotSPM(df)

  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("plotSPMx handles runSPM output with Year column", {
  detail <- data.frame(
    Stock = "test",
    Alt = rep(c(1, 2), each = 6),
    Sim = rep(1:3, times = 4),
    Year = rep(2022:2023, each = 3, times = 2),
    SSB = c(100, 110, 120, 105, 115, 125, 130, 140, 150, 135, 145, 155),
    Rec = 1,
    Tot_biom = 1,
    SPR_Implied = NA_real_,
    F = 0.1,
    Ntot = NA_real_,
    Catch = c(10, 11, 12, 11, 12, 13, 20, 21, 22, 21, 22, 23),
    ABC = c(10, 11, 12, 11, 12, 13, 20, 21, 22, 21, 22, 23),
    OFL = c(11, 12, 13, 12, 13, 14, 21, 22, 23, 22, 23, 24),
    AvgAge = NA_real_,
    AvgAgeTot = NA_real_,
    SexRatio = NA_real_,
    B100 = 100,
    B40 = 40,
    B35 = 35
  )
  p <- spmR::plotSPMx(detail, alt = 2, thisyr = min(detail$Year))

  testthat::expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})
