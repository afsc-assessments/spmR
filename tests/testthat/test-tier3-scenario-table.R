testthat::test_that("tier3_scenario_table returns seven scenario rows", {
  detail <- expand.grid(
    Alt = 1:7, Sim = 1:3, Year = 2027:2028,
    KEEP.OUT.ATTRS = FALSE
  )
  detail$Stock <- "pollock"
  detail$Catch <- 100 + detail$Alt
  detail$SSB <- 500 + detail$Year - 2027
  detail$ABC <- 110 + detail$Alt
  detail$OFL <- 120 + detail$Alt
  detail$F <- 0.1 * detail$Alt
  detail$B35 <- 350

  out <- spmR::tier3_scenario_table(detail, years = 2027:2028)

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(out$Alt, 1:7)
  testthat::expect_equal(nrow(out), 7)
  testthat::expect_true(all(c("Catch_2027", "SSB_2028", "B_B35_2028") %in% names(out)))
  testthat::expect_equal(out$Catch_2027, 101:107)
})

testthat::test_that("tier3_scenario_table validates years and alternatives", {
  detail <- data.frame(
    Stock = "pollock", Alt = 1, Sim = 1, Year = 2027,
    Catch = 100, SSB = 500, ABC = 110, OFL = 120, F = 0.2, B35 = 350
  )
  testthat::expect_error(
    spmR::tier3_scenario_table(detail, years = 2030),
    "not found"
  )
  detail$Alt <- 8
  testthat::expect_error(
    spmR::tier3_scenario_table(detail),
    "1 through 7"
  )
})
