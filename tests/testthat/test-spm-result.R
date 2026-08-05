minimal_result <- function() {
  data.frame(
    Stock = "test",
    Alt = c(1, 2),
    Sim = 1,
    Year = 2025,
    Catch = c(50, 60),
    SSB = c(100, 110),
    ABC = c(55, 65),
    OFL = c(60, 70),
    Extra = c("a", "b")
  )
}

testthat::test_that("as_spm_result standardizes legacy output", {
  result <- spmR::as_spm_result(minimal_result())
  generic <- minimal_result()
  generic$Scenario <- c("baseline", "high-catch")
  generic$Alt <- NULL
  generic_result <- spmR::as_spm_result(generic)

  testthat::expect_s3_class(result, "spm_result")
  testthat::expect_equal(result$Scenario, c("1", "2"))
  testthat::expect_equal(result$Extra, c("a", "b"))
  testthat::expect_equal(generic_result$Scenario, c("baseline", "high-catch"))
})

testthat::test_that("as_spm_result validates the common schema", {
  missing_metric <- minimal_result()
  missing_metric$OFL <- NULL
  duplicate_key <- rbind(minimal_result()[1, ], minimal_result()[1, ])
  character_year <- minimal_result()
  character_year$Year <- as.character(character_year$Year)

  testthat::expect_snapshot(spmR::as_spm_result(list()), error = TRUE)
  testthat::expect_snapshot(spmR::as_spm_result(missing_metric), error = TRUE)
  testthat::expect_snapshot(spmR::as_spm_result(duplicate_key), error = TRUE)
  testthat::expect_snapshot(spmR::as_spm_result(character_year), error = TRUE)
})

testthat::test_that("model adapters standardize backend output", {
  executed <- new.env(parent = emptyenv())
  executed$value <- FALSE
  adapter <- spmR:::new_spm_adapter(
    name = "test",
    execute = function(dirname) executed$value <- TRUE,
    read_output = function(dirname, run) minimal_result()
  )

  result <- spmR:::run_spm_adapter(adapter, tempdir(), run = FALSE)
  run_result <- spmR:::run_spm_adapter(adapter, tempdir(), run = TRUE)

  testthat::expect_s3_class(result, "spm_result")
  testthat::expect_equal(result$Scenario, c("1", "2"))
  testthat::expect_s3_class(run_result, "spm_result")
  testthat::expect_identical(executed$value, TRUE)
})
