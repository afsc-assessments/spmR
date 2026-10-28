local_gt2026_example <- function(complete = TRUE, env = parent.frame()) {
  script <- testthat::test_path("..", "..", "examples", "gt_2026", "run.R")
  fixture_dir <- normalizePath(
    testthat::test_path("fixtures", "gt2026-incomplete"),
    mustWork = TRUE
  )
  testthat::skip_if_not(file.exists(script), "GT2026 example is unavailable")
  script <- normalizePath(script, mustWork = TRUE)
  withr::local_dir(dirname(dirname(dirname(script))), .local_envir = env)
  runner <- new.env(parent = globalenv())
  sys.source(script, envir = runner)
  source_dir <- withr::local_tempdir(.local_envir = env)
  supplied <- c("ForProjections.dat", "spm.dat", "tacpar.dat")
  stopifnot(all(file.copy(file.path(fixture_dir, supplied), source_dir)))
  if (complete) {
    stock_path <- file.path(source_dir, "ForProjections.dat")
    stock_lines <- readLines(stock_path, warn = FALSE)
    stock_lines <- c(
      stock_lines,
      "# Synthetic test-only SSB positions",
      "25000 24000"
    )
    writeLines(stock_lines, stock_path)
  }
  list(runner = runner, source_dir = source_dir, script_dir = dirname(script))
}

testthat::test_that("GT2026 audit identifies incomplete and repairable inputs", {
  example <- local_gt2026_example(complete = FALSE)
  audit <- example$runner$check_gt2026_inputs(example$source_dir)

  testthat::expect_s3_class(audit, "data.frame")
  testthat::expect_true(all(c("item", "status", "detail") %in% names(audit)))
  testthat::expect_true(all(
    audit$status %in% c("missing", "review", "repair", "ok", "info")
  ))
  testthat::expect_true(any(audit$status == "missing"))
  testthat::expect_true(any(audit$status == "review"))
  testthat::expect_true(any(audit$status == "repair"))
  testthat::expect_true(all(nzchar(audit$item)))
  testthat::expect_true(all(nzchar(audit$detail)))
  testthat::expect_identical(
    audit$status[audit$item == "SSB history"],
    "missing"
  )
  testthat::expect_match(
    audit$detail[audit$item == "SSB history"],
    "47 recruitment values but 45 SSB values",
    fixed = TRUE
  )
  testthat::expect_identical(
    audit$status[audit$item == "Recruitment basis"],
    "ok"
  )
})

testthat::test_that("GT2026 preserves historical inputs and rejects incomplete histories", {
  example <- local_gt2026_example(complete = FALSE)
  source_path <- file.path(example$source_dir, "ForProjections.dat")
  source_hash <- digest::digest(source_path, file = TRUE, algo = "sha256")
  raw <- example$runner$gt2026_read_source(example$source_dir)
  destination <- file.path(withr::local_tempdir(), "incomplete")

  testthat::expect_length(raw$recruitment, 47)
  testthat::expect_equal(tail(raw$recruitment, 2), c(811.8, 460.5))
  testthat::expect_equal(raw$recruitment_copies, 2)
  testthat::expect_length(raw$recruitment_rows, 4)
  testthat::expect_identical(raw$recruitment_basis, "per_sex")
  testthat::expect_length(raw$ssb, 45)
  testthat::expect_equal(tail(raw$ssb, 2), c(27020.6, 25862.1))
  testthat::expect_error(
    example$runner$run_gt2026_projection(
      example$source_dir,
      run_dir = destination,
      diagnostic = TRUE,
      run = FALSE
    ),
    "47 recruitment values but 45 SSB values",
    fixed = TRUE
  )
  testthat::expect_false(dir.exists(destination))
  testthat::expect_identical(
    digest::digest(source_path, file = TRUE, algo = "sha256"),
    source_hash
  )
})

testthat::test_that("GT2026 accepts matching 47-value histories with one row per sex", {
  example <- local_gt2026_example()
  raw <- example$runner$gt2026_read_source(example$source_dir)
  source_lines <- raw$lines
  source_lines[raw$recruitment_rows[c(1, 3)]] <- paste(
    raw$recruitment,
    collapse = " "
  )
  source_lines <- source_lines[-raw$recruitment_rows[c(2, 4)]]
  writeLines(source_lines, file.path(example$source_dir, "ForProjections.dat"))

  revised <- example$runner$gt2026_read_source(example$source_dir)
  audit <- example$runner$check_gt2026_inputs(example$source_dir)

  testthat::expect_length(revised$recruitment, 47)
  testthat::expect_length(revised$ssb, 47)
  testthat::expect_equal(revised$recruitment_copies, 2)
  testthat::expect_length(revised$recruitment_rows, 2)
  testthat::expect_identical(revised$recruitment_basis, "per_sex")
  testthat::expect_equal(revised$recruitment, raw$recruitment)
  testthat::expect_equal(revised$ssb, raw$ssb)
  testthat::expect_identical(
    audit$status[audit$item == "SSB history"],
    "review"
  )
  testthat::expect_false(grepl(
    "repeat the preceding pair",
    audit$detail[audit$item == "SSB history"],
    fixed = TRUE
  ))
})

testthat::test_that("GT2026 requires scientific declarations before staging", {
  example <- local_gt2026_example()
  destination <- file.path(withr::local_tempdir(), "projection")
  declarations <- list(
    population_weights = list(female = rep(2, 30), male = rep(1, 30)),
    units = list(abundance = "thousand_fish", weight = "kg", biomass = "t"),
    ages = 1:30
  )

  testthat::expect_error(
    example$runner$run_gt2026_projection(
      example$source_dir,
      run_dir = destination,
      run = FALSE
    ),
    "population|weight|units|ages",
    ignore.case = TRUE
  )
  testthat::expect_false(dir.exists(destination))

  for (missing_field in names(declarations)) {
    supplied <- declarations[setdiff(names(declarations), missing_field)]
    testthat::expect_error(do.call(
      example$runner$run_gt2026_projection,
      c(
        list(
          source_dir = example$source_dir,
          run_dir = destination,
          run = FALSE
        ),
        supplied
      )
    ))
    testthat::expect_false(dir.exists(destination))
  }
})

testthat::test_that("GT2026 rejects unsupported spawning timing before staging", {
  example <- local_gt2026_example()
  stock_file <- file.path(example$source_dir, "ForProjections.dat")
  lines <- readLines(stock_file, warn = FALSE)
  lines[grepl("# Spawning month$", trimws(lines))] <- "4 # Spawning month"
  writeLines(lines, stock_file)
  before <- digest::digest(stock_file, file = TRUE, algo = "sha256")
  destination <- file.path(withr::local_tempdir(), "unsupported-spawning")

  condition <- tryCatch(
    example$runner$run_gt2026_projection(
      example$source_dir,
      run_dir = destination,
      diagnostic = TRUE,
      run = FALSE
    ),
    error = identity
  )
  testthat::expect_s3_class(condition, "error")
  testthat::expect_match(
    conditionMessage(condition),
    "requires spawning month 1"
  )
  testthat::expect_identical(dir.exists(destination), FALSE)
  testthat::expect_identical(
    digest::digest(stock_file, file = TRUE, algo = "sha256"),
    before
  )
})

testthat::test_that("GT2026 flags a repeated terminal SSB pair and permits diagnostic staging", {
  example <- local_gt2026_example()
  raw <- example$runner$gt2026_read_source(example$source_dir)
  source_lines <- raw$lines
  source_lines[tail(raw$ssb_rows, 1)] <- paste(
    raw$ssb[length(raw$ssb) - c(3, 2)],
    collapse = " "
  )
  writeLines(source_lines, file.path(example$source_dir, "ForProjections.dat"))

  audit <- example$runner$check_gt2026_inputs(example$source_dir)
  testthat::expect_identical(
    audit$status[audit$item == "SSB history"],
    "review"
  )
  testthat::expect_match(
    audit$detail[audit$item == "SSB history"],
    "The final two SSB values repeat the preceding pair",
    fixed = TRUE
  )

  staged <- example$runner$run_gt2026_projection(
    example$source_dir,
    run_dir = file.path(withr::local_tempdir(), "repeated-ssb"),
    diagnostic = TRUE,
    run = FALSE
  )
  testthat::expect_s3_class(staged$inputs, "spm_inputs")
  testthat::expect_equal(
    tail(staged$inputs$species[[1]]$SSB, 4),
    rep(raw$ssb[length(raw$ssb) - c(3, 2)], 2)
  )
})

testthat::test_that("GT2026 diagnostic staging preserves supplied data and histories", {
  example <- local_gt2026_example()
  source_files <- file.path(
    example$source_dir,
    intersect(
      c(
        "ForProjections.dat",
        "spm.dat",
        "spm_input_v2.dat",
        "tacpar.dat"
      ),
      list.files(example$source_dir)
    )
  )
  before <- vapply(
    source_files,
    digest::digest,
    "",
    file = TRUE,
    algo = "sha256"
  )
  raw <- example$runner$gt2026_read_source(example$source_dir)
  destination <- file.path(withr::local_tempdir(), "diagnostic")

  staged <- example$runner$run_gt2026_projection(
    example$source_dir,
    run_dir = destination,
    diagnostic = TRUE,
    run = FALSE
  )
  stock <- staged$inputs$species[[1]]
  metadata <- staged$inputs$resolved_species[[1]]

  testthat::expect_identical(
    vapply(source_files, digest::digest, "", file = TRUE, algo = "sha256"),
    before
  )
  testthat::expect_s3_class(staged$inputs, "spm_inputs")
  testthat::expect_true(dir.exists(staged$run_dir))
  testthat::expect_false(file.exists(file.path(
    staged$run_dir,
    "spm_metadata.json"
  )))
  testthat::expect_false(file.exists(file.path(
    staged$run_dir,
    "spm_input_v2.dat"
  )))
  testthat::expect_true(any(grepl(
    "^# SPMR_INPUT_V2 2 1$",
    readLines(file.path(staged$run_dir, "ForProjections.dat"))
  )))
  testthat::expect_length(stock$R, 47)
  testthat::expect_equal(stock$R, raw$recruitment)
  testthat::expect_equal(stock$SSB, raw$ssb)
  testthat::expect_equal(stock$SSB[1], 397243)
  testthat::expect_false(identical(stock$SSB, stock$R))
  testthat::expect_equal(tail(stock$SSB, 2), c(25000, 24000))
  testthat::expect_identical(metadata$recruitment_basis, "per_sex")
  testthat::expect_equal(stock$wt_F[1:2] / 1e-12, rep(1, 2), tolerance = 1e-14)
  testthat::expect_equal(stock$wt_M[1:2] / 1e-12, rep(1, 2), tolerance = 1e-14)
  testthat::expect_equal(
    stock$wt_F[-(1:2)],
    raw$numbers(raw$female_spawn)[-(1:2)]
  )
  testthat::expect_equal(
    stock$wt_M[-(1:2)],
    raw$numbers(raw$male_spawn)[-(1:2)]
  )
  testthat::expect_equal(stock$pmature_F, rep(1, 30))
  testthat::expect_equal(stock$pmature_M, rep(1, 30))
  testthat::expect_equal(
    staged$inputs$spm$Obs_Catch,
    cbind(
      2026:2028,
      c(193.28, 378, 378)
    )
  )
  testthat::expect_null(staged$result)
  testthat::expect_null(staged$summary)
  testthat::expect_false(file.exists(file.path(destination, "spm_detail.csv")))
})

testthat::test_that("GT2026 diagnostic biomass uses declared fleet mixtures by sex", {
  example <- local_gt2026_example()
  raw <- example$runner$gt2026_read_source(example$source_dir)
  female_weights <- as.numeric(crossprod(
    raw$fleet_fraction,
    raw$female_fishery
  ))
  male_weights <- as.numeric(crossprod(raw$fleet_fraction, raw$male_fishery))
  expected_biomass <- sum(raw$female_abundance * female_weights) +
    sum(raw$male_abundance * male_weights)
  spawning_weights <- raw$numbers(raw$female_spawn)
  spawning_weights[spawning_weights == 0] <- 1e-12
  expected_ssb <- sum(raw$female_abundance * spawning_weights)
  destination <- file.path(withr::local_tempdir(), "diagnostic")

  staged <- example$runner$run_gt2026_projection(
    example$source_dir,
    run_dir = destination,
    diagnostic = TRUE,
    run = FALSE
  )
  metadata <- staged$inputs$resolved_species[[1]]

  testthat::expect_equal(metadata$population_weights$female, female_weights)
  testthat::expect_equal(metadata$population_weights$male, male_weights)
  testthat::expect_equal(staged$initial_biomass, expected_biomass)
  testthat::expect_equal(staged$initial_ssb, expected_ssb)
  testthat::expect_equal(metadata$ages, 1:30)
  testthat::expect_identical(
    metadata$units,
    list(
      abundance = "thousand_fish",
      weight = "kg",
      biomass = "t"
    )
  )
})

testthat::test_that("GT2026 uses supplied population weights, units, and ages", {
  example <- local_gt2026_example()
  destination <- file.path(withr::local_tempdir(), "declared")
  population_weights <- list(
    female = seq_len(30) / 10,
    male = seq_len(30) / 20
  )
  units <- list(abundance = "fish", weight = "kg", biomass = "kg")
  ages <- 2:31

  staged <- example$runner$run_gt2026_projection(
    example$source_dir,
    run_dir = destination,
    population_weights = population_weights,
    units = units,
    ages = ages,
    run = FALSE
  )
  stock <- staged$inputs$species[[1]]
  metadata <- staged$inputs$resolved_species[[1]]

  testthat::expect_equal(metadata$population_weights, population_weights)
  testthat::expect_identical(metadata$units, units)
  testthat::expect_equal(metadata$ages, ages)
  testthat::expect_equal(
    staged$initial_biomass,
    sum(stock$n0_F * population_weights$female) +
      sum(stock$n0_M * population_weights$male)
  )
})

testthat::test_that("GT2026 refuses unequal female and male recruitment histories", {
  example <- local_gt2026_example()
  fixture <- withr::local_tempdir()
  source_files <- file.path(
    example$source_dir,
    intersect(
      c(
        "ForProjections.dat",
        "spm.dat",
        "spm_input_v2.dat",
        "tacpar.dat"
      ),
      list.files(example$source_dir)
    )
  )
  testthat::expect_true(all(file.copy(source_files, fixture)))
  stock_file <- file.path(fixture, "ForProjections.dat")
  stock_lines <- readLines(stock_file, warn = FALSE)
  raw <- example$runner$gt2026_read_source(fixture)
  second_row <- raw$recruitment_rows[3]
  second_recruitment <- scan(text = stock_lines[second_row], quiet = TRUE)
  second_recruitment[1] <- second_recruitment[1] + 1
  stock_lines[second_row] <- paste(second_recruitment, collapse = " ")
  writeLines(stock_lines, stock_file)
  destination <- file.path(withr::local_tempdir(), "rejected")

  testthat::expect_error(
    example$runner$run_gt2026_projection(
      fixture,
      run_dir = destination,
      diagnostic = TRUE,
      run = FALSE
    ),
    "recruit|identical|match",
    ignore.case = TRUE
  )
  testthat::expect_false(dir.exists(destination))
})

testthat::test_that("GT2026 accepts a wrapped single total-recruitment series", {
  example <- local_gt2026_example()
  source_path <- file.path(example$source_dir, "ForProjections.dat")
  raw <- example$runner$gt2026_read_source(example$source_dir)
  source_lines <- raw$lines
  source_lines[raw$recruitment_declaration] <- "total # recruitment basis"
  source_lines <- source_lines[-raw$recruitment_rows[3:4]]
  writeLines(source_lines, source_path)

  revised <- example$runner$gt2026_read_source(example$source_dir)
  testthat::expect_identical(revised$recruitment_basis, "total")
  testthat::expect_equal(revised$recruitment_copies, 1)
  testthat::expect_length(revised$recruitment_rows, 2)
  testthat::expect_equal(revised$recruitment, raw$recruitment)

  staged <- example$runner$run_gt2026_projection(
    example$source_dir,
    run_dir = file.path(withr::local_tempdir(), "total"),
    diagnostic = TRUE,
    run = FALSE
  )
  testthat::expect_identical(
    staged$inputs$resolved_species[[1]]$recruitment_basis,
    "total"
  )
  testthat::expect_equal(staged$inputs$species[[1]]$R, raw$recruitment)
  audit <- example$runner$check_gt2026_inputs(staged$run_dir)
  testthat::expect_identical(
    audit$status[audit$item == "Native metadata"],
    "review"
  )
  testthat::expect_identical(
    audit$status[audit$item == "Population weights"],
    "review"
  )
  testthat::expect_identical(
    example$runner$gt2026_read_source(staged$run_dir)$recruitment_basis,
    "total"
  )
})

testthat::test_that("GT2026 stages without an external sidecar or unused TAC file", {
  example <- local_gt2026_example()
  fixture <- withr::local_tempdir()
  required_files <- c("ForProjections.dat", "spm.dat")
  testthat::expect_true(all(file.copy(
    file.path(example$source_dir, required_files),
    fixture
  )))

  audit <- example$runner$check_gt2026_inputs(fixture)
  testthat::expect_identical(
    audit$status[audit$item == "Native metadata"],
    "info"
  )
  destination <- file.path(withr::local_tempdir(), "diagnostic")
  staged <- example$runner$run_gt2026_projection(
    fixture,
    run_dir = destination,
    diagnostic = TRUE,
    run = FALSE
  )

  testthat::expect_s3_class(staged$inputs, "spm_inputs")
  testthat::expect_setequal(
    list.files(file.path(staged$run_dir, "source")),
    required_files
  )
  testthat::expect_false(file.exists(file.path(fixture, "spm_input_v2.dat")))
  testthat::expect_false(file.exists(file.path(fixture, "tacpar.dat")))
})

testthat::test_that("GT2026 finds helpers beside its script from another input directory", {
  example <- local_gt2026_example()
  script <- file.path(example$script_dir, "run.R")
  fixture <- withr::local_tempdir()
  supplied_files <- intersect(
    c(
      "ForProjections.dat",
      "spm.dat",
      "spm_input_v2.dat",
      "tacpar.dat"
    ),
    list.files(example$source_dir)
  )
  testthat::expect_true(all(file.copy(
    file.path(example$source_dir, supplied_files),
    fixture
  )))
  withr::local_dir(fixture)
  runner <- new.env(parent = globalenv())
  source(script, local = runner)

  testthat::expect_identical(runner$.gt2026_source_dir, example$script_dir)
  destination <- file.path(withr::local_tempdir(), "diagnostic")
  staged <- runner$run_gt2026_projection(
    fixture,
    run_dir = destination,
    diagnostic = TRUE,
    run = FALSE
  )

  testthat::expect_s3_class(staged$inputs, "spm_inputs")
  testthat::expect_equal(staged$inputs$species[[1]]$SSB[1], 397243)
})

testthat::test_that("GT2026 audits changed controls before creating an attempt", {
  example <- local_gt2026_example()
  fixture <- withr::local_tempdir()
  testthat::expect_true(all(file.copy(
    file.path(example$source_dir, c("ForProjections.dat", "spm.dat")),
    fixture
  )))
  control_path <- file.path(fixture, "spm.dat")
  controls <- readLines(control_path, warn = FALSE)
  controls[match("#srr_form", controls) + 1L] <- "2"
  controls[match("#nprj_yrs", controls) + 1L] <- "4"
  controls[match("#nsims", controls) + 1L] <- "2"
  writeLines(controls, control_path)

  audit <- example$runner$check_gt2026_inputs(fixture)
  testthat::expect_identical(audit$status[audit$item == "Controls"], "review")
  testthat::expect_match(
    audit$detail[audit$item == "Controls"],
    "2026-2029; 7 alternatives; 2 simulations",
    fixed = TRUE
  )
  destination <- file.path(withr::local_tempdir(), "unsupported")
  testthat::expect_error(
    example$runner$run_gt2026_projection(
      fixture,
      run_dir = destination,
      diagnostic = TRUE,
      run = FALSE
    ),
    "Rec_Gen=1",
    fixed = TRUE
  )
  testthat::expect_false(dir.exists(destination))
})

testthat::test_that("GT2026 archives supplied metadata before writing staged declarations", {
  example <- local_gt2026_example()
  fixture <- withr::local_tempdir()
  testthat::expect_true(all(file.copy(
    file.path(example$source_dir, c("ForProjections.dat", "spm.dat")),
    fixture
  )))
  source_metadata <- file.path(fixture, "spm_metadata.json")
  writeLines(
    '{"source_note":"Caller metadata retained for review"}',
    source_metadata
  )
  source_hash <- digest::digest(source_metadata, file = TRUE, algo = "sha256")

  audit <- example$runner$check_gt2026_inputs(fixture)
  testthat::expect_identical(audit$status[audit$item == "R metadata"], "review")
  testthat::expect_identical(
    audit$status[audit$item == "Population weights"],
    "review"
  )
  staged <- example$runner$run_gt2026_projection(
    fixture,
    run_dir = file.path(withr::local_tempdir(), "diagnostic"),
    diagnostic = TRUE,
    run = FALSE
  )
  archived_metadata <- file.path(staged$run_dir, "source", "spm_metadata.json")
  testthat::expect_identical(
    digest::digest(archived_metadata, file = TRUE, algo = "sha256"),
    source_hash
  )
  testthat::expect_identical(
    digest::digest(source_metadata, file = TRUE, algo = "sha256"),
    source_hash
  )
  decisions <- jsonlite::read_json(file.path(
    staged$run_dir,
    "staging-decisions.json"
  ))
  testthat::expect_identical(
    decisions$source_sha256$spm_metadata.json,
    source_hash
  )
  testthat::expect_s3_class(staged$inputs, "spm_inputs")
})
