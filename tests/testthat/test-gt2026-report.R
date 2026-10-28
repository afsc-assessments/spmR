local_gt2026_report <- function(
  n_years = 3L,
  control_names = c("canonical", "gt2026", "arbitrary"),
  env = parent.frame()
) {
  control_names <- match.arg(control_names)
  script <- testthat::test_path("..", "..", "examples", "gt_2026", "report.R")
  testthat::skip_if_not(
    file.exists(script),
    "GT2026 report helper is unavailable"
  )
  helper <- new.env(parent = globalenv())
  sys.source(script, envir = helper)
  root <- withr::local_tempdir(.local_envir = env)
  run_dir <- file.path(root, "synthetic-attempt")
  source_dir <- file.path(root, "synthetic-current")
  dir.create(run_dir)
  dir.create(source_dir)
  dir.create(file.path(run_dir, "source"))
  controls <- list(
    run_name = "Synthetic_report_test",
    Tier = 3,
    nalts = 7,
    alt_list = 1:7,
    TAC_ABC = 1,
    SrType = 2,
    Rec_Gen = 1,
    Fmsy_F35 = 0,
    Rec_Cond = 0,
    Write_Big = 1,
    npro = n_years,
    nsims = 20,
    styr = 2026,
    nyrs_catch_in = 1,
    nspp = 1,
    OY_min = 0,
    OY_max = 2000000,
    spp_files = "ForProjections.dat",
    ABC_Multiplier = 1,
    N_scalar = 1,
    Alt4_SPR = 0.75,
    ntacspp = 1,
    tac_ind = 1,
    Obs_Catch = matrix(c(2026, 193.28), nrow = 1)
  )
  if (identical(control_names, "gt2026")) {
    names(controls) <- c(
      "rn",
      "Tier",
      "nalts",
      "alts",
      "tac_flag",
      "srr_type",
      "srr_form",
      "srr_conditioning",
      "srr_reserved",
      "spm_detail_flag",
      "nprj_yrs",
      "nsims",
      "beg_yr",
      "nyrs_fixed_catch",
      "nspp",
      "OY_min",
      "OY_max",
      "datafile",
      "ABC_mults",
      "scalars",
      "alt4_spr",
      "nTAC_cat",
      "nTACind",
      "fixed_catch"
    )
  } else if (identical(control_names, "arbitrary")) {
    names(controls) <- paste0("arbitrary_comment_", seq_along(controls))
  }
  spmR::list2dat(
    controls,
    file.path(run_dir, "spm.dat"),
    "Synthetic report test only"
  )
  species_lines <- readLines(testthat::test_path(
    "fixtures",
    "input-v2",
    "pm.prj"
  ))
  species_lines[species_lines == "weights_audit"] <- "synthetic"
  writeLines(species_lines, file.path(run_dir, "ForProjections.dat"))
  spmR::write_spm_metadata(
    run_dir,
    list(list(
      file = "ForProjections.dat",
      recruitment_basis = "total",
      ages = 1:15,
      units = list(abundance = "thousand_fish", weight = "kg", biomass = "t"),
      population_weights = list(
        female = list(ages = 1:15, values = (1:15) / 10),
        male = list(ages = 1:15, values = (1:15) / 20)
      )
    ))
  )
  files <- c("spm.dat", "ForProjections.dat")
  file.copy(file.path(run_dir, files), source_dir)
  file.copy(file.path(run_dir, files), file.path(run_dir, "source"))
  result <- tidyr::expand_grid(
    Alt = 1:7,
    Sim = 1:20,
    Year = seq.int(2026, length.out = n_years)
  ) |>
    dplyr::mutate(
      Stock = "synthetic",
      SSB = 100 + .data$Sim + .data$Alt,
      Tot_biom = 200 + .data$Sim,
      Catch = 10 + .data$Sim,
      ABC = 20 + .data$Sim,
      OFL = 30 + .data$Sim,
      Rec = 50,
      F = 0.05,
      B100 = 100,
      B40 = 40,
      B35 = 35
    )
  readr::write_csv(result, file.path(run_dir, "spm_detail.csv"))
  reference_summary <- tibble::tibble(
    spp_file = "synthetic",
    Alt = NA_real_,
    Year = NA_real_,
    variable = c("F_abc", "F_ofl"),
    value = c(0.1, 0.15)
  )
  readr::write_csv(reference_summary, file.path(run_dir, "spm_summary.csv"))
  writeLines(
    "Finished simulations using standard (avg, var) stochastic approach",
    file.path(run_dir, "spm_stdout.log")
  )
  writeLines(character(), file.path(run_dir, "spm_stderr.log"))
  writeLines(
    "Synthetic receipt fixture",
    file.path(run_dir, "spm_input_receipt.tsv")
  )
  hashes <- function(files, directory = run_dir) {
    as.list(stats::setNames(
      vapply(
        file.path(directory, files),
        digest::digest,
        character(1),
        file = TRUE,
        algo = "sha256"
      ),
      files
    ))
  }
  provenance <- list(
    status = "success",
    engine = "admb",
    exit_status = 0,
    run_id = "synthetic-report-test",
    completed_utc = "2026-10-27 00:00:00 UTC",
    package_version = "synthetic-test-only",
    executable_sha256 = paste(rep("0", 64), collapse = ""),
    input_hashes = hashes(files),
    output_hashes = hashes(c(
      "spm_detail.csv",
      "spm_summary.csv",
      "spm_stdout.log",
      "spm_stderr.log",
      "spm_input_receipt.tsv"
    )),
    species = list(list(
      file = "ForProjections.dat",
      stock = "synthetic",
      units = list(biomass = "t")
    )),
    convergence = list(applicable = FALSE)
  )
  decisions <- list(
    status = "SYNTHETIC TEST ONLY; no GT scientific results",
    diagnostic = TRUE,
    population_weights = "Synthetic test only",
    source_sha256 = hashes(files)
  )
  jsonlite::write_json(
    provenance,
    file.path(run_dir, "spm_last_success_provenance.json"),
    auto_unbox = TRUE
  )
  jsonlite::write_json(
    decisions,
    file.path(run_dir, "staging-decisions.json"),
    auto_unbox = TRUE
  )
  list(
    helper = helper,
    run_dir = run_dir,
    source_dir = source_dir,
    result = result,
    provenance = provenance,
    decisions = decisions,
    hashes = hashes
  )
}

expect_gt2026_report_error <- function(code, pattern) {
  condition <- tryCatch(force(code), error = identity)
  testthat::expect_s3_class(condition, "error")
  testthat::expect_match(conditionMessage(condition), pattern)
}

testthat::test_that("GT2026 saved report verifies an explicitly selected synthetic attempt", {
  fixture <- local_gt2026_report()
  withr::local_options(warn = 1)
  report <- fixture$helper$gt2026_read_report_run(
    fixture$run_dir,
    fixture$source_dir
  )
  testthat::expect_equal(nrow(report$result), 420L)
  testthat::expect_identical(report$biomass_units, "t")
  testthat::expect_identical(report$provenance$species[[1]]$stock, "synthetic")
  testthat::expect_identical(report$decisions$diagnostic, TRUE)
  testthat::expect_equal(report$controls$nsims, 20)
  testthat::expect_equal(report$f_reference_points, c(F40 = 0.1, F35 = 0.15))
  testthat::expect_identical(
    report$source_comparison$matches_current,
    c(TRUE, TRUE)
  )
  testthat::expect_identical(getOption("warn"), 1L)
  writeLines(
    "Changed current source only",
    file.path(fixture$source_dir, "ForProjections.dat")
  )
  older <- fixture$helper$gt2026_read_report_run(
    fixture$run_dir,
    fixture$source_dir
  )
  testthat::expect_identical(
    older$source_comparison$matches_current,
    c(TRUE, FALSE)
  )
  testthat::expect_equal(older$result, report$result)
})

testthat::test_that("GT2026 saved report rejects incomplete and altered evidence", {
  fixture <- local_gt2026_report()
  reader <- function() {
    fixture$helper$gt2026_read_report_run(fixture$run_dir, fixture$source_dir)
  }
  expect_gt2026_report_error(
    fixture$helper$gt2026_read_report_run("", fixture$source_dir),
    "explicitly"
  )
  expect_gt2026_report_error(
    fixture$helper$gt2026_read_report_run(
      file.path(fixture$run_dir, "missing-attempt"),
      fixture$source_dir
    ),
    "Select an existing saved attempt"
  )
  for (location in c(
    "spm.dat",
    "spm_detail.csv",
    "spm_summary.csv",
    "source/ForProjections.dat"
  )) {
    path <- file.path(fixture$run_dir, location)
    original <- readLines(path)
    writeLines(c(original, "altered evidence"), path)
    expect_gt2026_report_error(reader(), "Hash mismatch")
    writeLines(original, path)
  }
  provenance_path <- file.path(
    fixture$run_dir,
    "spm_last_success_provenance.json"
  )
  incomplete <- fixture$provenance
  incomplete$output_hashes$spm_detail.csv <- NULL
  jsonlite::write_json(incomplete, provenance_path, auto_unbox = TRUE)
  expect_gt2026_report_error(reader(), "invalid outputs hash map")
  incomplete <- fixture$provenance
  incomplete$status <- "failure"
  jsonlite::write_json(incomplete, provenance_path, auto_unbox = TRUE)
  expect_gt2026_report_error(reader(), "successful ADMB provenance")
  incomplete <- fixture$provenance
  incomplete$package_version <- NULL
  jsonlite::write_json(incomplete, provenance_path, auto_unbox = TRUE)
  expect_gt2026_report_error(reader(), "successful ADMB provenance")
  unlink(provenance_path)
  expect_gt2026_report_error(reader(), "requires staging decisions")
})

testthat::test_that("GT2026 F references require a complete hash-verified summary", {
  fixture <- local_gt2026_report()
  reader <- function() {
    fixture$helper$gt2026_read_report_run(fixture$run_dir, fixture$source_dir)
  }
  summary_path <- file.path(fixture$run_dir, "spm_summary.csv")
  summary <- readr::read_csv(summary_path, show_col_types = FALSE)
  unlink(summary_path)
  expect_gt2026_report_error(reader(), "Missing outputs file: spm_summary.csv")
  readr::write_csv(summary, summary_path)
  provenance_path <- file.path(fixture$run_dir, "spm_last_success_provenance.json")
  incomplete <- fixture$provenance
  incomplete$output_hashes$spm_summary.csv <- NULL
  jsonlite::write_json(incomplete, provenance_path, auto_unbox = TRUE)
  expect_gt2026_report_error(reader(), "invalid outputs hash map")
  for (invalid_summary in list(
    summary[1, ],
    dplyr::mutate(summary, value = c(0, 0.15))
  )) {
    readr::write_csv(invalid_summary, summary_path)
    provenance <- fixture$provenance
    provenance$output_hashes <- fixture$hashes(names(provenance$output_hashes))
    jsonlite::write_json(provenance, provenance_path, auto_unbox = TRUE)
    expect_gt2026_report_error(reader(), "one positive F_abc and F_ofl value")
  }
})

testthat::test_that("GT2026 F40 and F35 labels require their saved SPR targets", {
  fixture <- local_gt2026_report()
  species_path <- file.path(fixture$run_dir, "ForProjections.dat")
  original <- readLines(species_path)
  for (field in c("SPR_abc", "SPR_ofl")) {
    changed <- original
    changed[which(changed == paste("#", field)) + 1L] <- "0.45"
    writeLines(changed, species_path)
    provenance <- fixture$provenance
    provenance$input_hashes <- fixture$hashes(names(provenance$input_hashes))
    jsonlite::write_json(
      provenance,
      file.path(fixture$run_dir, "spm_last_success_provenance.json"),
      auto_unbox = TRUE
    )
    expect_gt2026_report_error(
      fixture$helper$gt2026_read_report_run(fixture$run_dir, fixture$source_dir),
      "F40/F35 labels require saved SPR targets"
    )
  }
})

testthat::test_that("GT2026 saved report accepts the staged control labels", {
  fixture <- local_gt2026_report(control_names = "gt2026")
  raw_controls <- spmR::dat2list(file.path(fixture$run_dir, "spm.dat"))
  testthat::expect_null(raw_controls$npro)
  testthat::expect_equal(raw_controls$nprj_yrs, 3)
  testthat::expect_equal(dim(raw_controls$alts), c(7L, 1L))
  report <- fixture$helper$gt2026_read_report_run(
    fixture$run_dir,
    fixture$source_dir
  )
  testthat::expect_equal(nrow(report$result), 420L)
  testthat::expect_equal(report$controls$npro, 3)
  testthat::expect_equal(report$controls$styr, 2026)
  testthat::expect_equal(report$controls$Rec_Gen, 1)
  testthat::expect_equal(as.vector(report$controls$alt_list), 1:7)
  testthat::expect_identical(report$controls$spp_files, "ForProjections.dat")
})

testthat::test_that("GT2026 saved report reads controls by position rather than comments", {
  fixture <- local_gt2026_report(control_names = "arbitrary")
  report <- fixture$helper$gt2026_read_report_run(
    fixture$run_dir,
    fixture$source_dir
  )
  testthat::expect_equal(nrow(report$result), 420L)
  testthat::expect_equal(report$controls$npro, 3)
  testthat::expect_equal(report$controls$styr, 2026)
  testthat::expect_equal(report$controls$Rec_Gen, 1)
  testthat::expect_equal(report$controls$alt_list, 1:7)
  testthat::expect_identical(report$controls$spp_files, "ForProjections.dat")
})

testthat::test_that("GT2026 saved report rejects incomplete and unsupported aliased controls", {
  fixture <- local_gt2026_report(control_names = "gt2026")
  controls_path <- file.path(fixture$run_dir, "spm.dat")
  controls <- spmR::dat2list(controls_path)
  invalid_cases <- list(
    list(field = "srr_form", value = NULL, pattern = "spm.dat:"),
    list(
      field = "srr_form",
      value = 2,
      pattern = "unsupported projection controls"
    ),
    list(field = "nprj_yrs", value = 0, pattern = "invalid npro")
  )
  for (invalid_case in invalid_cases) {
    invalid <- controls
    invalid[[invalid_case$field]] <- invalid_case$value
    spmR::list2dat(invalid, controls_path, "Synthetic invalid control fixture")
    provenance <- fixture$provenance
    provenance$input_hashes <- fixture$hashes(names(provenance$input_hashes))
    jsonlite::write_json(
      provenance,
      file.path(fixture$run_dir, "spm_last_success_provenance.json"),
      auto_unbox = TRUE
    )
    expect_gt2026_report_error(
      fixture$helper$gt2026_read_report_run(
        fixture$run_dir,
        fixture$source_dir
      ),
      invalid_case$pattern
    )
  }
})

testthat::test_that("GT2026 saved report checks completion and detail dimensions beyond hashes", {
  fixture <- local_gt2026_report()
  provenance_path <- file.path(
    fixture$run_dir,
    "spm_last_success_provenance.json"
  )
  rewrite <- function() {
    fixture$provenance$output_hashes <- fixture$hashes(names(
      fixture$provenance$output_hashes
    ))
    jsonlite::write_json(fixture$provenance, provenance_path, auto_unbox = TRUE)
  }
  reader <- function() {
    fixture$helper$gt2026_read_report_run(fixture$run_dir, fixture$source_dir)
  }
  writeLines(
    "Incomplete simulation",
    file.path(fixture$run_dir, "spm_stdout.log")
  )
  rewrite()
  expect_gt2026_report_error(reader(), "verified completion")
  writeLines(
    "Finished simulations using standard (avg, var) stochastic approach",
    file.path(fixture$run_dir, "spm_stdout.log")
  )
  readr::write_csv(
    fixture$result[-1, ],
    file.path(fixture$run_dir, "spm_detail.csv")
  )
  rewrite()
  expect_gt2026_report_error(reader(), "projection dimensions")
  invalid <- fixture$result
  invalid$SSB[1] <- -1
  readr::write_csv(invalid, file.path(fixture$run_dir, "spm_detail.csv"))
  rewrite()
  expect_gt2026_report_error(reader(), "valid values")
  controls <- spmR::dat2list(file.path(fixture$run_dir, "spm.dat"))
  controls$Rec_Gen <- 2
  spmR::list2dat(
    controls,
    file.path(fixture$run_dir, "spm.dat"),
    "Synthetic fitted-mode rejection"
  )
  fixture$provenance$input_hashes <- fixture$hashes(names(
    fixture$provenance$input_hashes
  ))
  rewrite()
  expect_gt2026_report_error(reader(), "unsupported projection controls")
})

testthat::test_that("GT2026 report summaries use every simulation and common deterministic worms", {
  fixture <- local_gt2026_report()
  series <- fixture$helper$gt2026_report_series(fixture$result)
  testthat::expect_equal(series$worm_ids, 1:15)
  testthat::expect_equal(nrow(series$worms), 7 * 15 * 3 * 5)
  testthat::expect_equal(nrow(series$summary), 7 * 3 * 5)
  ssb <- dplyr::filter(
    series$summary,
    .data$Alt == 1,
    .data$Year == 2026,
    .data$metric == "SSB"
  )
  testthat::expect_equal(ssb$mean, mean(101 + 1:20))
  testthat::expect_equal(ssb$median, stats::median(101 + 1:20))
  testthat::expect_equal(ssb$lower, unname(stats::quantile(101 + 1:20, 0.05)))
  testthat::expect_equal(ssb$upper, unname(stats::quantile(101 + 1:20, 0.95)))
  reordered <- fixture$helper$gt2026_report_series(fixture$result[
    nrow(fixture$result):1,
  ])
  testthat::expect_equal(reordered$worms, series$worms)
  testthat::expect_equal(reordered$summary, series$summary)
  too_few <- dplyr::filter(fixture$result, .data$Sim < 15)
  expect_gt2026_report_error(
    fixture$helper$gt2026_report_series(too_few),
    "15 worms but only 14"
  )
  expect_gt2026_report_error(
    fixture$helper$gt2026_report_series(fixture$result[-1, ]),
    "same simulation IDs"
  )
  expect_gt2026_report_error(
    fixture$helper$gt2026_report_series(fixture$result, 1.5),
    "positive integer"
  )
})

testthat::test_that("GT2026 source report renders an audit by default and saved results explicitly", {
  testthat::skip_if_not_installed("quarto")
  testthat::skip_if_not_installed("gt")
  testthat::skip_if_not(
    !is.null(quarto::quarto_path()),
    "Quarto CLI is unavailable"
  )
  package_examples <- testthat::test_path("..", "..", "examples")
  testthat::skip_if_not(
    dir.exists(package_examples),
    "Source examples are unavailable in installed-package checks"
  )
  package_examples <- normalizePath(package_examples, mustWork = TRUE)
  root <- withr::local_tempdir()
  example_dir <- file.path(root, "examples", "gt_2026")
  helper_dir <- file.path(root, "examples", "gt20")
  dir.create(example_dir, recursive = TRUE)
  dir.create(helper_dir)
  source_names <- c(
    "greenland_turbot.qmd",
    "run.R",
    "report.R",
    "spm.dat",
    "ForProjections.dat",
    "tacpar.dat"
  )
  stopifnot(all(file.copy(
    file.path(package_examples, "gt_2026", source_names),
    example_dir
  )))
  stopifnot(file.copy(
    file.path(package_examples, "gt20", "run_spmR_split_sex.R"),
    helper_dir
  ))
  source_paths <- c(
    file.path(example_dir, source_names),
    file.path(helper_dir, "run_spmR_split_sex.R")
  )
  hashes <- function(paths) {
    vapply(paths, digest::digest, character(1), file = TRUE, algo = "sha256")
  }
  source_before <- hashes(source_paths)
  fixture <- local_gt2026_report()
  saved_paths <- list.files(
    fixture$run_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  saved_before <- hashes(saved_paths)
  attempts <- file.path(example_dir, "spmR_runs")
  testthat::expect_false(dir.exists(attempts))
  qmd <- file.path(example_dir, "greenland_turbot.qmd")
  rendered_body <- function(filename) {
    html <- paste(
      readLines(file.path(example_dir, filename), warn = FALSE),
      collapse = "\n"
    )
    for (tag in c("script", "style", "pre", "code")) {
      html <- gsub(
        paste0("(?is)<", tag, "\\b[^>]*>.*?</", tag, ">"),
        "",
        html,
        perl = TRUE
      )
    }
    html
  }
  png_count <- function(html) {
    matches <- gregexpr(
      "<img\\b[^>]*\\bsrc=[\"']data:image/png[;,]",
      html,
      perl = TRUE
    )
    length(regmatches(html, matches)[[1]])
  }

  quarto::quarto_render(
    input = qmd,
    output_file = "audit.html",
    execute_dir = example_dir,
    quiet = TRUE
  )
  audit <- rendered_body("audit.html")
  testthat::expect_match(
    audit,
    "<strong>Audit only: no saved attempt selected.</strong>",
    fixed = TRUE
  )
  testthat::expect_identical(png_count(audit), 0L)
  testthat::expect_false(dir.exists(attempts))
  testthat::expect_identical(hashes(source_paths), source_before)
  testthat::expect_identical(hashes(saved_paths), saved_before)

  quarto::quarto_render(
    input = qmd,
    output_file = "saved.html",
    execute_dir = example_dir,
    execute_params = list(saved_run = fixture$run_dir),
    quiet = TRUE
  )
  saved <- rendered_body("saved.html")
  testthat::expect_match(
    saved,
    "<strong>Saved conditional projection loaded.</strong>",
    fixed = TRUE
  )
  testthat::expect_identical(png_count(saved), 3L)
  testthat::expect_false(dir.exists(attempts))
  testthat::expect_identical(hashes(source_paths), source_before)
  testthat::expect_identical(hashes(saved_paths), saved_before)
})
