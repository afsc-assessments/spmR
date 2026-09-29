local_spm_input_fixture <- function(env = parent.frame()) {
  folder <- withr::local_tempdir(.local_envir = env)
  file.copy(
    list.files(testthat::test_path("fixtures", "input-v2"), full.names = TRUE),
    folder
  )
  folder
}

spm_fixture_metadata <- function() {
  list(list(
    file = "pm.prj",
    recruitment_basis = "total",
    ages = 1:15,
    units = list(abundance = "fish", weight = "kg", biomass = "kg"),
    population_weights = list(
      female = list(ages = 1:15, values = (1:15) / 10),
      male = list(ages = 1:15, values = (1:15) / 20)
    )
  ))
}

spm_fixture_edit <- function(folder, file, old, new) {
  path <- file.path(folder, file)
  text <- paste(readLines(path), collapse = "\n")
  stopifnot(grepl(old, text, fixed = TRUE))
  writeLines(sub(old, new, text, fixed = TRUE), path)
}

testthat::test_that("version-2 metadata preserves inputs and emits explicit native schedules", {
  folder <- local_spm_input_fixture()
  before <- spm_sha256(list.files(folder, full.names = TRUE))
  write_spm_metadata(folder, spm_fixture_metadata())
  inputs <- validate_spm_inputs(folder)
  testthat::expect_s3_class(inputs, "spm_inputs")
  testthat::expect_equal(inputs$species[[1]]$R, c(80, 100, 120, 100))
  testthat::expect_equal(
    inputs$resolved_species[[1]]$population_weights$male,
    (1:15) / 20
  )
  testthat::expect_identical(
    inputs$resolved_species[[1]]$male_population_substitute,
    "none"
  )
  testthat::expect_identical(
    before,
    spm_sha256(file.path(folder, names(before)))
  )
  native <- file.path(folder, "spm_input_v2.dat")
  spm_write_native_input(inputs, native)
  lines <- readLines(native)
  testthat::expect_identical(
    lines[1:2],
    c("SPMR_INPUT_V2 2 1", "pm.prj 2 15 1 1 1 1 0")
  )
  testthat::expect_identical(tail(lines, 1), "END_SPMR_INPUT_V2")
})

testthat::test_that("missing recruitment basis preserves existing metadata", {
  folder <- local_spm_input_fixture()
  metadata <- spm_fixture_metadata()
  write_spm_metadata(folder, metadata)
  before <- readLines(file.path(folder, "spm_metadata.json"))
  metadata[[1]]$recruitment_basis <- NULL
  testthat::expect_snapshot(error = TRUE, write_spm_metadata(folder, metadata))
  testthat::expect_identical(
    readLines(file.path(folder, "spm_metadata.json")),
    before
  )
})

testthat::test_that("split-sex male population weights must be supplied or declared", {
  folder <- local_spm_input_fixture()
  metadata <- spm_fixture_metadata()
  metadata[[1]]$population_weights$male <- NULL
  testthat::expect_snapshot(
    error = TRUE,
    write_spm_metadata(folder, metadata, strict = FALSE)
  )
})

testthat::test_that("female population weights are required", {
  folder <- local_spm_input_fixture()
  metadata <- spm_fixture_metadata()
  metadata[[1]]$population_weights$female <- NULL
  testthat::expect_snapshot(error = TRUE, write_spm_metadata(folder, metadata))
})

testthat::test_that("declared male substitute is reported and recorded", {
  folder <- local_spm_input_fixture()
  metadata <- spm_fixture_metadata()
  metadata[[1]]$population_weights$male <- NULL
  metadata[[1]]$male_population_substitute <- "mean_male_fishery"
  testthat::expect_snapshot(write_spm_metadata(folder, metadata))
  inputs <- suppressWarnings(validate_spm_inputs(folder))
  testthat::expect_equal(
    inputs$resolved_species[[1]]$population_weights$male,
    (1:15) / 5
  )
  testthat::expect_identical(
    inputs$resolved_species[[1]]$male_population_substitute,
    "mean_male_fishery"
  )
})

testthat::test_that("units are explicit and coherent", {
  folder <- local_spm_input_fixture()
  metadata <- spm_fixture_metadata()
  metadata[[1]]$units$weight <- "g"
  testthat::expect_snapshot(error = TRUE, write_spm_metadata(folder, metadata))
  metadata[[1]]$units$weight <- "kg"
  metadata[[1]]$units$abundance <- "million_fish"
  testthat::expect_snapshot(error = TRUE, write_spm_metadata(folder, metadata))
  metadata[[1]]$units$biomass <- "thousand_t"
  testthat::expect_no_condition(write_spm_metadata(folder, metadata))
})

testthat::test_that("population age vectors are ordered and dimensioned", {
  folder <- local_spm_input_fixture()
  metadata <- spm_fixture_metadata()
  metadata[[1]]$ages <- rev(1:15)
  testthat::expect_snapshot(error = TRUE, write_spm_metadata(folder, metadata))
  metadata <- spm_fixture_metadata()
  metadata[[1]]$population_weights$male$ages <- rev(1:15)
  testthat::expect_snapshot(error = TRUE, write_spm_metadata(folder, metadata))
  metadata <- spm_fixture_metadata()
  metadata[[1]]$population_weights$male$values <- 1:14
  testthat::expect_snapshot(error = TRUE, write_spm_metadata(folder, metadata))
})

testthat::test_that("population weights are finite and positive", {
  folder <- local_spm_input_fixture()
  for (value in list(NA_real_, Inf, 0, -1)) {
    metadata <- spm_fixture_metadata()
    metadata[[1]]$population_weights$male$values[2] <- value
    testthat::expect_snapshot(
      error = TRUE,
      write_spm_metadata(folder, metadata)
    )
  }
})

testthat::test_that("unknown formats and fields are visible", {
  folder <- local_spm_input_fixture()
  document <- list(format_version = 3L, species = spm_fixture_metadata())
  jsonlite::write_json(
    document,
    file.path(folder, "spm_metadata.json"),
    auto_unbox = TRUE
  )
  testthat::expect_snapshot(error = TRUE, validate_spm_inputs(folder))
  metadata <- spm_fixture_metadata()
  metadata[[1]]$mistyped_field <- 1
  testthat::expect_snapshot(error = TRUE, write_spm_metadata(folder, metadata))
})

testthat::test_that("positional parsing rejects trailing fields and wrong recruitment history", {
  folder <- local_spm_input_fixture()
  cat("\n1 2 3\n", file = file.path(folder, "pm.prj"), append = TRUE)
  testthat::expect_snapshot(
    error = TRUE,
    write_spm_metadata(folder, spm_fixture_metadata())
  )
  folder <- local_spm_input_fixture()
  spm_fixture_edit(folder, "pm.prj", "80 100 120 100", "80 0 120 100")
  testthat::expect_snapshot(
    error = TRUE,
    write_spm_metadata(folder, spm_fixture_metadata())
  )
})

testthat::test_that("catch years and population scalars are checked", {
  folder <- local_spm_input_fixture()
  spm_fixture_edit(folder, "spm.dat", "2026 0", "2027 0")
  testthat::expect_snapshot(
    error = TRUE,
    write_spm_metadata(folder, spm_fixture_metadata())
  )
  folder <- local_spm_input_fixture()
  spm_fixture_edit(folder, "spm.dat", "# N_scalar\n1", "# N_scalar\n1000")
  testthat::expect_snapshot(
    error = TRUE,
    write_spm_metadata(folder, spm_fixture_metadata())
  )
})

testthat::test_that("legacy executable probe cannot clobber previous projection output", {
  testthat::skip_on_os("windows")
  folder <- local_spm_input_fixture()
  write_spm_metadata(folder, spm_fixture_metadata())
  writeLines("old valid output", file.path(folder, "spm_detail.csv"))
  executable <- file.path(folder, "spm")
  writeLines(
    c("#!/bin/sh", "echo clobber > spm_detail.csv", "exit 1"),
    executable
  )
  Sys.chmod(executable, "0755")
  testthat::expect_snapshot(error = TRUE, runSPM(folder, run = TRUE))
  testthat::expect_identical(
    readLines(file.path(folder, "spm_detail.csv")),
    "old valid output"
  )
  provenance <- jsonlite::read_json(file.path(
    folder,
    "spm_run_provenance.json"
  ))
  testthat::expect_identical(provenance$status, "failure")
  testthat::expect_match(provenance$executable_sha256, "^[0-9a-f]{64}$")
})

testthat::test_that("fresh execution rejects stale CSV despite success-like stdout", {
  testthat::skip_on_os("windows")
  folder <- local_spm_input_fixture()
  write_spm_metadata(folder, spm_fixture_metadata())
  writeLines("old valid output", file.path(folder, "spm_detail.csv"))
  executable <- file.path(folder, "spm")
  writeLines(
    c(
      "#!/bin/sh",
      'if [ "$1" = "-spmr-capabilities" ]; then',
      "  echo SPMR_INPUT_FORMAT=2",
      "  exit 0",
      "fi",
      "echo 'Finished simulations using standard (avg, var) stochastic approach'",
      "exit 0"
    ),
    executable
  )
  Sys.chmod(executable, "0755")
  testthat::expect_snapshot(error = TRUE, runSPM(folder, run = TRUE))
  testthat::expect_identical(
    readLines(file.path(folder, "spm_detail.csv")),
    "old valid output"
  )
  provenance <- jsonlite::read_json(file.path(
    folder,
    "spm_run_provenance.json"
  ))
  testthat::expect_identical(provenance$status, "failure")
  testthat::expect_identical(provenance$exit_status, 0L)
  testthat::expect_named(
    provenance$input_hashes,
    c("spm.dat", "pm.prj", "tacpar.dat", "spm_metadata.json")
  )
})

testthat::test_that("experimental RTMB adapter refuses scientific execution", {
  folder <- local_spm_input_fixture()
  testthat::expect_snapshot(
    error = TRUE,
    runSPM(folder, run = TRUE, engine = "rtmb")
  )
})

testthat::test_that("metadata and species filenames protect reserved files", {
  folder <- local_spm_input_fixture()
  before <- readLines(file.path(folder, "spm.dat"))
  testthat::expect_snapshot(
    error = TRUE,
    write_spm_metadata(folder, spm_fixture_metadata(), metadata = "spm.dat")
  )
  testthat::expect_identical(readLines(file.path(folder, "spm.dat")), before)
  spm_fixture_edit(folder, "spm.dat", "pm.prj", "spm_detail.csv")
  testthat::expect_snapshot(
    error = TRUE,
    write_spm_metadata(folder, spm_fixture_metadata())
  )
})

testthat::test_that("pooled sexes share declared population weights with an explicit record", {
  folder <- local_spm_input_fixture()
  species <- spm_read_legacy(folder)$species[[1]]
  species$nsexes <- 1
  species[c("file", "M_M", "pmature_M", "wt_gear_M", "sel_M", "n0_M")] <- NULL
  list2dat(species, file.path(folder, "pm.prj"))
  metadata <- spm_fixture_metadata()
  metadata[[1]]$population_weights$male <- NULL
  testthat::expect_snapshot(write_spm_metadata(folder, metadata))
  inputs <- suppressWarnings(validate_spm_inputs(folder))
  testthat::expect_equal(
    inputs$resolved_species[[1]]$population_weights$male,
    inputs$resolved_species[[1]]$population_weights$female
  )
  testthat::expect_identical(
    inputs$resolved_species[[1]]$male_population_substitute,
    "female_population"
  )
})

testthat::test_that("failed retry preserves the previous successful provenance", {
  folder <- local_spm_input_fixture()
  old <- list(
    status = "success",
    run_id = "previous-success",
    output_hashes = list(spm_detail.csv = "abc")
  )
  jsonlite::write_json(
    old,
    file.path(folder, "spm_run_provenance.json"),
    auto_unbox = TRUE
  )
  jsonlite::write_json(
    old,
    file.path(folder, "spm_last_success_provenance.json"),
    auto_unbox = TRUE
  )
  testthat::expect_snapshot(error = TRUE, runSPM(folder, run = TRUE))
  testthat::expect_identical(
    jsonlite::read_json(file.path(
      folder,
      "spm_run_history",
      "previous-success.json"
    )),
    old
  )
  testthat::expect_identical(
    jsonlite::read_json(file.path(folder, "spm_last_success_provenance.json")),
    old
  )
  history <- list.files(file.path(folder, "spm_run_history"))
  testthat::expect_length(history, 2)
})

testthat::test_that("stock recruitment fits require a small gradient and positive Hessian", {
  folder <- withr::local_tempdir()
  writeLines(
    "# Number of parameters = 2 Maximum gradient component = 1e-7",
    file.path(folder, "spm.par")
  )
  con <- file(file.path(folder, "admodel.hes"), "wb")
  writeBin(2L, con, size = 4)
  writeBin(c(2, 0.5, 0.5, 1), con, size = 8)
  close(con)
  diagnostics <- spm_verify_convergence(folder, 2)
  testthat::expect_equal(diagnostics$maximum_gradient, 1e-7)
  testthat::expect_identical(diagnostics$hessian_positive_definite, TRUE)
  testthat::expect_identical(
    spm_verify_convergence(folder, 1)$applicable,
    FALSE
  )
  writeLines(
    "# Number of parameters = 2 Maximum gradient component = 0.001",
    file.path(folder, "spm.par")
  )
  testthat::expect_snapshot(error = TRUE, spm_verify_convergence(folder, 2))
  writeLines(
    "# Number of parameters = 2 Maximum gradient component = 1e-7",
    file.path(folder, "spm.par")
  )
  con <- file(file.path(folder, "admodel.hes"), "wb")
  writeBin(2L, con, size = 4)
  writeBin(c(1, 0, 0, -1), con, size = 8)
  close(con)
  testthat::expect_snapshot(error = TRUE, spm_verify_convergence(folder, 2))
})

testthat::test_that("receipt uses the native zero-variation cutoff", {
  folder <- local_spm_input_fixture()
  spm_fixture_edit(
    folder,
    "pm.prj",
    "80 100 120 100",
    "100 100.0001 100 100.0001"
  )
  write_spm_metadata(folder, spm_fixture_metadata())
  inputs <- validate_spm_inputs(folder)
  receipt <- data.frame(
    format_version = 2,
    stock_file = "pm.prj",
    nsexes = 2,
    nages = 15,
    recruitment_basis = 1,
    mean_recruitment_total = 100.00005,
    mean_recruitment_female = 50.000025,
    recruitment_cv = 0,
    abundance_unit = 1,
    weight_unit = 1,
    biomass_unit = 1,
    male_substitute = 0
  )
  path <- file.path(folder, "spm_input_receipt.tsv")
  readr::write_tsv(receipt, path)
  testthat::expect_equal(spm_verify_receipt(path, inputs)$recruitment_cv, 0)
  receipt$recruitment_basis <- 2
  readr::write_tsv(receipt, path)
  testthat::expect_snapshot(error = TRUE, spm_verify_receipt(path, inputs))
})

testthat::test_that("duplicate projection keys are rejected before publication", {
  folder <- local_spm_input_fixture()
  write_spm_metadata(folder, spm_fixture_metadata())
  inputs <- validate_spm_inputs(folder)
  writeLines(
    "Finished simulations using standard (avg, var) stochastic approach",
    file.path(folder, "spm_stdout.log")
  )
  writeLines(character(), file.path(folder, "spm_stderr.log"))
  detail <- data.frame(
    Stock = "weights_audit",
    Alt = 5,
    Sim = 1,
    Year = c(2026, 2026, 2028),
    SSB = 1,
    Rec = 100,
    Tot_biom = 1,
    F = 0,
    Catch = 0,
    ABC = 0,
    OFL = 0
  )
  readr::write_csv(detail, file.path(folder, "spm_detail.csv"))
  testthat::expect_snapshot(
    error = TRUE,
    spm_verify_fresh_output(folder, inputs, 0L)
  )
})

testthat::test_that("a publication copy failure restores every previous output", {
  destination <- withr::local_tempdir()
  stage <- withr::local_tempdir()
  for (name in c("first.csv", "second.csv")) {
    writeLines(paste("old", name), file.path(destination, name))
    writeLines(paste("new", name), file.path(stage, name))
  }
  writeLines(
    "prior successful provenance",
    file.path(destination, "spm_last_success_provenance.json")
  )
  testthat::local_mocked_bindings(spm_copy_output = function(from, to) {
    if (basename(from) == "second.csv") {
      return(FALSE)
    }
    file.copy(from, to, overwrite = TRUE)
  })
  testthat::expect_snapshot(
    error = TRUE,
    spm_publish_outputs(
      stage,
      destination,
      c("first.csv", "second.csv"),
      list()
    )
  )
  testthat::expect_identical(
    readLines(file.path(destination, "first.csv")),
    "old first.csv"
  )
  testthat::expect_identical(
    readLines(file.path(destination, "second.csv")),
    "old second.csv"
  )
  testthat::expect_identical(
    readLines(file.path(destination, "spm_last_success_provenance.json")),
    "prior successful provenance"
  )
})

testthat::test_that("a provenance failure rolls back output publication", {
  destination <- withr::local_tempdir()
  stage <- withr::local_tempdir()
  writeLines("old", file.path(destination, "spm_detail.csv"))
  writeLines("new", file.path(stage, "spm_detail.csv"))
  writeLines(
    "prior successful provenance",
    file.path(destination, "spm_last_success_provenance.json")
  )
  testthat::local_mocked_bindings(spm_write_provenance = function(...) {
    stop("Provenance save failed.")
  })
  testthat::expect_snapshot(
    error = TRUE,
    spm_publish_outputs(stage, destination, "spm_detail.csv", list())
  )
  testthat::expect_identical(
    readLines(file.path(destination, "spm_detail.csv")),
    "old"
  )
  testthat::expect_identical(
    readLines(file.path(destination, "spm_last_success_provenance.json")),
    "prior successful provenance"
  )
})

testthat::test_that("constant buffer flag is an integer", {
  folder <- local_spm_input_fixture()
  spm_fixture_edit(folder, "pm.prj", "# Const_Buffer\n0", "# Const_Buffer\n0.5")
  testthat::expect_snapshot(
    error = TRUE,
    write_spm_metadata(folder, spm_fixture_metadata())
  )
})

testthat::test_that("species share abundance and biomass units for aggregate limits", {
  folder <- local_spm_input_fixture()
  file.copy(file.path(folder, "pm.prj"), file.path(folder, "second.prj"))
  spm_fixture_edit(folder, "second.prj", "weights_audit", "second_stock")
  spm_fixture_edit(folder, "spm.dat", "# nspp\n1", "# nspp\n2")
  spm_fixture_edit(
    folder,
    "spm.dat",
    "# spp_file_name\npm.prj",
    "# spp_file_name\npm.prj second.prj"
  )
  spm_fixture_edit(
    folder,
    "spm.dat",
    "# ABC_Multiplier\n1",
    "# ABC_Multiplier\n1 1"
  )
  spm_fixture_edit(folder, "spm.dat", "# N_scalar\n1", "# N_scalar\n1 1")
  spm_fixture_edit(folder, "spm.dat", "# Alt4_SPR\n0.6", "# Alt4_SPR\n0.6 0.6")
  spm_fixture_edit(folder, "spm.dat", "# tac_ind\n1", "# tac_ind\n1 1")
  spm_fixture_edit(folder, "spm.dat", "2026 0", "2026 0 0")
  metadata <- rep(spm_fixture_metadata(), 2)
  metadata[[2]]$file <- "second.prj"
  metadata[[2]]$units <- list(
    abundance = "thousand_fish",
    weight = "kg",
    biomass = "t"
  )
  testthat::expect_snapshot(error = TRUE, write_spm_metadata(folder, metadata))
  metadata[[2]]$units <- metadata[[1]]$units
  testthat::expect_no_condition(write_spm_metadata(folder, metadata))
})
