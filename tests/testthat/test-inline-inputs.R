local_inline_fixture <- function(env = parent.frame()) {
  folder <- withr::local_tempdir(.local_envir = env)
  file.copy(
    list.files(test_path("fixtures", "input-v2"), full.names = TRUE),
    folder
  )
  folder
}

inline_species_record <- function() {
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

test_that("default metadata stays with its species input", {
  folder <- local_inline_fixture()
  before <- spm_read_legacy(folder)
  paths <- write_spm_metadata(folder, inline_species_record())
  inputs <- validate_spm_inputs(folder)
  expect_identical(
    paths,
    normalizePath(file.path(folder, "pm.prj"), winslash = "/")
  )
  expect_identical(inputs$metadata_storage, "inline")
  expect_null(inputs$metadata_file)
  expect_identical(inputs$input_files, c("spm.dat", "pm.prj"))
  expect_equal(spm_read_legacy(folder), before)
  expect_identical(sort(list.files(folder)), c("pm.prj", "spm.dat"))
  expect_equal(
    inputs$resolved_species[[1]]$population_weights$male,
    (1:15) / 20
  )
})

test_that("inline writes replace one block and override stale JSON", {
  folder <- local_inline_fixture()
  records <- inline_species_record()
  write_spm_metadata(folder, records)
  records[[1]]$population_weights$male$values <- (1:15) / 30
  write_spm_metadata(folder, records)
  writeLines("stale JSON", file.path(folder, "spm_metadata.json"))
  block <- spm_inline_block(file.path(folder, "pm.prj"))
  expect_length(block$indices, 6L)
  expect_equal(
    validate_spm_inputs(folder)$resolved_species[[1]]$population_weights$male,
    (1:15) / 30
  )
  before <- spm_sha256(file.path(folder, "pm.prj"))
  write_spm_metadata(folder, records)
  expect_identical(spm_sha256(file.path(folder, "pm.prj")), before)
})

test_that("explicit JSON remains available and automatic discovery falls back", {
  folder <- local_inline_fixture()
  records <- inline_species_record()
  before <- spm_sha256(file.path(folder, "pm.prj"))
  write_spm_metadata(folder, records, metadata = "spm_metadata.json")
  expect_identical(spm_sha256(file.path(folder, "pm.prj")), before)
  expect_identical(validate_spm_inputs(folder)$metadata_storage, "json")
  records[[1]]$population_weights$male$values <- (1:15) / 30
  write_spm_metadata(folder, records)
  expect_equal(
    validate_spm_inputs(folder)$resolved_species[[1]]$population_weights$male,
    (1:15) / 30
  )
  expect_equal(
    validate_spm_inputs(
      folder,
      metadata = "spm_metadata.json"
    )$resolved_species[[1]]$population_weights$male,
    (1:15) / 20
  )
})

test_that("all positive finite population weights round trip", {
  folder <- local_inline_fixture()
  records <- inline_species_record()
  records[[1]]$population_weights$female$values[1] <- 1e-20
  records[[1]]$population_weights$male$values[1] <- 1e-25
  write_spm_metadata(folder, records)
  weights <- validate_spm_inputs(folder)$resolved_species[[
    1
  ]]$population_weights
  expect_identical(weights$female[1], 1e-20)
  expect_identical(weights$male[1], 1e-25)
})

test_that("incomplete embedded metadata cannot fall back to JSON", {
  folder <- local_inline_fixture()
  records <- inline_species_record()
  write_spm_metadata(folder, records, metadata = "spm_metadata.json")
  write_spm_metadata(folder, records)
  path <- file.path(folder, "pm.prj")
  lines <- readLines(path)
  writeLines(lines[-length(lines)], path)
  expect_snapshot(error = TRUE, validate_spm_inputs(folder))
})

test_that("inline metadata uses native token semantics", {
  folder <- local_inline_fixture()
  write_spm_metadata(folder, inline_species_record())
  path <- file.path(folder, "pm.prj")
  lines <- readLines(path)
  header <- grep("^# pm.prj ", lines)
  lines[header] <- paste(lines[header], "# hidden text")
  writeLines(lines, path)
  expect_snapshot(error = TRUE, validate_spm_inputs(folder))
})

test_that("inline header codes require native integer spelling", {
  folder <- local_inline_fixture()
  write_spm_metadata(folder, inline_species_record())
  path <- file.path(folder, "pm.prj")
  lines <- sub(
    "SPMR_INPUT_V2 2 1",
    "SPMR_INPUT_V2 2e0 1",
    readLines(path),
    fixed = TRUE
  )
  writeLines(lines, path)
  expect_snapshot(error = TRUE, validate_spm_inputs(folder))
})

test_that("a failed inline replacement preserves the source bytes", {
  folder <- local_inline_fixture()
  before <- spm_sha256(list.files(folder, full.names = TRUE))
  local_mocked_bindings(spm_replace_input = function(...) FALSE)
  expect_snapshot(
    error = TRUE,
    write_spm_metadata(folder, inline_species_record())
  )
  expect_identical(spm_sha256(file.path(folder, names(before))), before)
  expect_identical(
    sort(list.files(folder, all.files = TRUE, no.. = TRUE)),
    c("pm.prj", "spm.dat")
  )
})

test_that("a later inline replacement failure restores earlier stocks", {
  folder <- local_inline_fixture()
  inputs <- spm_validate_document(
    folder,
    list(format_version = 2L, species = inline_species_record()),
    TRUE
  )
  file.copy(file.path(folder, "pm.prj"), file.path(folder, "second.prj"))
  inputs$spm$spp_files <- c("pm.prj", "second.prj")
  inputs$spm$nspp <- 2L
  inputs$resolved_species <- rep(inputs$resolved_species, 2)
  inputs$resolved_species[[2]]$file <- "second.prj"
  before <- spm_sha256(list.files(folder, full.names = TRUE))
  local_mocked_bindings(spm_replace_input = function(from, to) {
    if (basename(to) == "second.prj") {
      return(FALSE)
    }
    file.rename(from, to)
  })
  expect_snapshot(error = TRUE, spm_write_inline_input(inputs, folder))
  expect_identical(spm_sha256(file.path(folder, names(before))), before)
  expect_identical(
    sort(list.files(folder, all.files = TRUE, no.. = TRUE)),
    c("pm.prj", "second.prj", "spm.dat")
  )
})

test_that("new native execution needs neither metadata sidecar", {
  executable <- Sys.getenv("SPMR_TEST_EXE")
  skip_if(!nzchar(executable), "Set SPMR_TEST_EXE for native execution")
  capabilities <- spm_verify_capabilities(executable)
  skip_if(
    !"SPMR_INLINE_INPUT=1" %in% capabilities$stdout,
    "Compile the current inline-capable engine"
  )
  folder <- local_inline_fixture()
  write_spm_metadata(folder, inline_species_record())
  binary <- file.path(
    folder,
    if (.Platform$OS.type == "windows") "spm.exe" else "spm"
  )
  file.copy(executable, binary, copy.mode = TRUE)
  result <- runSPM(folder, run = TRUE)
  expect_equal(nrow(result), 3L)
  expect_identical(
    file.exists(file.path(folder, c("spm_metadata.json", "spm_input_v2.dat"))),
    c(FALSE, FALSE)
  )
  provenance <- jsonlite::read_json(file.path(
    folder,
    "spm_last_success_provenance.json"
  ))
  expect_identical(provenance$native_input_storage, "inline")
  expect_named(provenance$native_input_sha256, "pm.prj")
  expected <- digest::digest(
    file.path(folder, "spm_detail.csv"),
    algo = "sha256",
    file = TRUE
  )
  status <- spm_call_executable(
    binary,
    character(),
    folder,
    file.path(folder, "direct.stdout"),
    file.path(folder, "direct.stderr")
  )
  expect_identical(status, 0L)
  expect_identical(
    digest::digest(
      file.path(folder, "spm_detail.csv"),
      algo = "sha256",
      file = TRUE
    ),
    expected
  )
})

test_that("older format-2 engines retain the generated sidecar path", {
  executable <- Sys.getenv("SPMR_LEGACY_TEST_EXE")
  skip_if(
    !nzchar(executable),
    "Set SPMR_LEGACY_TEST_EXE for old-engine compatibility"
  )
  folder <- local_inline_fixture()
  write_spm_metadata(folder, inline_species_record())
  binary <- file.path(
    folder,
    if (.Platform$OS.type == "windows") "spm.exe" else "spm"
  )
  file.copy(executable, binary, copy.mode = TRUE)
  result <- runSPM(folder, run = TRUE)
  expect_equal(nrow(result), 3L)
  provenance <- jsonlite::read_json(file.path(
    folder,
    "spm_last_success_provenance.json"
  ))
  expect_identical(provenance$native_input_storage, "sidecar")
  expect_match(provenance$native_input_sha256, "^[0-9a-f]{64}$")
  expect_identical(file.exists(file.path(folder, "spm_input_v2.dat")), TRUE)
})
