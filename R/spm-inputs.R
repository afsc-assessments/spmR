#' Write explicit recruitment and population-weight metadata
#'
#' Version 2 metadata accompanies the positional SPM input files. Split-sex
#' species files contain `wt_M` immediately after `wt_F`; pooled-sex files use
#' `wt_F` for both internal sex groups. Recruitment must be identified as total
#' across sexes or per sex. Population weights are distinct from fishery and
#' spawning weights. Version-2 projections use `TAC_ABC = 1` and do not read
#' `tacpar.dat`. Inputs are validated before replacing an existing metadata file.
#'
#' @param dirname Directory containing `spm.dat` and species files.
#' @param species A list of species records. Each record contains `file`,
#'   `recruitment_basis` (`"total"` or `"per_sex"`), consecutive integer `ages`,
#'   `units`, and `population_weights`. Units contain `abundance` (`"fish"`,
#'   `"thousand_fish"`, or `"million_fish"`), `weight` (`"kg"`), and matching
#'   `biomass` (`"kg"`, `"t"`, or `"thousand_t"`). Each population-weight record
#'   contains `ages` and `values`. Female population weights are required.
#'   Split-sex male weights are required unless `male_population_substitute`
#'   explicitly specifies `"female_population"` or `"mean_male_fishery"`.
#'   Pooled-sex models can use the female population vector for both internal
#'   sex groups; that assumption is recorded and reported.
#' @param metadata Relative metadata filename. Defaults to `spm_metadata.json`.
#' @param strict Logical. If `TRUE`, reject unknown metadata fields; otherwise
#'   report them as warnings. Required fields and scientific checks always apply.
#' @return `write_spm_metadata()` invisibly returns the validated metadata path.
#'   `validate_spm_inputs()` returns an `spm_inputs` list containing parsed inputs,
#'   resolved population weights, unit declarations, and substitutions.
#' @export
write_spm_metadata <- function(
  dirname,
  species,
  metadata = "spm_metadata.json",
  strict = TRUE
) {
  dirname <- normalizePath(dirname, winslash = "/", mustWork = TRUE)
  metadata <- spm_metadata_filename(metadata)
  document <- list(format_version = 2L, species = species)
  validated <- spm_validate_document(dirname, document, strict)
  if (tolower(metadata) %in% tolower(validated$input_files)) {
    stop(
      "metadata filename must differ from legacy input filenames.",
      call. = FALSE
    )
  }
  destination <- file.path(dirname, metadata)
  temporary <- tempfile(".spm-metadata-", tmpdir = dirname, fileext = ".json")
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(
    document,
    temporary,
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = NA,
    null = "null"
  )
  if (!file.rename(temporary, destination)) {
    stop(
      "Could not atomically replace metadata file: ",
      metadata,
      call. = FALSE
    )
  }
  invisible(destination)
}

#' @rdname write_spm_metadata
#' @export
validate_spm_inputs <- function(
  dirname,
  metadata = "spm_metadata.json",
  strict = TRUE
) {
  dirname <- normalizePath(dirname, winslash = "/", mustWork = TRUE)
  metadata <- spm_metadata_filename(metadata)
  path <- file.path(dirname, metadata)
  if (!file.exists(path)) {
    stop(
      "Missing ",
      metadata,
      ": declare recruitment basis, units, and female/male ",
      "population weights with write_spm_metadata() before running projections.",
      call. = FALSE
    )
  }
  document <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) {
      stop("Invalid metadata JSON: ", conditionMessage(e), call. = FALSE)
    }
  )
  result <- spm_validate_document(dirname, document, strict)
  if (tolower(metadata) %in% tolower(result$input_files)) {
    stop(
      "metadata filename must differ from legacy input filenames.",
      call. = FALSE
    )
  }
  result$metadata_file <- metadata
  result$input_files <- c(result$input_files, metadata)
  result
}

spm_metadata_filename <- function(x) {
  x <- spm_input_filename(x, "metadata")
  if (
    !grepl("[.]json$", x) ||
      tolower(x) %in%
        c("spm_run_provenance.json", "spm_last_success_provenance.json")
  ) {
    stop(
      "metadata must use a distinct .json filename reserved for input metadata.",
      call. = FALSE
    )
  }
  x
}

spm_reserved_files <- function() {
  c(
    "spm",
    "spm.exe",
    "spm.dat",
    "spm_input_v2.dat",
    "spm_input_receipt.tsv",
    "spm_detail.csv",
    "spm_summary.csv",
    "spm_stdout.log",
    "spm_stderr.log",
    "spm_run_provenance.json",
    "spm_last_success_provenance.json",
    "spm_metadata.json",
    "means.out",
    "alt_proj.out",
    "percentiles.out",
    "F_profile.out",
    "elasticity.csv",
    "Input_Log.rep",
    "spm.par",
    "spm.rep",
    "spm.std",
    "spm.cor",
    "admodel.hes",
    "admodel.cov",
    "spm.log",
    "fmin.log"
  )
}

spm_input_filename <- function(x, field) {
  if (
    !is.character(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", x) ||
      x %in% c(".", "..")
  ) {
    stop(
      field,
      " must be a plain filename without whitespace or directories.",
      call. = FALSE
    )
  }
  x
}

spm_token_reader <- function(path) {
  if (!file.exists(path)) {
    stop("Missing input file: ", basename(path), call. = FALSE)
  }
  text <- readLines(path, warn = FALSE)
  text <- sub("#.*$", "", text)
  text <- sub("//.*$", "", text)
  tokens <- strsplit(paste(text, collapse = " "), "[[:space:]]+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  position <- 0L
  take <- function(
    n = 1L,
    field,
    numeric = TRUE,
    integer = FALSE,
    lower = -Inf,
    upper = Inf
  ) {
    if (n < 0L || position + n > length(tokens)) {
      stop(basename(path), ": missing values for ", field, ".", call. = FALSE)
    }
    if (n == 0L) {
      return(if (numeric) numeric() else character())
    }
    value <- tokens[seq.int(position + 1L, position + n)]
    position <<- position + n
    if (!numeric) {
      return(value)
    }
    value <- suppressWarnings(as.numeric(value))
    if (
      any(!is.finite(value)) ||
        any(value < lower | value > upper) ||
        (integer && any(value != floor(value)))
    ) {
      stop(
        basename(path),
        ": invalid ",
        field,
        "; expected finite ",
        if (integer) "integer " else "numeric ",
        "values in [",
        lower,
        ", ",
        upper,
        "].",
        call. = FALSE
      )
    }
    value
  }
  done <- function() {
    if (position != length(tokens)) {
      stop(
        basename(path),
        ": unexpected trailing fields (",
        length(tokens) - position,
        " tokens). Check the positional input format.",
        call. = FALSE
      )
    }
  }
  list(take = take, done = done)
}

spm_read_legacy <- function(dirname) {
  p <- spm_token_reader(file.path(dirname, "spm.dat"))
  get <- p$take
  s <- list(run_name = get(field = "run_name", numeric = FALSE))
  s$Tier <- get(field = "Tier", integer = TRUE, lower = 1, upper = 3)
  s$nalts <- get(field = "nalts", integer = TRUE, lower = 1, upper = 20)
  s$alt_list <- get(s$nalts, "alt_list", integer = TRUE)
  allowed <- c(1:7, 66, 77, 97, 98)
  if (any(!s$alt_list %in% allowed) || anyDuplicated(s$alt_list)) {
    stop(
      "spm.dat: alternatives must be unique supported scenario codes.",
      call. = FALSE
    )
  }
  s$TAC_ABC <- get(field = "TAC_ABC", integer = TRUE)
  if (s$TAC_ABC != 1) {
    stop(
      "Version 2 projections currently support TAC_ABC=1; external and fitted TAC modes require separate validation.",
      call. = FALSE
    )
  }
  s$SrType <- get(field = "SrType", integer = TRUE)
  if (!s$SrType %in% c(1, 2, 4)) {
    stop("Version 2 supports SrType 1, 2, or 4.", call. = FALSE)
  }
  s$Rec_Gen <- get(field = "Rec_Gen", integer = TRUE, lower = 1, upper = 2)
  s$Fmsy_F35 <- get(field = "Fmsy_F35", integer = TRUE, lower = 0, upper = 1)
  s$Rec_Cond <- get(field = "Rec_Cond", lower = 0)
  s$Write_Big <- get(field = "Write_Big", integer = TRUE, lower = 0, upper = 1)
  s$npro <- get(field = "npro", integer = TRUE, lower = 1, upper = 299)
  s$nsims <- get(field = "nsims", integer = TRUE, lower = 1)
  s$styr <- get(field = "styr", integer = TRUE)
  s$nyrs_catch_in <- get(
    field = "nyrs_catch_in",
    integer = TRUE,
    lower = 0,
    upper = s$npro
  )
  s$nspp <- get(field = "nspp", integer = TRUE, lower = 1, upper = 20)
  s$OY_min <- get(field = "OY_min", lower = 0)
  s$OY_max <- get(field = "OY_max", lower = s$OY_min)
  s$spp_files <- get(s$nspp, "spp_file_name", numeric = FALSE)
  s$spp_files <- vapply(
    s$spp_files,
    spm_input_filename,
    character(1),
    field = "species file",
    USE.NAMES = FALSE
  )
  if (
    anyDuplicated(tolower(s$spp_files)) ||
      any(tolower(s$spp_files) %in% tolower(spm_reserved_files()))
  ) {
    stop(
      "spm.dat: species files must have distinct, unreserved filenames.",
      call. = FALSE
    )
  }
  s$ABC_Multiplier <- get(s$nspp, "ABC_Multiplier", lower = 0)
  s$N_scalar <- get(s$nspp, "N_scalar", lower = 1, upper = 1)
  s$Alt4_SPR <- get(s$nspp, "Alt4_SPR", lower = .Machine$double.eps, upper = 1)
  s$ntacspp <- get(field = "ntacspp", integer = TRUE, lower = 1, upper = 20)
  s$tac_ind <- get(
    s$nspp,
    "tac_ind",
    integer = TRUE,
    lower = 1,
    upper = s$ntacspp
  )
  s$Obs_Catch <- matrix(
    get(s$nyrs_catch_in * (s$nspp + 1L), "Obs_Catch", lower = 0),
    nrow = s$nyrs_catch_in,
    ncol = s$nspp + 1L,
    byrow = TRUE
  )
  if (
    s$nyrs_catch_in > 0 &&
      any(s$Obs_Catch[, 1L] != s$styr + seq_len(s$nyrs_catch_in) - 1L)
  ) {
    stop(
      "spm.dat: fixed catch years must start at styr and be consecutive.",
      call. = FALSE
    )
  }
  p$done()
  spp <- lapply(s$spp_files, function(file) {
    r <- spm_token_reader(file.path(dirname, file))
    get <- r$take
    x <- list(file = file, spname = get(field = "spname", numeric = FALSE))
    x$SSL_spp <- get(field = "SSL_spp", integer = TRUE, lower = 0, upper = 1)
    x$Const_Buffer <- get(
      field = "Const_Buffer",
      integer = TRUE,
      lower = 0,
      upper = 1
    )
    x$ngear <- get(field = "ngear", integer = TRUE, lower = 1, upper = 5)
    x$nsexes <- get(field = "nsexes", integer = TRUE, lower = 1, upper = 2)
    x$avg_5yrF <- get(field = "avg_5yrF", lower = 0)
    x$FABC_Adj <- get(field = "FABC_Adj", lower = 0)
    x$SPR_abc <- get(field = "SPR_abc", lower = .Machine$double.eps, upper = 1)
    x$SPR_ofl <- get(field = "SPR_ofl", lower = .Machine$double.eps, upper = 1)
    x$spawnmo <- get(field = "spawnmo", lower = 1, upper = 13)
    x$nages <- get(field = "nages", integer = TRUE, lower = 2, upper = 69)
    n <- x$nages
    g <- x$ngear
    x$Frat <- get(g, "Frat", lower = 0, upper = 1)
    if (abs(sum(x$Frat) - 1) > 1e-8) {
      stop(file, ": Frat must sum to one.", call. = FALSE)
    }
    x$M_F <- get(n, "M_F", lower = .Machine$double.eps)
    x$M_M <- if (x$nsexes == 2) {
      get(n, "M_M", lower = .Machine$double.eps)
    } else {
      x$M_F
    }
    x$pmature_F <- get(n, "pmature_F", lower = 0, upper = 1)
    x$pmature_M <- if (x$nsexes == 2) {
      get(n, "pmature_M", lower = 0, upper = 1)
    } else {
      x$pmature_F
    }
    x$wt_F <- get(n, "wt_F", lower = .Machine$double.eps)
    x$wt_M <- if (x$nsexes == 2) {
      get(n, "wt_M", lower = .Machine$double.eps)
    } else {
      x$wt_F
    }
    x$wt_gear_F <- matrix(
      get(n * g, "wt_gear_F", lower = .Machine$double.eps),
      nrow = g,
      byrow = TRUE
    )
    x$wt_gear_M <- if (x$nsexes == 2) {
      matrix(
        get(n * g, "wt_gear_M", lower = .Machine$double.eps),
        nrow = g,
        byrow = TRUE
      )
    } else {
      x$wt_gear_F
    }
    x$sel_F <- matrix(get(n * g, "sel_F", lower = 0), nrow = g, byrow = TRUE)
    x$sel_M <- if (x$nsexes == 2) {
      matrix(get(n * g, "sel_M", lower = 0), nrow = g, byrow = TRUE)
    } else {
      x$sel_F
    }
    if (any(rowSums(x$sel_F) <= 0) || any(rowSums(x$sel_M) <= 0)) {
      stop(
        file,
        ": each sex/fleet selectivity vector must contain a positive value.",
        call. = FALSE
      )
    }
    x$n0_F <- get(n, "n0_F", lower = 0)
    x$n0_M <- if (x$nsexes == 2) get(n, "n0_M", lower = 0) else x$n0_F / 2
    x$nrec <- get(field = "nrec", integer = TRUE, lower = 2, upper = 69)
    if (x$nrec + s$npro + 1L > 300L) {
      stop(
        file,
        ": recruitment history plus projection exceeds native storage.",
        call. = FALSE
      )
    }
    x$R <- get(x$nrec, "R", lower = .Machine$double.eps)
    x$SSB <- get(x$nrec, "SSB", lower = .Machine$double.eps)
    r$done()
    x
  })
  if (anyDuplicated(vapply(spp, `[[`, "", "spname"))) {
    stop(
      "Species stock names must be unique in projection output.",
      call. = FALSE
    )
  }
  list(
    spm = s,
    species = spp,
    input_files = c("spm.dat", s$spp_files)
  )
}

spm_schema_fields <- function(x, allowed, required, label, strict) {
  if (!is.list(x) || is.null(names(x)) || anyDuplicated(names(x))) {
    stop(label, " must be an object with unique named fields.", call. = FALSE)
  }
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      label,
      ": missing ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  extra <- setdiff(names(x), allowed)
  if (length(extra)) {
    text <- paste0(
      label,
      ": unknown fields: ",
      paste(extra, collapse = ", "),
      "."
    )
    if (strict) stop(text, call. = FALSE) else warning(text, call. = FALSE)
  }
}

spm_numeric_vector <- function(x, n, label, ages = FALSE) {
  x <- unlist(x, use.names = FALSE)
  if (
    !is.numeric(x) ||
      length(x) != n ||
      any(!is.finite(x)) ||
      if (ages) any(x < 0 | x != floor(x)) else any(x <= 0)
  ) {
    stop(
      label,
      ": require exactly ",
      n,
      " finite ",
      if (ages) "nonnegative integer ages" else "positive numeric values",
      ".",
      call. = FALSE
    )
  }
  as.numeric(x)
}

spm_validate_document <- function(dirname, document, strict) {
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) {
    stop("strict must be TRUE or FALSE.", call. = FALSE)
  }
  spm_schema_fields(
    document,
    c("format_version", "species"),
    c("format_version", "species"),
    "metadata",
    strict
  )
  if (
    !is.numeric(document$format_version) ||
      length(document$format_version) != 1L ||
      is.na(document$format_version) ||
      document$format_version != 2
  ) {
    stop("Unsupported metadata format_version; expected 2.", call. = FALSE)
  }
  inputs <- spm_read_legacy(dirname)
  records <- document$species
  if (!is.list(records) || length(records) != inputs$spm$nspp) {
    stop(
      "metadata species must contain one record per species in spm.dat order.",
      call. = FALSE
    )
  }
  normalized <- vector("list", length(records))
  for (i in seq_along(records)) {
    z <- records[[i]]
    x <- inputs$species[[i]]
    label <- paste0("metadata species ", i, " (", x$file, ")")
    required <- c(
      "file",
      "recruitment_basis",
      "ages",
      "units",
      "population_weights"
    )
    spm_schema_fields(
      z,
      c(required, "male_population_substitute"),
      required,
      label,
      strict
    )
    if (!identical(z$file, x$file)) {
      stop(label, ": file must match spm.dat species order.", call. = FALSE)
    }
    basis <- z$recruitment_basis
    if (
      !is.character(basis) ||
        length(basis) != 1L ||
        !basis %in% c("total", "per_sex")
    ) {
      stop(
        label,
        ": recruitment_basis must be total or per_sex.",
        call. = FALSE
      )
    }
    if (x$nsexes == 1 && basis != "total") {
      stop(
        label,
        ": pooled-sex recruitment_basis must be total.",
        call. = FALSE
      )
    }
    ages <- spm_numeric_vector(
      z$ages,
      x$nages,
      paste(label, "ages"),
      ages = TRUE
    )
    if (any(diff(ages) != 1)) {
      stop(
        label,
        ": ages must be consecutive and in increasing order.",
        call. = FALSE
      )
    }
    units <- z$units
    spm_schema_fields(
      units,
      c("abundance", "weight", "biomass"),
      c("abundance", "weight", "biomass"),
      paste(label, "units"),
      strict
    )
    choices <- c(fish = "kg", thousand_fish = "t", million_fish = "thousand_t")
    if (
      !is.character(units$abundance) ||
        length(units$abundance) != 1L ||
        !units$abundance %in% names(choices) ||
        !identical(units$weight, "kg") ||
        !identical(units$biomass, unname(choices[units$abundance]))
    ) {
      stop(
        label,
        ": coherent units required: fish/kg/kg, thousand_fish/kg/t, or million_fish/kg/thousand_t.",
        call. = FALSE
      )
    }
    weights <- z$population_weights
    spm_schema_fields(
      weights,
      c("female", "male"),
      "female",
      paste(label, "population_weights"),
      strict
    )
    read_weights <- function(value, sex) {
      title <- paste(label, sex, "population weights")
      spm_schema_fields(
        value,
        c("ages", "values"),
        c("ages", "values"),
        title,
        strict
      )
      wa <- spm_numeric_vector(
        value$ages,
        x$nages,
        paste(title, "ages"),
        ages = TRUE
      )
      if (!identical(wa, ages)) {
        stop(
          title,
          ": ages must exactly match the declared age order.",
          call. = FALSE
        )
      }
      spm_numeric_vector(value$values, x$nages, title)
    }
    female <- read_weights(weights$female, "female")
    substitute <- z$male_population_substitute
    if (
      !is.null(substitute) &&
        (!is.character(substitute) ||
          length(substitute) != 1L ||
          !substitute %in% c("female_population", "mean_male_fishery"))
    ) {
      stop(
        label,
        ": male_population_substitute must be female_population or mean_male_fishery.",
        call. = FALSE
      )
    }
    if (!is.null(weights$male) && !is.null(substitute)) {
      stop(
        label,
        ": provide male population weights or an explicit substitute, not both.",
        call. = FALSE
      )
    }
    if (!is.null(weights$male)) {
      male <- read_weights(weights$male, "male")
      substitute <- "none"
    } else {
      if (is.null(substitute)) {
        if (x$nsexes == 2) {
          stop(
            label,
            ": missing male population weight-at-age vector; supply it or explicitly declare male_population_substitute.",
            call. = FALSE
          )
        }
        substitute <- "female_population"
      }
      male <- if (substitute == "female_population") {
        female
      } else {
        colMeans(x$wt_gear_M)
      }
      warning(
        label,
        ": male population weights use declared ",
        substitute,
        if (x$nsexes == 1) {
          " (common population weights for pooled sexes)."
        } else {
          "."
        },
        call. = FALSE
      )
    }
    normalized[[i]] <- list(
      file = x$file,
      stock = x$spname,
      nsexes = x$nsexes,
      nages = x$nages,
      ages = ages,
      recruitment_basis = basis,
      units = units,
      population_weights = list(female = female, male = male),
      male_population_substitute = substitute
    )
  }
  run_units <- vapply(
    normalized,
    function(x) paste(x$units$abundance, x$units$biomass),
    character(1)
  )
  if (length(unique(run_units)) != 1L) {
    stop(
      "All species in a projection run must use the same abundance and biomass units because catches and biomass are aggregated against shared OY limits.",
      call. = FALSE
    )
  }
  inputs$metadata <- document
  inputs$resolved_species <- normalized
  inputs$format_version <- 2L
  class(inputs) <- "spm_inputs"
  inputs
}

spm_write_native_input <- function(inputs, path) {
  number_line <- function(x) {
    paste(
      format(x, digits = 17, scientific = TRUE, trim = TRUE),
      collapse = " "
    )
  }
  lines <- paste("SPMR_INPUT_V2 2", inputs$spm$nspp)
  for (z in inputs$resolved_species) {
    lines <- c(
      lines,
      paste(
        z$file,
        z$nsexes,
        z$nages,
        match(z$recruitment_basis, c("total", "per_sex")),
        match(z$units$abundance, c("fish", "thousand_fish", "million_fish")),
        1,
        match(z$units$biomass, c("kg", "t", "thousand_t")),
        match(
          z$male_population_substitute,
          c("none", "female_population", "mean_male_fishery")
        ) -
          1L
      ),
      paste(z$ages, collapse = " "),
      number_line(z$population_weights$female),
      number_line(z$population_weights$male)
    )
  }
  writeLines(c(lines, "END_SPMR_INPUT_V2"), path)
  invisible(path)
}
