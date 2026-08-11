#' Summarize the seven Tier 3 projection alternatives
#'
#' Converts simulation-level SPM output into one row per Tier 3 alternative.
#' Results for the requested projection years are placed in separate columns so
#' the returned object can be printed directly as an assessment table.
#'
#' @param x An object returned by [runSPM()] or a data frame accepted by
#'   [as_spm_result()].
#' @param years Projection years to include. The default uses all years.
#' @param scenario_names Optional character vector of seven scenario names.
#' @param digits Number of digits used to round summarized values.
#'
#' @return A tibble with one row per Tier 3 alternative and year-specific mean
#'   Catch, ABC, OFL, spawning biomass, fishing mortality, and B/B35 columns.
#' @export
#'
#' @examples
#' detail <- data.frame(
#'   Stock = "example", Alt = rep(1:7, each = 2), Sim = 1,
#'   Year = rep(2027:2028, 7), Catch = 100, SSB = 500,
#'   ABC = 110, OFL = 120, F = 0.2, B35 = 350
#' )
#' tier3_scenario_table(detail, years = 2027:2028)
tier3_scenario_table <- function(
    x,
    years = NULL,
    scenario_names = c(
      "Maximum permissible ABC",
      "Author-specified ABC",
      "Average recent F",
      "Alternative SPR rate",
      "No fishing",
      "OFL threshold determination",
      "Status-determination ramp"
    ),
    digits = 1) {
  x <- as_spm_result(x)
  if (!"Alt" %in% names(x)) {
    suppressWarnings(x$Alt <- as.integer(x$Scenario))
  }
  if (anyNA(x$Alt) || !all(x$Alt %in% 1:7)) {
    stop("Tier 3 alternatives must be identified by `Alt` values 1 through 7.",
         call. = FALSE)
  }
  if (length(scenario_names) != 7L) {
    stop("`scenario_names` must contain exactly seven names.", call. = FALSE)
  }
  available_years <- sort(unique(x$Year))
  if (is.null(years)) years <- available_years
  years <- as.numeric(years)
  missing_years <- setdiff(years, available_years)
  if (length(missing_years)) {
    stop(
      "Requested projection year(s) not found: ",
      paste(missing_years, collapse = ", "), ".",
      call. = FALSE
    )
  }
  required <- c("F", "B35")
  missing_columns <- setdiff(required, names(x))
  if (length(missing_columns)) {
    stop(
      "Tier 3 scenario table requires columns: ",
      paste(missing_columns, collapse = ", "), ".",
      call. = FALSE
    )
  }

  out <- x |>
    dplyr::filter(.data$Year %in% years) |>
    dplyr::group_by(.data$Alt, .data$Year) |>
    dplyr::summarise(
      Catch = mean(.data$Catch, na.rm = TRUE),
      ABC = mean(.data$ABC, na.rm = TRUE),
      OFL = mean(.data$OFL, na.rm = TRUE),
      SSB = mean(.data$SSB, na.rm = TRUE),
      F = mean(.data$F, na.rm = TRUE),
      B_B35 = mean(.data$SSB / .data$B35, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = "Year",
      values_from = c("Catch", "ABC", "OFL", "SSB", "F", "B_B35"),
      names_glue = "{.value}_{Year}"
    ) |>
    dplyr::mutate(
      Scenario = scenario_names[.data$Alt],
      .after = "Alt"
    ) |>
    dplyr::arrange(.data$Alt)

  numeric_columns <- vapply(out, is.numeric, logical(1))
  numeric_columns[match("Alt", names(out))] <- FALSE
  out[numeric_columns] <- lapply(out[numeric_columns], round, digits = digits)
  tibble::as_tibble(out)
}
