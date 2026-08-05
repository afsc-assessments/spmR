#' Validate and standardize projection model results
#'
#' `as_spm_result()` converts a model output data frame to the common result
#' format used by spmR. Legacy SPM output is supported by deriving `Scenario`
#' from `Alt` when needed. Model-specific columns are preserved.
#'
#' @param x A data frame containing projection model results.
#'
#' @return `x` with a `Scenario` column and the additional S3 class
#'   `spm_result`.
#' @export
#'
#' @examples
#' result <- data.frame(
#'   Stock = "example",
#'   Alt = 1,
#'   Sim = 1,
#'   Year = 2025,
#'   Catch = 100,
#'   SSB = 500,
#'   ABC = 110,
#'   OFL = 120
#' )
#' as_spm_result(result)
as_spm_result <- function(x) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame.", call. = FALSE)
  }

  if (!"Scenario" %in% names(x)) {
    if (!"Alt" %in% names(x)) {
      stop("`x` must contain `Scenario` or the legacy `Alt` column.", call. = FALSE)
    }
    x$Scenario <- as.character(x$Alt)
  }

  required <- c(
    "Stock", "Scenario", "Sim", "Year", "Catch", "SSB", "ABC", "OFL"
  )
  missing_columns <- setdiff(required, names(x))
  if (length(missing_columns) > 0) {
    stop(
      "Missing required result columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  x$Stock <- as.character(x$Stock)
  x$Scenario <- as.character(x$Scenario)

  key <- c("Stock", "Scenario", "Sim", "Year")
  if (anyNA(x[key])) {
    stop("Result key columns cannot contain missing values.", call. = FALSE)
  }
  if (anyDuplicated(x[key])) {
    stop(
      "Result rows must be unique by Stock, Scenario, Sim, and Year.",
      call. = FALSE
    )
  }

  numeric_columns <- c("Sim", "Year", "Catch", "SSB", "ABC", "OFL")
  invalid_numeric <- numeric_columns[!vapply(x[numeric_columns], is.numeric, logical(1))]
  if (length(invalid_numeric) > 0) {
    stop(
      "Result columns must be numeric: ",
      paste(invalid_numeric, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  class(x) <- unique(c("spm_result", class(x)))
  x
}
