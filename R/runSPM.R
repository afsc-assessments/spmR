#' Run SPM Analysis in a Specific Directory
#'
#' This function runs a Stock Production Model (SPM) analysis in the specified directory.
#' The function changes the working directory to `dirname`, runs the SPM analysis, and then reads the results from `spm_detail.csv`.
#' It returns to the original working directory after completing the analysis.
#'
#' @param dirname A string specifying the directory in which to run the SPM analysis.
#' @param ctrl Optional control settings for the SPM analysis. If NULL, default settings are used.
#' @param run Logical. If TRUE, the SPM analysis is run. If FALSE, the function only reads the results from `spm_detail.csv`.
#' @param engine Model backend to use. `"admb"` runs or reads the legacy SPM
#'   implementation; `"rtmb"` can read experimental output but cannot run
#'   validated scientific projections. New ADMB runs require explicit version-2
#'   metadata and a compatible executable. Existing output can still be read
#'   with `run = FALSE` without metadata.
#' @param metadata Filename of the version-2 metadata created by
#'   [write_spm_metadata()]. Used only for new ADMB runs.
#' @param strict Reject unknown metadata fields when TRUE. Scientific input
#'   requirements always apply.
#' @return An `spm_result` data frame containing standardized projection
#'   results. Existing model-specific columns are preserved.
#' @importFrom readr read_csv
#' @examples
#' \dontrun{
#' runSPM("examples/atka")
#' }
#' @export
runSPM <- function(
  dirname,
  ctrl = NULL,
  run = FALSE,
  engine = c("admb", "rtmb"),
  metadata = "spm_metadata.json",
  strict = TRUE
) {
  engine <- match.arg(engine)
  dirname <- normalizePath(dirname, winslash = "/", mustWork = TRUE)

  if (!is.null(ctrl)) {
    warning("`ctrl` is currently ignored by `runSPM()`.", call. = FALSE)
  }

  run_spm_adapter(
    spm_adapter(engine, metadata = metadata, strict = strict),
    dirname = dirname,
    run = run
  )
}
# runSPM("examples/atka")
