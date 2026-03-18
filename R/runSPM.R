#' Run SPM Analysis in a Specific Directory
#'
#' This function runs a Stock Production Model (SPM) analysis in the specified directory.
#' The function changes the working directory to `dirname`, runs the SPM analysis, and then reads the results from `spm_detail.csv`.
#' It returns to the original working directory after completing the analysis.
#'
#' @param dirname A string specifying the directory in which to run the SPM analysis.
#' @param ctrl Optional control settings for the SPM analysis. If NULL, default settings are used.
#' @param run Logical. If TRUE, the SPM analysis is run. If FALSE, the function only reads the results from `spm_detail.csv`.
#' @return A data frame containing the results from `spm_detail.csv`.
#' @importFrom readr read_csv
#' @examples
#' \dontrun{
#' runSPM("examples/atka")
#' }
#' @export
runSPM <- function(dirname, ctrl = NULL, run = FALSE, engine = c("admb", "rtmb")){
  engine <- match.arg(engine)
  dirname <- normalizePath(dirname, winslash = "/", mustWork = TRUE)
  args <- character()

  if (!is.null(ctrl)) {
    warning("`ctrl` is currently ignored by `runSPM()`.", call. = FALSE)
  }

  if (engine == "admb") {
    if (run) {
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(dirname)

      exe <- if (.Platform$OS.type == "windows") "spm.exe" else "./spm"
      if (!file.exists(exe)) {
        exe <- Sys.which("spm")
      }
      if (!nzchar(exe)) {
        stop("Could not find an SPM executable in `", dirname, "` or on PATH.")
      }

      status <- system2(exe, args = args)
      if (!identical(status, 0L)) {
        stop("SPM execution failed with exit status ", status, ".")
      }
    }
    res <- readr::read_csv(file.path(dirname, "spm_detail.csv"))
  } else {
    res <- runSPM_rtmb(dirname = dirname, run = run)
  }

  return(res)
}
#runSPM("examples/atka")
