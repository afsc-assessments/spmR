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
#' runSPM("examples/atka")
#' @export
runSPM <- function(dirname, ctrl=NULL,run=FALSE){
  if(is.null(ctrl)) #ctrl=setup
  #write.table(ctrl,file="spm.dat",quote=FALSE,col.names=FALSE,row.names=FALSE)
  if(is.null(title)) title=dirname
  args=""
  if (run) system(paste0('cd ',dirname,'; spm',args))

  res <- readr::read_csv(paste0(dirname,"/spm_detail.csv"))
  return(res)
}
#runSPM("examples/atka")
