#' Write List Object to Projection Model Data File
#'
#' This function writes a list object to a file formatted for a projection model.
#' Each element of the list is written with a header.
#'
#' @param D List object to be written.
#' @param fn Filename where the data will be written.
#' @param hdr Header text to be included in the file.
#' @return The function does not return a value; it writes to a file.
#' @export
#' @examples
#' # Example usage:
#' # list2dat(myList, "datafile.dat", "Header for new file")
list2dat <- function(D, fn, hdr="a new file") {
  # Open file connection
  sink(fn)
  on.exit(sink())  # Ensure the connection is closed when the function exits
  cat(paste0("# ", hdr, "\n"))

  for (i in seq_along(D)) {
    cat(paste0("#", names(D[i]), "\n"))
    write.table(D[[i]], append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
    # The following writes a data file
    # cat(file=fn,paste0("# ",hdr,"\n"))
    # ol <-length(D)
    # for (i in 1:ol){
    #   cat(file=fn,paste0("#",names(D[i]),"\n"),append=TRUE)
    #   write.table(D[[i]],file=fn,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
    # }
}

#' @title Convert Data to List
#' @description This function reads data from a file and converts it into a list.
#' If the data are numeric, it maintains the numeric list. If the data are strings, it returns a character string.
#' @param fn A character string representing the file name to be read.
#' @return A list with numeric data or character strings based on the content of the file.
#' @examples
#' # Example usage:
#' # result <- dat2list("datafile.txt")
#' @export
dat2list <- function(fn) {
  options(warn = -1) # Suppress the NA message in the coercion to double
  datfile <- scan(fn, what = "character", flush = TRUE, blank.lines.skip = FALSE, quiet = TRUE)

  datfile
  # Identify potential list names by checking if they are not entirely numeric
  #idx <- sapply(datfile, function(x) all(is.na(as.numeric(x))))
  idx2 <- (grepl(datfile,pattern="#"))
  #idx
  #idx2
  #length(idx)
  vnam <- datfile[idx2] # list names
  #vnam

  nv <- length(vnam) # number of objects
  A <- list()
  ir <- 0

  for (i in 1:nv) {
    ir <- match(vnam[i], datfile)
    if (i != nv) irr <- match(vnam[i + 1], datfile) else irr <- length(datfile) + 1 # next row
    #dum <- NA

    if (irr - ir == 2) {
      content <- scan(fn, skip = ir, nlines = 1, quiet = TRUE, what = "")
      if (all(is.na(as.numeric(content)))) {
        dum <- as.character(content)
      } else {
        dum <- as.numeric(content)
      }
    } else if (irr - ir > 2) {
      content <- read.table(fn, skip = ir, nrow = irr - ir - 1, fill = TRUE, row.names = NULL)
      if (all(is.na(as.numeric(as.matrix(content))))) {
        dum <- as.character(as.matrix(content))
      } else {
        dum <- as.matrix(content)
      }
    }

    # Ensure proper naming and storing in the list
    if (is.numeric(dum) && !any(is.na(dum))) {
      A[[vnam[i]]] <- dum
    } else {
      A[[vnam[i]]] <- as.character(dum)
    }
  }
  names(A) <- substr(names(A), 2, nchar(names(A)))
  options(warn = 0)

  return(A)
}



