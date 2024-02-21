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

#' Read Data File into List Object
#'
#' Reads a data file formatted for a projection model and converts it into a list object.
#' The function handles numeric and NA values appropriately.
#'
#' @param fn Filename of the data file to be read.
#' @return A list object containing the data from the file.
#' @export
#' @examples
#' # Example usage:
#' # myDataList <- dat2list("datafile.dat")
dat2list <- function(fn) {
  options(warn = -1)  # Suppress warnings temporarily
  on.exit(options(warn = 0))  # Reset warning options on exit

  file_content = scan(fn, what = "character", flush = TRUE, blank.lines.skip = FALSE, quiet = TRUE)
  header_indices = substr(file_content, 1, 1) == "#"
  header_names = file_content[header_indices]

  list_data = list()
  for (i in seq_along(header_names)) {
    start_line = match(header_names[i], file_content)
    end_line = if (i < length(header_names)) match(header_names[i + 1], file_content) - 1 else length(file_content)

    data_chunk = file_content[(start_line + 1):end_line]
    if (length(data_chunk) == 1) {
      list_data[[substr(header_names[i], 2)]] = as.numeric(data_chunk)
    } else {
      list_data[[substr(header_names[i], 2)]] = read.table(text = data_chunk, fill = TRUE)
    }
  }

  return(list_data)
}

#' Print Tier 3 Tables
#'
#' Generates and prints HTML tables for Tier 3 projections including catch, ABC, fishing mortality, and spawning biomass for various scenarios.
#'
#' @param df Data frame containing Tier 3 data.
#' @param modname Name of the model used, defaults to "base".
#' @param stock Name of the stock, defaults to "BSAI Atka mackerel".
#' @return HTML tables for the specified Tier 3 data.
#' @export
#' @importFrom xtable xtable
#' @importFrom dplyr select, group_by, summarise, spread
#' @examples
#' # Example usage:
#' # print_Tier3_tables(myDataFrame, "model1", "Some Fish Stock")
print_Tier3_tables <- function(df, modname="base", stock="BSAI Atka mackerel") {
  tabcap<-tablab <- c("tier3_C","tier3_ABC","tier3_F","tier3_SSB")
  tabcap[1]=paste0("Tier 3 projections of ",stock," catch for the 7 scenarios.")
  tabcap[2]=paste0("Tier 3 projections of ",stock," ABC for the 7 scenarios.")
  tabcap[3]=paste0("Tier 3 projections of ",stock," fishing mortality for the 7 scenarios.")
  tabcap[4]=paste0("Tier 3 projections of ",stock," spawning biomass for the 7 scenarios.")

  # Stock Alt Sim Yr  SSB Rec Tot_biom SPR_Implied F Ntot Catch ABC OFL AvgAge AvgAgeTot SexRatio FABC FOFL
  bfsum <- df %>% select(Alt,Yr,SSB,F,ABC ,Catch) %>% group_by(Alt,Yr) %>% summarise(Catch=mean(Catch),SSB=mean(SSB),F=mean(F),ABC=mean(ABC))

  tC <- bfsum %>% select(Alt,Yr,Catch) %>% spread(Alt,Catch)
  names(tC) <- c("Catch","Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6","Scenario 7")

  tB <- bfsum %>% select(Alt,Yr,SSB) %>% spread(Alt,SSB)
  names(tB) <- c("SSB","Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6","Scenario 7")

  tF <- bfsum %>% select(Alt,Yr,F) %>% spread(Alt,F)
  names(tF) <- c("F","Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6","Scenario 7")

  tA <- bfsum %>% select(Alt,Yr,ABC) %>% spread(Alt,ABC)
  names(tA) <- c("ABC","Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6","Scenario 7")

  tab <- (data.frame(tC))
  rownames(tab)<-c()
  cap <- tabcap[1]
  for (i in 2:length(tab[1,]) )
    tab[,i] <- formatC((tab[,i]), format="d", big.mark=",")
  tab <- xtable(tab, caption = cap, label=paste0("tab:",tablab[1]),
                digits=0, auto=TRUE, align=rep("r",(length(tab[1,])+1)) )
  print(tab, "html", caption.placement = "top",include.rownames = FALSE, sanitize.text.function = function(x){x}, scalebox=.85)

  tab <- (data.frame(tB))
  cap <- tabcap[2]
  for (i in 2:length(tab[1,]) )
    tab[,i] <- formatC(as.numeric(tab[,i]), format="d", big.mark=",")
  tab <- xtable(tab, caption = cap, label=paste0("tab:",tablab[2]),digits=0, auto=TRUE, align=rep("r",(length(tab[1,])+1)) )
  print(tab, "html", caption.placement = "top",include.rownames = FALSE, sanitize.text.function = function(x){x}, scalebox=.85)

  tab <- (data.frame(tF))
  cap <- tabcap[3]
  for (i in 2:length(tab[1,]) )
    tab[,i] <- formatC(as.numeric(tab[,i]), format="f",digits=3)
  tab <- xtable(tab, caption = cap, label=paste0("tab:",tablab[3]), digits=3, align=rep("r",(length(tab[1,])+1)) )
  print(tab, "html", caption.placement = "top",include.rownames = FALSE, sanitize.text.function = function(x){x}, scalebox=.85)

  tab <- (data.frame(tA))
  cap <- tabcap[4]
  for (i in 2:length(tab[1,]) )
    tab[,i] <- formatC(as.numeric(tab[,i]), format="d", big.mark=",")
  tab <- xtable(tab, caption = cap, label=paste0("tab:",tablab[4]),digits=0, auto=TRUE, align=rep("r",(length(tab[1,])+1)) )
  print(tab, "html", caption.placement = "top",include.rownames = FALSE, sanitize.text.function = function(x){x}, scalebox=.85)
  return(tab)
#
#
#   if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr package is required but not installed")
#   if (!requireNamespace("xtable", quietly = TRUE)) stop("xtable package is required but not installed")
#
#   # Compute summaries
#   summary_df <- df %>%
#     dplyr::select(Alt, Yr, SSB, F, ABC, Catch) %>%
#     dplyr::group_by(Alt, Yr) %>%
#     dplyr::summarise(Catch = mean(Catch), SSB = mean(SSB), F = mean(F), ABC = mean(ABC))
#
#   # Prepare and print tables
#   table_types <- c("Catch", "SSB", "F", "ABC")
#   for (type in table_types) {
#     formatted_table <- create_formatted_table(summary_df, type, stock)
#     print_table(formatted_table, type, stock)
#   }
# }
#
# create_formatted_table <- function(df, type, stock) {
#   # Table creation logic
# }
#
# print_table <- function(table, type, stock) {
#   # Table printing logic
#}
}
