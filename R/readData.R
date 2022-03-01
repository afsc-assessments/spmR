#' list2dat
#'
#' write list object to projection model data file
#'
#' @param D objection to be written to
#' @return  written data file for spm model
#' @export
list2dat <- function(D,fn,hdr="a new file") {
    # The following writes a data file
    cat(file=fn,paste0("# ",hdr,"\n"))
    ol <-length(D)
    for (i in 1:ol){
      cat(file=fn,paste0("#",names(D[i]),"\n"),append=TRUE)
      write.table(D[[i]],file=fn,append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

#' dat2list
#' Read list object to projection model data file
#'
#' @return  update from last year's files
#' @export
dat2list <- function(fn)
{
	options(warn=-1)  #Suppress the NA message in the coercion to double
	ifile=scan(fn,what="character",flush=TRUE,blank.lines.skip=FALSE,quiet=TRUE)
	idx=sapply(as.double(ifile),is.na)
	vnam=ifile[idx] #list names
	nv=length(vnam) #number of objects
	A=list()
	ir=0
	for(i in 1:nv)
	{
		ir=match(vnam[i],ifile)
		print(ir)
		if(i!=nv) irr=match(vnam[i+1],ifile) else irr=length(ifile)+1 #next row
		dum=NA
		if(irr-ir==2) dum=as.double(scan(fn,skip=ir,nlines=1,quiet=TRUE,what=""))
		if(irr-ir>2)  dum=as.matrix(read.table(fn,skip=ir,nrow=irr-ir-1,fill=TRUE))

		if(is.numeric(dum))#Logical test to ensure dealing with numbers
		{
			A[[substr(vnam[i],2,10)]]=dum
		}
	}
	options(warn=0)
	
	return(A)
}

#'print_Tier3_tables
#'
#' @param Dataframe (spm_detail.csv)
#' @return formatted table
#' @export
#' @example 
#' 
print_Tier3_tables <- function(df, modname="base",stock="BSAI Atka mackerel") {
  modname="base"
  modname="base";stock="BSAI Atka mackerel"
  
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
}
