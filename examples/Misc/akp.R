#' # Projection model work
#' Projection model work for demonstrating application of controls and input data

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(gtable)
source("../R/readData.R")

#' ## Set initial "setup" parameters
thisyr=2019
setup<-list(
  Run_name     = noquote("Std"),
  Tier         = 3    ,
  nalts        = 7    ,
  alts         = c(1,2,3,4,5,6,7),
  tac_abc      = 1,    #' Flag to set TAC equal to ABC (1 means true, otherwise false)
  srr          = 1 ,   #' Stock-recruitment type (1=Ricker, 2=Bholt)
  rec_proj     = 1,    #' projection rec form (default: 1 = use observed mean and std, option 2 = use estimated SRR and estimated sigma R)
  srr_cond     = 0 ,   #' SR-Conditioning (0 means no, 1 means use Fmsy == F35%?, 2 means Fmsy == F35% and Bmsy=B35%  condition (affects SRR fits)
  srr_prior    = 0.0,  #' Condition that there is a prior that mean historical recruitment is similar to expected recruitment at half mean SSB and double mean SSB 0 means don't use, otherwise specify CV
  write_big    = 1,    #' Flag to write big file (of all simulations rather than a summary, 0 means don't do it, otherwise do it) Write_Big
  nyrs_proj    = 14,   #' Number of projection years
  nsims        = 100, #' Number of simulations
  beg_yr_label = thisyr  #' Begin Year
)

#' ## Set up the species specific run file
config<-list(
  nFixCatchYrs = 2,
  nSpecies     = 1,
  OYMin        = .1343248,
  OYMax        = 1943248,
  dataFiles    = noquote("data/t1.dat"),
  ABCMult      = 1,
  PoplnScalar  = 1000,
  AltFabcSPR   = 0.75,
  nTAC         = 1,
  TACIndices   = 1,
  Catch        = c( 2016,55000.,   2017,55000. )
)

#' ##  Make list of main file w/ assessment model results
#' E.g., "data/bsai_atka.dat"
datfile <- list(
  runname     = noquote("M16.2"), 
  ssl_spp     = 1,         # SSL_spp
  Dorn_buffer = 1,         # Dorn_buffer
  nfsh        = 1,         # N_fsh
  nsex        = 1,         # N_sexes
  avgF5yr     = 0.0661399, # avg_5yr_F
  F40_mult    = 1,         # F_40_multiplier
  spr_abc     = 0.4,       # SPR_abc
  spr_msy     = 0.35,      # SPR_msy
  sp_mo       = 8,         # spawn_month
  nages       = 11,        # N_ages
  Frat        = 1,         # F_ratio
	# M
	M    = c(0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3), 
	#  Maturity
	pmat = c(0.005,0.037,0.224,0.688,0.944,0.992,0.999,1,1,1,1),
	#  Wt_at_age_spawners  
	wtage_sp  = c(44.8,161.377,398.272,557.695,652.113,719.573,863.744,948.744,921.397,885.912,1069.87),
	#  Wt_at_age_fsh
	wtage_fsh = c(69.3778,253.522,408.211,614.731,668.483,718.137,803.017,798.707,788.117,842.468,960.006), 
	# select
	sel = c(0.002576427,0.040030753,0.651104228,0.768404263,0.794886081,1,0.889293108,0.604815671,0.451169778,0.403195516,0.403195516),
	# N
	N   = c(511.179,378.528,278.443,194.385,183.423,45.5404,61.1188,19.8073,39.6285,33.6501,51.7806),
	# Nyrs
	nyrs = 37,
	# recruits
	R    = c(1578.51,479.509,357.919,443.588,318.981,413.125,514.351,600.987,536.301,692.34,452.279,1618.73,702.801,372.811,597.812,1136.19,402.862,424.179,1025.36,207.05,383.695,1054.64,2224.52,1379.34,1545.81,345.556,454.884,617.194,404.876,993.497,727.754,236.795,505.948,258.653,726.627,524.198,473.54),
	# SSB 
	SSB  = c(206.391,194.569,187.097,183.296,195.289,240.774,252.143,238.163,223.199,200.776,182.378,179.671,189.193,198.242,212.123,233.484,277.891,282.004,250.709,231.816,218.403,195.275,181.628,189.976,175.912,168.624,220.206,315.124,376.621,397.171,365.476,317.159,277.777,242.719,233.415,223.636,198.117,183.537,177.91)
	)

#' ## Save lists for running model to files expected by projection model
# Setup.dat
list2dat(setup,"setup.dat")
# spp_catch.dat
list2dat(config,"data/t1_spcat.dat")
runfn<-"t1"
file.copy(paste0("data/",runfn,"_spcat.dat"),"spp_catch.dat",overwrite=TRUE)

list2dat(datfile,"data/t1.dat")

#' ## Run projection model
system("../src/main")
#' ## Read in projection model mainfiles
  .projdir="akp_out/"
  dir.create(.projdir)
  file.copy(list.files(getwd(), pattern="out$"), .projdir,overwrite=TRUE)      
  file.remove(list.files(getwd(), pattern="out$"))
  
  bf <- data.frame(read.table(paste0(.projdir,"bigfile.out"),header=TRUE,as.is=TRUE))
  bfs <- bf %>% filter(Sim<=30)
  #write.csv(bfs,"data/proj.csv")
 # head(bfs)
  bfss <- bfs %>% filter(Alt==2) %>% select(Alt,Yr,Catch,SSB,Sim) 
  pf <- data.frame(read.table(paste0(.projdir,"percentdb.out"),header=F) )
  names(pf) <- c("stock","Alt","Yr","variable","value") 
#' ## Make plot of projection model simulations
  p1 <- pf %>% filter(substr(variable,1,1)=="C",variable!="CStdn",Alt==2) %>% select(Yr,variable,value) %>% spread(variable,value) %>%
    ggplot(aes(x=Yr,y=CMean),width=1.2) + geom_ribbon(aes(ymax=CUCI,ymin=CLCI),fill="goldenrod",alpha=.5) + theme_few() + geom_line() +
    scale_x_continuous(breaks=seq(thisyr,thisyr+14,2))  +  xlab("Year") + ylab("Tier 3 ABC (kt)") + geom_point() + 
    expand_limits(y=0) +
    geom_line(aes(y=Cabc)) + geom_line(aes(y=Cofl),linetype="dashed") + geom_line(data=bfss,aes(x=Yr,y=Catch,col=as.factor(Sim)))+ guides(size=FALSE,fill=FALSE,alpha=FALSE,col=FALSE) 
  p2 <- pf %>% filter(substr(variable,1,1)=="S",variable!="SSBStdn",Alt==2) %>% select(Yr,variable,value) %>% spread(variable,value) %>%
    ggplot(aes(x=Yr,y=SSBMean),width=1.2) + geom_ribbon(aes(ymax=SSBUCI,ymin=SSBLCI),fill="coral",alpha=.5) + theme_few() + geom_line() +
    scale_x_continuous(breaks=seq(thisyr,thisyr+14,2))  +  xlab("Year") + ylab("Tier 3 Spawning biomass (kt)") + geom_point() + 
    expand_limits(y=0) +
    geom_line(aes(y=SSBFabc)) + geom_line(aes(y=SSBFofl),linetype="dashed")+ geom_line(data=bfss,aes(x=Yr,y=SSB,col=as.factor(Sim)))+ guides(size=FALSE,fill=FALSE,alpha=FALSE,col=FALSE) 
  t3 <- grid.arrange(p1, p2, nrow=2)
  library(patchwork)
  p1/p2
  ggsave(paste0(.projdir,"tier3_proj.pdf"),plot=t3,width=5.4,height=7,units="in")


#' ## Make tables
  # Stock Alt Sim Yr  SSB Rec Tot_biom SPR_Implied F Ntot Catch ABC OFL AvgAge AvgAgeTot SexRatio FABC FOFL
  bfsum <- bf %>% select(Alt,Yr,SSB,F,ABC ,Catch) %>% group_by(Alt,Yr) %>% summarise(Catch=mean(Catch),SSB=mean(SSB),F=mean(F),ABC=mean(ABC))
  t1 <- bfsum %>% select(Alt,Yr,Catch) %>% spread(Alt,Catch) 
  names(t1) <- c("Catch","Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6","Scenario 7")

  print_Tier3_tables(bf)

