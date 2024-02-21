# Projection model work
Projection model work for demonstrating application of controls and input data


```r
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(gtable)
source("../R/readData.R")
```

## Set initial "setup" parameters


```r
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
```

## Set up the species specific run file


```r
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
```

##  Make list of main file w/ assessment model results
E.g., "data/bsai_atka.dat"


```r
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
```

## Save lists for running model to files expected by projection model


```r
# Setup.dat
list2dat(setup,"setup.dat")
# spp_catch.dat
list2dat(config,"data/t1_spcat.dat")
runfn<-"t1"
file.copy(paste0("data/",runfn,"_spcat.dat"),"spp_catch.dat",overwrite=TRUE)
```

```
## [1] TRUE
```

```r
list2dat(datfile,"data/t1.dat")
```

## Run projection model


```r
system("../src/main")
```

## Read in projection model mainfiles


```r
  .projdir="t1/"
  dir.create(.projdir)
```

```
## Warning in dir.create(.projdir): 't1' already exists
```

```r
  file.copy(list.files(getwd(), pattern="out$"), .projdir,overwrite=TRUE)      
```

```
## [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```

```r
  file.remove(list.files(getwd(), pattern="out$"))
```

```
## [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```

```r
  bf <- data.frame(read.table(paste0(.projdir,"bigfile.out"),header=TRUE,as.is=TRUE))
  bfs <- bf %>% filter(Sim<=30)
  #write.csv(bfs,"data/proj.csv")
 # head(bfs)
  bfss <- bfs %>% filter(Alt==2) %>% select(Alt,Yr,Catch,SSB,Sim) 
  pf <- data.frame(read.table(paste0(.projdir,"percentdb.out"),header=F) )
  names(pf) <- c("stock","Alt","Yr","variable","value") 
```

## Make plot of projection model simulations


```r
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
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
  ggsave(paste0(.projdir,"tier3_proj.pdf"),plot=t3,width=5.4,height=7,units="in")
```

## Make tables


```r
  # Stock Alt Sim Yr  SSB Rec Tot_biom SPR_Implied F Ntot Catch ABC OFL AvgAge AvgAgeTot SexRatio FABC FOFL
  bfsum <- bf %>% select(Alt,Yr,SSB,F,ABC ,Catch) %>% group_by(Alt,Yr) %>% summarise(Catch=mean(Catch),SSB=mean(SSB),F=mean(F),ABC=mean(ABC))
  t1 <- bfsum %>% select(Alt,Yr,Catch) %>% spread(Alt,Catch) 
  names(t1) <- c("Catch","Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6","Scenario 7")

  print_Tier3_tables(bf)
```

```
## <!-- html table generated in R 4.0.0 by xtable 1.8-4 package -->
## <!-- Tue Jun  9 08:53:57 2020 -->
## <table border=1>
## <caption align="top"> Tier 3 projections of BSAI Atka mackerel catch for the 7 scenarios. </caption>
## <tr> <th> Catch </th> <th> Scenario.1 </th> <th> Scenario.2 </th> <th> Scenario.3 </th> <th> Scenario.4 </th> <th> Scenario.5 </th> <th> Scenario.6 </th> <th> Scenario.7 </th>  </tr>
##   <tr> <td align="right"> 2019 </td> <td align="right"> 55 </td> <td align="right"> 55 </td> <td align="right"> 55 </td> <td align="right"> 55 </td> <td align="right"> 55 </td> <td align="right"> 55 </td> <td align="right"> 55 </td> </tr>
##   <tr> <td align="right"> 2020 </td> <td align="right"> 91 </td> <td align="right"> 55 </td> <td align="right"> 21 </td> <td align="right"> 27 </td> <td align="right"> 0 </td> <td align="right"> 106 </td> <td align="right"> 91 </td> </tr>
##   <tr> <td align="right"> 2021 </td> <td align="right"> 83 </td> <td align="right"> 85 </td> <td align="right"> 22 </td> <td align="right"> 28 </td> <td align="right"> 0 </td> <td align="right"> 91 </td> <td align="right"> 83 </td> </tr>
##   <tr> <td align="right"> 2022 </td> <td align="right"> 81 </td> <td align="right"> 80 </td> <td align="right"> 25 </td> <td align="right"> 30 </td> <td align="right"> 0 </td> <td align="right"> 85 </td> <td align="right"> 93 </td> </tr>
##   <tr> <td align="right"> 2023 </td> <td align="right"> 82 </td> <td align="right"> 81 </td> <td align="right"> 27 </td> <td align="right"> 33 </td> <td align="right"> 0 </td> <td align="right"> 88 </td> <td align="right"> 91 </td> </tr>
##   <tr> <td align="right"> 2024 </td> <td align="right"> 86 </td> <td align="right"> 84 </td> <td align="right"> 29 </td> <td align="right"> 35 </td> <td align="right"> 0 </td> <td align="right"> 92 </td> <td align="right"> 93 </td> </tr>
##   <tr> <td align="right"> 2025 </td> <td align="right"> 90 </td> <td align="right"> 88 </td> <td align="right"> 30 </td> <td align="right"> 37 </td> <td align="right"> 0 </td> <td align="right"> 96 </td> <td align="right"> 97 </td> </tr>
##   <tr> <td align="right"> 2026 </td> <td align="right"> 90 </td> <td align="right"> 88 </td> <td align="right"> 31 </td> <td align="right"> 38 </td> <td align="right"> 0 </td> <td align="right"> 96 </td> <td align="right"> 97 </td> </tr>
##   <tr> <td align="right"> 2027 </td> <td align="right"> 89 </td> <td align="right"> 86 </td> <td align="right"> 31 </td> <td align="right"> 38 </td> <td align="right"> 0 </td> <td align="right"> 94 </td> <td align="right"> 94 </td> </tr>
##   <tr> <td align="right"> 2028 </td> <td align="right"> 89 </td> <td align="right"> 86 </td> <td align="right"> 31 </td> <td align="right"> 38 </td> <td align="right"> 0 </td> <td align="right"> 95 </td> <td align="right"> 95 </td> </tr>
##   <tr> <td align="right"> 2029 </td> <td align="right"> 85 </td> <td align="right"> 82 </td> <td align="right"> 31 </td> <td align="right"> 37 </td> <td align="right"> 0 </td> <td align="right"> 90 </td> <td align="right"> 90 </td> </tr>
##   <tr> <td align="right"> 2030 </td> <td align="right"> 84 </td> <td align="right"> 80 </td> <td align="right"> 30 </td> <td align="right"> 37 </td> <td align="right"> 0 </td> <td align="right"> 88 </td> <td align="right"> 88 </td> </tr>
##   <tr> <td align="right"> 2031 </td> <td align="right"> 85 </td> <td align="right"> 83 </td> <td align="right"> 31 </td> <td align="right"> 37 </td> <td align="right"> 0 </td> <td align="right"> 91 </td> <td align="right"> 91 </td> </tr>
##   <tr> <td align="right"> 2032 </td> <td align="right"> 88 </td> <td align="right"> 85 </td> <td align="right"> 31 </td> <td align="right"> 38 </td> <td align="right"> 0 </td> <td align="right"> 93 </td> <td align="right"> 93 </td> </tr>
##    </table>
## <!-- html table generated in R 4.0.0 by xtable 1.8-4 package -->
## <!-- Tue Jun  9 08:53:57 2020 -->
## <table border=1>
## <caption align="top"> Tier 3 projections of BSAI Atka mackerel ABC for the 7 scenarios. </caption>
## <tr> <th> SSB </th> <th> Scenario.1 </th> <th> Scenario.2 </th> <th> Scenario.3 </th> <th> Scenario.4 </th> <th> Scenario.5 </th> <th> Scenario.6 </th> <th> Scenario.7 </th>  </tr>
##   <tr> <td align="right"> 2019 </td> <td align="right"> 172 </td> <td align="right"> 172 </td> <td align="right"> 172 </td> <td align="right"> 172 </td> <td align="right"> 172 </td> <td align="right"> 172 </td> <td align="right"> 172 </td> </tr>
##   <tr> <td align="right"> 2020 </td> <td align="right"> 156 </td> <td align="right"> 164 </td> <td align="right"> 172 </td> <td align="right"> 171 </td> <td align="right"> 177 </td> <td align="right"> 152 </td> <td align="right"> 156 </td> </tr>
##   <tr> <td align="right"> 2021 </td> <td align="right"> 138 </td> <td align="right"> 152 </td> <td align="right"> 178 </td> <td align="right"> 175 </td> <td align="right"> 192 </td> <td align="right"> 131 </td> <td align="right"> 138 </td> </tr>
##   <tr> <td align="right"> 2022 </td> <td align="right"> 130 </td> <td align="right"> 141 </td> <td align="right"> 187 </td> <td align="right"> 182 </td> <td align="right"> 209 </td> <td align="right"> 122 </td> <td align="right"> 128 </td> </tr>
##   <tr> <td align="right"> 2023 </td> <td align="right"> 132 </td> <td align="right"> 142 </td> <td align="right"> 204 </td> <td align="right"> 197 </td> <td align="right"> 233 </td> <td align="right"> 123 </td> <td align="right"> 126 </td> </tr>
##   <tr> <td align="right"> 2024 </td> <td align="right"> 135 </td> <td align="right"> 144 </td> <td align="right"> 218 </td> <td align="right"> 210 </td> <td align="right"> 254 </td> <td align="right"> 125 </td> <td align="right"> 127 </td> </tr>
##   <tr> <td align="right"> 2025 </td> <td align="right"> 139 </td> <td align="right"> 146 </td> <td align="right"> 232 </td> <td align="right"> 223 </td> <td align="right"> 275 </td> <td align="right"> 128 </td> <td align="right"> 129 </td> </tr>
##   <tr> <td align="right"> 2026 </td> <td align="right"> 140 </td> <td align="right"> 147 </td> <td align="right"> 243 </td> <td align="right"> 232 </td> <td align="right"> 292 </td> <td align="right"> 129 </td> <td align="right"> 129 </td> </tr>
##   <tr> <td align="right"> 2027 </td> <td align="right"> 139 </td> <td align="right"> 146 </td> <td align="right"> 249 </td> <td align="right"> 237 </td> <td align="right"> 302 </td> <td align="right"> 127 </td> <td align="right"> 127 </td> </tr>
##   <tr> <td align="right"> 2028 </td> <td align="right"> 138 </td> <td align="right"> 145 </td> <td align="right"> 252 </td> <td align="right"> 240 </td> <td align="right"> 309 </td> <td align="right"> 126 </td> <td align="right"> 126 </td> </tr>
##   <tr> <td align="right"> 2029 </td> <td align="right"> 137 </td> <td align="right"> 144 </td> <td align="right"> 254 </td> <td align="right"> 241 </td> <td align="right"> 314 </td> <td align="right"> 125 </td> <td align="right"> 125 </td> </tr>
##   <tr> <td align="right"> 2030 </td> <td align="right"> 134 </td> <td align="right"> 141 </td> <td align="right"> 251 </td> <td align="right"> 238 </td> <td align="right"> 313 </td> <td align="right"> 122 </td> <td align="right"> 122 </td> </tr>
##   <tr> <td align="right"> 2031 </td> <td align="right"> 134 </td> <td align="right"> 141 </td> <td align="right"> 252 </td> <td align="right"> 238 </td> <td align="right"> 315 </td> <td align="right"> 123 </td> <td align="right"> 123 </td> </tr>
##   <tr> <td align="right"> 2032 </td> <td align="right"> 137 </td> <td align="right"> 144 </td> <td align="right"> 255 </td> <td align="right"> 242 </td> <td align="right"> 319 </td> <td align="right"> 125 </td> <td align="right"> 125 </td> </tr>
##    </table>
## <!-- html table generated in R 4.0.0 by xtable 1.8-4 package -->
## <!-- Tue Jun  9 08:53:57 2020 -->
## <table border=1>
## <caption align="top"> Tier 3 projections of BSAI Atka mackerel fishing mortality for the 7 scenarios. </caption>
## <tr> <th> F </th> <th> Scenario.1 </th> <th> Scenario.2 </th> <th> Scenario.3 </th> <th> Scenario.4 </th> <th> Scenario.5 </th> <th> Scenario.6 </th> <th> Scenario.7 </th>  </tr>
##   <tr> <td align="right"> 2019 </td> <td align="right"> 0.169 </td> <td align="right"> 0.169 </td> <td align="right"> 0.169 </td> <td align="right"> 0.169 </td> <td align="right"> 0.169 </td> <td align="right"> 0.169 </td> <td align="right"> 0.169 </td> </tr>
##   <tr> <td align="right"> 2020 </td> <td align="right"> 0.299 </td> <td align="right"> 0.172 </td> <td align="right"> 0.066 </td> <td align="right"> 0.083 </td> <td align="right"> 0.000 </td> <td align="right"> 0.353 </td> <td align="right"> 0.299 </td> </tr>
##   <tr> <td align="right"> 2021 </td> <td align="right"> 0.299 </td> <td align="right"> 0.283 </td> <td align="right"> 0.066 </td> <td align="right"> 0.083 </td> <td align="right"> 0.000 </td> <td align="right"> 0.343 </td> <td align="right"> 0.299 </td> </tr>
##   <tr> <td align="right"> 2022 </td> <td align="right"> 0.284 </td> <td align="right"> 0.263 </td> <td align="right"> 0.066 </td> <td align="right"> 0.083 </td> <td align="right"> 0.000 </td> <td align="right"> 0.315 </td> <td align="right"> 0.330 </td> </tr>
##   <tr> <td align="right"> 2023 </td> <td align="right"> 0.277 </td> <td align="right"> 0.259 </td> <td align="right"> 0.066 </td> <td align="right"> 0.083 </td> <td align="right"> 0.000 </td> <td align="right"> 0.311 </td> <td align="right"> 0.317 </td> </tr>
##   <tr> <td align="right"> 2024 </td> <td align="right"> 0.275 </td> <td align="right"> 0.258 </td> <td align="right"> 0.066 </td> <td align="right"> 0.083 </td> <td align="right"> 0.000 </td> <td align="right"> 0.310 </td> <td align="right"> 0.312 </td> </tr>
##   <tr> <td align="right"> 2025 </td> <td align="right"> 0.276 </td> <td align="right"> 0.260 </td> <td align="right"> 0.066 </td> <td align="right"> 0.083 </td> <td align="right"> 0.000 </td> <td align="right"> 0.312 </td> <td align="right"> 0.314 </td> </tr>
##   <tr> <td align="right"> 2026 </td> <td align="right"> 0.277 </td> <td align="right"> 0.261 </td> <td align="right"> 0.066 </td> <td align="right"> 0.083 </td> <td align="right"> 0.000 </td> <td align="right"> 0.313 </td> <td align="right"> 0.314 </td> </tr>
##   <tr> <td align="right"> 2027 </td> <td align="right"> 0.277 </td> <td align="right"> 0.260 </td> <td align="right"> 0.066 </td> <td align="right"> 0.083 </td> <td align="right"> 0.000 </td> <td align="right"> 0.313 </td> <td align="right"> 0.313 </td> </tr>
##   <tr> <td align="right"> 2028 </td> <td align="right"> 0.278 </td> <td align="right"> 0.259 </td> <td align="right"> 0.066 </td> <td align="right"> 0.083 </td> <td align="right"> 0.000 </td> <td align="right"> 0.312 </td> <td align="right"> 0.312 </td> </tr>
##   <tr> <td align="right"> 2029 </td> <td align="right"> 0.277 </td> <td align="right"> 0.258 </td> <td align="right"> 0.066 </td> <td align="right"> 0.083 </td> <td align="right"> 0.000 </td> <td align="right"> 0.310 </td> <td align="right"> 0.310 </td> </tr>
##   <tr> <td align="right"> 2030 </td> <td align="right"> 0.275 </td> <td align="right"> 0.254 </td> <td align="right"> 0.066 </td> <td align="right"> 0.083 </td> <td align="right"> 0.000 </td> <td align="right"> 0.306 </td> <td align="right"> 0.306 </td> </tr>
##   <tr> <td align="right"> 2031 </td> <td align="right"> 0.274 </td> <td align="right"> 0.254 </td> <td align="right"> 0.066 </td> <td align="right"> 0.083 </td> <td align="right"> 0.000 </td> <td align="right"> 0.307 </td> <td align="right"> 0.307 </td> </tr>
##   <tr> <td align="right"> 2032 </td> <td align="right"> 0.275 </td> <td align="right"> 0.256 </td> <td align="right"> 0.066 </td> <td align="right"> 0.083 </td> <td align="right"> 0.000 </td> <td align="right"> 0.309 </td> <td align="right"> 0.309 </td> </tr>
##    </table>
## <!-- html table generated in R 4.0.0 by xtable 1.8-4 package -->
## <!-- Tue Jun  9 08:53:57 2020 -->
## <table border=1>
## <caption align="top"> Tier 3 projections of BSAI Atka mackerel spawning biomass for the 7 scenarios. </caption>
## <tr> <th> ABC </th> <th> Scenario.1 </th> <th> Scenario.2 </th> <th> Scenario.3 </th> <th> Scenario.4 </th> <th> Scenario.5 </th> <th> Scenario.6 </th> <th> Scenario.7 </th>  </tr>
##   <tr> <td align="right"> 2019 </td> <td align="right"> 93 </td> <td align="right"> 93 </td> <td align="right"> 22 </td> <td align="right"> 27 </td> <td align="right"> 0 </td> <td align="right"> 107 </td> <td align="right"> 107 </td> </tr>
##   <tr> <td align="right"> 2020 </td> <td align="right"> 91 </td> <td align="right"> 89 </td> <td align="right"> 21 </td> <td align="right"> 27 </td> <td align="right"> 0 </td> <td align="right"> 106 </td> <td align="right"> 106 </td> </tr>
##   <tr> <td align="right"> 2021 </td> <td align="right"> 83 </td> <td align="right"> 85 </td> <td align="right"> 22 </td> <td align="right"> 28 </td> <td align="right"> 0 </td> <td align="right"> 91 </td> <td align="right"> 96 </td> </tr>
##   <tr> <td align="right"> 2022 </td> <td align="right"> 81 </td> <td align="right"> 80 </td> <td align="right"> 25 </td> <td align="right"> 30 </td> <td align="right"> 0 </td> <td align="right"> 85 </td> <td align="right"> 93 </td> </tr>
##   <tr> <td align="right"> 2023 </td> <td align="right"> 82 </td> <td align="right"> 81 </td> <td align="right"> 27 </td> <td align="right"> 33 </td> <td align="right"> 0 </td> <td align="right"> 88 </td> <td align="right"> 91 </td> </tr>
##   <tr> <td align="right"> 2024 </td> <td align="right"> 86 </td> <td align="right"> 84 </td> <td align="right"> 29 </td> <td align="right"> 35 </td> <td align="right"> 0 </td> <td align="right"> 92 </td> <td align="right"> 93 </td> </tr>
##   <tr> <td align="right"> 2025 </td> <td align="right"> 90 </td> <td align="right"> 88 </td> <td align="right"> 30 </td> <td align="right"> 37 </td> <td align="right"> 0 </td> <td align="right"> 96 </td> <td align="right"> 97 </td> </tr>
##   <tr> <td align="right"> 2026 </td> <td align="right"> 90 </td> <td align="right"> 88 </td> <td align="right"> 31 </td> <td align="right"> 38 </td> <td align="right"> 0 </td> <td align="right"> 96 </td> <td align="right"> 97 </td> </tr>
##   <tr> <td align="right"> 2027 </td> <td align="right"> 89 </td> <td align="right"> 86 </td> <td align="right"> 31 </td> <td align="right"> 38 </td> <td align="right"> 0 </td> <td align="right"> 94 </td> <td align="right"> 94 </td> </tr>
##   <tr> <td align="right"> 2028 </td> <td align="right"> 89 </td> <td align="right"> 86 </td> <td align="right"> 31 </td> <td align="right"> 38 </td> <td align="right"> 0 </td> <td align="right"> 95 </td> <td align="right"> 95 </td> </tr>
##   <tr> <td align="right"> 2029 </td> <td align="right"> 85 </td> <td align="right"> 82 </td> <td align="right"> 31 </td> <td align="right"> 37 </td> <td align="right"> 0 </td> <td align="right"> 90 </td> <td align="right"> 90 </td> </tr>
##   <tr> <td align="right"> 2030 </td> <td align="right"> 84 </td> <td align="right"> 80 </td> <td align="right"> 30 </td> <td align="right"> 37 </td> <td align="right"> 0 </td> <td align="right"> 88 </td> <td align="right"> 88 </td> </tr>
##   <tr> <td align="right"> 2031 </td> <td align="right"> 85 </td> <td align="right"> 83 </td> <td align="right"> 31 </td> <td align="right"> 37 </td> <td align="right"> 0 </td> <td align="right"> 91 </td> <td align="right"> 91 </td> </tr>
##   <tr> <td align="right"> 2032 </td> <td align="right"> 88 </td> <td align="right"> 85 </td> <td align="right"> 31 </td> <td align="right"> 38 </td> <td align="right"> 0 </td> <td align="right"> 93 </td> <td align="right"> 93 </td> </tr>
##    </table>
```

