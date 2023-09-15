#' # Projection model work
#' Projection model work for demonstrating application of controls and input data

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(gtable)
library(patchwork)
source("../../R/readData.R")
  df <- read_csv("spm_detail.csv")
  ?read_table
  srdf <- read_table("srec.out",skip=4)
  names(srdf)
  srdf
  srdf %>% ggplot(aes(x=SSB,y=Rec,color=Source)) + theme_few() + geom_point()

  dfs <- df %>% filter(Sim<=30)
  #write.csv(bfs,"data/proj.csv")
 # head(bfs)
  dfss <- dfs %>% filter(Alternative==2) %>% select(Yr,Catch,SSB,Sim) 
  pf <- data.frame(read.table(paste0("percentdb.out"),header=F) )
  names(pf) <- c("stock","Alt","Yr","variable","value") 
  pf
  thisyr=2022
#' ## Make plot of projection model simulations
  p1 <- pf %>% filter(substr(variable,1,1)=="C",variable!="CStdn",Alt==2) %>% select(Yr,variable,value) %>% spread(variable,value) %>%
    ggplot(aes(x=Yr,y=CMean),width=1.2) + geom_ribbon(aes(ymax=CUCI,ymin=CLCI),fill="goldenrod",alpha=.5) + theme_few() + geom_line() +
    scale_x_continuous(breaks=seq(thisyr,thisyr+14,2))  +  xlab("Year") + ylab("Tier 3 ABC (kt)") + geom_point() + 
    expand_limits(y=0) +
    geom_line(aes(y=Cabc)) + geom_line(aes(y=Cofl),linetype="dashed") + geom_line(data=dfss,aes(x=Yr,y=Catch,col=as.factor(Sim)))+ guides(size=FALSE,fill=FALSE,alpha=FALSE,col=FALSE) 

  p2 <- pf %>% filter(substr(variable,1,1)=="S",variable!="SSBStdn",Alt==2) %>% select(Yr,variable,value) %>% spread(variable,value) %>%
    ggplot(aes(x=Yr,y=SSBMean),width=1.2) + geom_ribbon(aes(ymax=SSBUCI,ymin=SSBLCI),fill="coral",alpha=.5) + theme_few() + geom_line() +
    scale_x_continuous(breaks=seq(thisyr,thisyr+14,2))  +  xlab("Year") + ylab("Tier 3 Spawning biomass (kt)") + geom_point() + 
    expand_limits(y=0) +
    geom_line(aes(y=SSBFabc)) + geom_line(aes(y=SSBFofl),linetype="dashed")+ geom_line(data=dfss,aes(x=Yr,y=SSB,col=as.factor(Sim)))+ guides(size=FALSE,fill=FALSE,alpha=FALSE,col=FALSE) 
  p1/p2
  ggsave(paste0(.projdir,"tier3_proj.pdf"),plot=t3,width=5.4,height=7,units="in")

#[1] "Stock"       "Alternative" "Sim"         "Yr"          "SSB"         "Rec"         "Tot_biom"    "SPR_Implied" "F"           "Ntot"        "Catch"
#[12] "ABC"         "OFL"         "AvgAge"      "AvgAgeTot"   "SexRatio"    "B100"        "B40"         "B35"

p1<- df %>% filter(Alternative==2,SSB<500) %>% ggplot(aes(x=SSB,y=F)) + geom_point(color="salmon",alpha=.2) + theme_few() 
p2<- df %>% filter(Alternative==2,SSB<500) %>% ggplot(aes(x=SSB,y=ABC)) + geom_point(alpha=.2,color="blue") + theme_few() 
p3<- df %>% filter(Alternative==2,SSB<500) %>% ggplot(aes(x=log(SSB),y=log(Rec)/log(SSB))) + geom_point(alpha=.2,color="green",size=.5) + geom_smooth() + theme_few() 
  p1/p2/p3

#' ## Make tables
  # Stock Alt Sim Yr  SSB Rec Tot_biom SPR_Implied F Ntot Catch ABC OFL AvgAge AvgAgeTot SexRatio FABC FOFL
  dfsum <- df %>% select(Alternative,Yr,SSB,F,ABC ,Catch) %>% group_by(Alternative,Yr) %>% summarise(Catch=mean(Catch),SSB=mean(SSB),F=mean(F),ABC=mean(ABC))
  t1 <- dfsum %>% select(Alternative,Yr,Catch) %>% spread(Alternative,Catch) 
  names(t1) <- c("Catch","Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5","Scenario 6","Scenario 7")
  t1
kkkkkk
  print_Tier3_tables(bf)

