library(tidyverse)
library(ggplot2)
library(ggthemes)
system("run.sh goanrs_central")
system("run.sh goanrs_western")
system("run.sh goanrs")

# Or do all three above in one run
system("run.sh goanrs_all")


plot_ssb <- function(dirname,title=NULL){
	df <- read_table(paste0(dirname,"_out/bigfile.out"))
	if(is.null(title)) title=dirname
  df %>% mutate(Alt=as.factor(Alt)) %>% group_by(Yr,Alt) %>% 
  summarise(SSB=mean(SSB),Catch=mean(Catch),ABC=mean(ABC)) %>%
  ggplot(aes(x=Yr,y=SSB,color=Alt)) + geom_line(size=2) + 
  expand_limits(y=0) + theme_few() + ggtitle(title)
}
plot_ssb(dirname="goanrs_central","N rock sole, Central")
plot_ssb(dirname="goanrs_western","N rock sole, western")

plot_ssb <- function(dirname,alt=c(1,7),title=NULL){
  df <- read_table(paste0(dirname,"_out/bigfile.out"))
  if(is.null(title)) title=dirname
  df %>% filter(Alt %in% alt) %>% group_by(Yr,Stock,Alt) %>% 
  summarise(ub=quantile(SSB,.9),lb=quantile(SSB,.1),
    SSB=mean(SSB), Catch=mean(Catch),ABC=mean(ABC)) %>%
    mutate(Stock=as.factor(Stock),Alt=as.factor(Alt)) %>% 
  ggplot(aes(x=Yr,y=SSB,ymax=ub,ymin=lb,color=Stock:Alt,fill=Stock:Alt)) + geom_line(size=2) + 
    geom_ribbon(alpha=.2) +
  expand_limits(y=0) + theme_few() + ggtitle(title)
}
names(df)
dirname <- "goanrs_all"
dirname="goanrs_all";title="N rock sole, GOA";alt=c(1,6)
plot_ssb(dirname="goanrs_all",title="N rock sole, GOA")

plot_catch <- function(dirname,alt=c(1,7),title=NULL){
  df <- read_table(paste0(dirname,"_out/bigfile.out"))
  if(is.null(title)) title=dirname
  df %>% filter(Alt %in% alt) %>% group_by(Yr,Stock,Alt) %>% 
  summarise(ub=quantile(Catch,.9),lb=quantile(Catch,.1),
    SSB=mean(SSB), Catch=mean(Catch),ABC=mean(ABC)) %>%
    mutate(Stock=as.factor(Stock),Alt=as.factor(Alt)) %>% 
  ggplot(aes(x=Yr,y=Catch,ymax=ub,ymin=lb,color=Stock:Alt,fill=Stock:Alt)) + geom_line(size=2) + 
    geom_ribbon(alpha=.2) +
  expand_limits(y=0) + theme_few() + ggtitle(title)
}
names(df)
plot_catch(dirname="goanrs_all",alt=1,title="N rock sole, GOA")
plot_ssb(dirname="goanrs_all",alt=c(7,6),title="N rock sole, GOA")
plot_catch(dirname="goanrs_all",alt=c(7,6),title="N rock sole, GOA")
plot_ssb(dirname="goanrs_western","lN rock sole, western")