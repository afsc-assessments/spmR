library(tidyverse)
library(ggplot2)
library(ggthemes)
system("run.sh goanrs_central")
system("run.sh goanrs_western")

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