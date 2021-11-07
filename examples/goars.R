library(tidyverse)
library(ggplot2)
library(ggthemes)

df <- read_table("bigfile.out")
names(df)
df %>% mutate(Alt=as.factor(Alt)) %>% group_by(Yr,Alt) %>% 
summarise(SSB=mean(SSB),Catch=mean(Catch),ABC=mean(ABC)) %>%
ggplot(aes(x=Yr,y=SSB,color=Alt)) +
geom_line(size=2) + ylim(c(0,40e3)) + theme_few()

df <- read_csv("bigfile.rep")