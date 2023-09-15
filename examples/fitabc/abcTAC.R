require(rmarkdown);require(devtools)
examplefile=paste0(tempdir(),"/example.docx")
download.file("https://file-examples.com/wp-content/uploads/2017/02/file-sample_100kB.docx",destfile=examplefile)
pandoc_convert('spm.docx',to="markdown",output = "spm.Rmd", options=c("--extract-media=."))

abc <- read.csv('../examples/data/earlyabc.csv')
tac <- read.csv('../examples/data/earlytac.csv')
names(abc)
names(tac)
library(tidyverse)
glimpse(abc)
tac_abc <- abc %>%  pivot_longer(2:17,names_to="species",values_to="Tons") %>% mutate(type="ABC")
tac_abc <- rbind(tac_abc,tac %>%  
  pivot_longer(2:17,names_to="species",values_to="Tons") %>%
                   mutate(type="TAC"))

