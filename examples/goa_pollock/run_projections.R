## Script to run the projections for various models
library(tidyverse)
library(GOApollock)
thisyear <- 2023

## Final 2023 model
replist <- readRDS('../results/tmbfit.RDS') %>% get_rep
datlist <- readRDS('../results/tmbfit.RDS')$obj$env$data

## New automated way using the new 'spm' package. Confirmed the
## same as manually doing it with 'proj' in 2022
write_spm_inputs(replist, datlist, 'spm2023')
setwd('spm2023')
system("spm")
setwd("..")
bf <- read.csv('spm2023/spm_detail.csv')
(exec_table <- get_exec_table(replist,bf))
(exec_tableF <- format_exec_table(exec_table))
## write.csv(exec_tableF, file='../results/exec_tableF.csv', row.names=FALSE)
## Need last year's table too to make full exec table
replist0 <- read_pk_rep(path='../model_runs/m22_13_2022_final', endyr=2022)
bf0 <- read.csv('spm2022/spm_detail.csv', header=TRUE)
## was a bug in summary biomass in 2022 so have to fix it here
## with a hack, take out moving forward
sumbio <- 1e6*tail(replist0[['Expected_summary_(age_3+)_biomass']],2)
exec_table0 <- get_exec_table(replist0,bf0)
exec_table0 <- rbind(exec_table0[1,], c('sumbio',sumbio), exec_table0[-1,])
exec_table <- cbind(exec_table0[,-1], exec_table[,-1])
exec_table
write.csv(exec_table, file='../results/exec_table.csv', row.names=FALSE)
saveRDS(bf, file='../results/spm_detail.RDS')

## proj_scens <- bf %>% select(-Spp) %>%
##   group_by(Alternative,Yr)%>%
##   summarize_all('mean') %>% arrange(Alternative,Yr) %>% ungroup
## write.csv(proj_scens, file='../results/proj_scens.csv', row.names=FALSE)


