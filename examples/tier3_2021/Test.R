library(spmR)
setwd(here("examples"))
getwd()

runspec<-dat2list("spm.dat")
runspec
system("spm")
df <- read_csv("spm_detail.csv")
glimpse(df)
print_Tier3_tables
df |> ggplot(aes(x=Yr,y=SSB,col))
