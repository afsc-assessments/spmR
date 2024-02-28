##==============================================##
##    Run Projection Model for Alaska Plaice    ##
##        First Created: 09/06/2023             ##
##        Last Updated: 09/12/2023              ##
##==============================================##

library(here)
library(tidyverse)
library(dplyr)

remove_unrequired_files <- T  # remove non-required files from projections folder

##---------------------##
#   Location of files   #
##---------------------##
file_loc <- "Projection_Model"  # path for the location of files need to run the projection model

##----------------##
#   Manual Inputs  #
##----------------##
export_dat <- F             # Export data (T = Yes, F = No)
get_old_summary_values <- T # Combine old summary table with the new one? (T = Yes, F = No)
cur_yr <- 2023              # Current Year
first_proj_yr <- 2021       # First year in the projection period
m <- 0.13                   # natural mortality
tier <- "3a"                # the Tier for the fishery




##---------------------------------------------------------------------------------##
#                            Modify Data files                                      #
#  This section is used to modify the data files for the projection with new data.  #
#  This requires other files and R code not present in this R project. The required #
#  files can be found in the Y drive under SEA_SSMA_MESA\BSAI_Alaska_Plaice\2023    #
##---------------------------------------------------------------------------------##
# # Read in data files
# catch <- read.csv(here(cur_yr, "data","output","akp_annual_catch.csv"))
# proj_catch <- read.csv(here(cur_yr, "data","output",paste("akp_",cur_yr,"_projected_catch.csv",sep = "")))
# 
# # read in spcat.dat file
# spcat <- suppressWarnings(readLines(here(cur_yr,file_loc,"data","bsai_akp_spcat.dat")))
# 
# # change the Number of years with specified catch
# N_sp_cat <- cur_yr - first_proj_yr + 2
# spcat[grep("#_Number_of_years with specified catch",spcat) + 1] <- N_sp_cat
# 
# # obtain the inputted catch for future years
# fut_catch <- catch[grep(first_proj_yr,catch[,1]):length(catch[,1]),]
# fut_catch[length(fut_catch[,1]),2] <- proj_catch[1,2]
# fut_catch <- rbind(fut_catch,tail(fut_catch,1))
# fut_catch[length(fut_catch[,1]),1] <- cur_yr + 1
# 
# # change the inputted catch in future years
# line_loc <- grep("# Catch in each future year",spcat) 
# spcat <- spcat[1:line_loc]
# count <- 1
# for(i in (line_loc+1):(line_loc+length(fut_catch[,1])))
# {
#   spcat[i] <- paste(fut_catch[count,1], round(fut_catch[count,2]),sep = "\t")
#   count <- count + 1
# }
# write(spcat, file = here(cur_yr, file_loc,"data","bsai_akp_spcat.dat"))


##----------------------------------------------------------------------------##
#                       Run Projection Model                                   #
# Note: there's a warning message that comes up with this, you can ignore it,  #
# 		it has to do with the ADMB version the projection model was originally   #
# 		compiled with but has not effect on results                              #
##----------------------------------------------------------------------------##

# From Lee's Code
setwd(here::here(file_loc))
shell("run bsai_akp")

if(remove_unrequired_files ==T){
  unlink(c("alt2_proj.out","alt3b.out","bigfile.out","F_profile.out",
           "fmin.log","eigv.rpt","main.log","means.out","percentdb.out",
           "percentiles.out","spp_catch.dat","variance"))
}
setwd(here::here())

# read alt2_proj.out output file.
alt2 <- read.table(here(file_loc,"bsai_akp_out","alt2_proj.out"), header = T)

alt2_mod <- alt2 %>%
  select(Year,ABC,OFL,SSB,TotBiom) %>%
  rename(Projected_Total_Biomass = TotBiom,
         Female_Spawning_Biomass = SSB) %>%
  filter(Year == cur_yr+1 | Year == cur_yr+2)%>%
  mutate(Max_ABC = ABC)

# read Spawning Stock Biomass at equilibrium values from percentiles.out output file
perc <- readLines(here(file_loc,"bsai_akp_out","percentiles.out"))
SB_vals <- as.numeric(unlist(strsplit(perc[grep("SB0",perc)+1],split = " ")))[1:3]

# read F_OFL and F_ABC values from percentiles.out output file
line_loc <- grep("Fishing_mortalitybsai_akp",perc)[1]+1
cl_nam <- unlist(strsplit(perc[line_loc],split = " "))
n_proj_yrs <- length(alt2[,1])+ 1
F_table <- matrix(0, nrow = n_proj_yrs, ncol = length(cl_nam))
colnames(F_table) <- cl_nam
for(i in 1:n_proj_yrs)  F_table[i,] <- as.numeric(unlist(strsplit(perc[i + line_loc],split = " +")))

F_val <- as_tibble(F_table) %>%
  filter(Year == cur_yr+1 | Year == cur_yr+2) %>%
  select(Year,Fabc,Fofl) %>%
  rename(F_ABC = Fabc,
         F_OFL = Fofl) %>%
  mutate(F_Max_ABC = F_OFL) %>%
  mutate(across(c('F_ABC', "F_OFL", 'F_Max_ABC'), round, 2))
  


# make Summary of Results table (S_of_R)
temp <- tibble(Year = as.integer(c(cur_yr+1, cur_yr+2))) %>%
  add_column("M" = m,
             "Tier" = tier,
             "B_100%" = SB_vals[1],
             "B_40%" = SB_vals[2],
             "B_35%" = SB_vals[3]) %>%
  left_join(alt2_mod, by = c("Year")) %>%
  left_join(F_val, by = c("Year")) %>%
  relocate(c(Projected_Total_Biomass,Female_Spawning_Biomass), .after = "Tier") %>%
  relocate(c(OFL,Max_ABC), .before = "ABC") %>%
  relocate(c(F_OFL,F_Max_ABC, F_ABC), .after = "B_35%") %>%
  add_column("blank" = NA,
             "Status" = c(cur_yr-1,cur_yr),
             "Overfishing" = "n/a",
             "Overfished" = "n/a",
             "Approaching overfished" = "n/a") %>%
  mutate(across(c('ABC', "OFL", 'Max_ABC'), round, 0)) %>%
  mutate(across(everything(), as.character))


S_of_R <- as.data.frame(temp)
rownames(S_of_R) <- S_of_R$Year
S_of_R <- S_of_R[,-1]
S_of_R <- t(S_of_R)

##------------------------------------------------------------------------------##
#  This code does not produce the final Summary of Results table that is present #
#  in the SAFE document. To produce the final produce requires additional files  #
#  that are present in the Y drive under SEA_SSMA_MESA\BSAI_Alaska_Plaice\2023.  #
#  The above code does run the projection model to completion.                   # 
##------------------------------------------------------------------------------##
