#====================================================================================================
#================ Instructions
#====================================================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
ggplot2::theme_set(cowplot::theme_cowplot(font_size = 13) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

# The code provided in this script runs the AFSC projection model 
# in order to increase efficiency and reduce transcription errors from
# spreadsheets to text files. This code has been customized to the GOA
# Rougheye/Blackspotted complex (REBS). This code also includes toggles to
# accomodate running the script in an 'on' and 'off' year (REMINDER: if you
# are performing a full assessment in an 'off' year, you will need to change
# parts of this code). To run this code each year a couple of hard-wired
# inputs need to be updated.
#	Input update 1: Define the most recent 3 full years TAC (i.e., if the
#		current year is X, these would be TACs for years X-3, X-2, and X-1):

# https://www.npfmc.org/bering-seaaleutian-islands-groundfish/
		# 1327 # 2017 TAC
		# 1444 # 2018 TAC
		# 1428 # 2019 TAC
		TAC <- c(1209, 1212, 788)
		# 1209 # 2020 TAC
		# 1212 # 2021 TAC
		# 788 # 2022 TAC
		
#	Input update 2: Define your oracle username and password here (for AKFIN):

		username="jsullivan"
		password=""

# The steps that this script works through are as follows:
#	Step 1: This step reads in the current catch data from AKFIN and estimates
#		the catch through the end of the current year and the yeild ratio of
#		catch to TAC for the last full 3 years. So that we can look back on
#		the data to investigate any issues the observer and landings raw catch
#		data is written to the folder 'catch data used'
#	Step 2: This step gets the initial data files set up for the projection
#		model. These files include: (1) the base species-specific data file
#		read in from the proj.dat file (REMINDER: for this file, when the 
#		finalized assessment is reached you need to copy-paste the model into
#		the species-specific folder in the 'FINAL assmnts' folder), and (2) the
#		setup.dat file, which only updates the Begin Year for the projection.
#	Step 3: This step runs the projection model for the max F scenario (from 
#		which you get the endyr+1 and +2 catches for the author's F scenario).
#		In this step what is done is (1) the spp_catch.dat/goa_rebs_max_spcat.dat
#		file is compiled (NOTE: the date to which catch is available in the
#		current year is written in this file next to the current year's catch),
#		(2) the projection model is run, (3) the results are read in and written
#		to the 'goa_rebs_max_out' folder and (4) the bigsum.dat file is created
#		(note that the data is now sorted and no sorting in excel is needed)
#	Step 4: This step runs the projection model for the Author's F scenario.
#		In this step what is done is (1) the endyr+1 and +2 catches are
#		estimated from the yield ratio and Max F catch scenario catches and
#		the spp_catch.dat/goa_rebs_spcat.dat file is compiled (NOTE: the date
#		to which catch is available in the current year is written in this
#		file next to the current year's catch), (2) the projection model is run,
#		(3) the results are read in and written to the 'goa_rebs_out' folder
#		and (4) the bigsum.dat file is created
#	Step 5: Now we return to the original way the executive summary and
#		projection alternatives table has been created and copy-paste the 
#		percentiles/bigsum files into the projections table spreadsheet.


#====================================================================================================
# Step 0: Define path, species-specific parameters, and endyr
#====================================================================================================

path <- "~/Assessments/SAFES2022/PROJECTIONS"
species <- 307
group_code <- 'REYE'
endyr <- 2022 # as.numeric(substr(Sys.time(), 1, 4))

#====================================================================================================
# Step 1: Get catch data and estimate catch to end of year and yield ratio
#====================================================================================================

# Set up connection, read in observer/catch landings data
library(RODBC)
channel = odbcConnect("akfin", uid = username, pwd = password, believeNRows = FALSE)
catch_data <- sqlQuery(channel,paste("SELECT    COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR, COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_NAME, 
                                                COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE, COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR, 
                                                COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_AREA, COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA, 
                                                COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE, COUNCIL.COMPREHENSIVE_BLEND_CA.WEIGHT_POSTED 
                                      FROM      COUNCIL.COMPREHENSIVE_BLEND_CA 
                                      WHERE     COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_AREA = 'GOA' AND 
                                                COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR <= ", endyr, " AND
                                                COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE = '", group_code, "'",
                                   sep = ""))
obs_data<-sqlQuery(channel,paste("SELECT    NORPAC.DEBRIEFED_SPCOMP_MV.YEAR, NORPAC.DEBRIEFED_SPCOMP_MV.HAUL_DATE, 
                                            NORPAC.DEBRIEFED_SPCOMP_MV.SPECIES, NORPAC.DEBRIEFED_HAUL_MV.FMP_AREA, 
                                            NORPAC.DEBRIEFED_SPCOMP_MV.EXTRAPOLATED_WEIGHT 
                                 FROM       NORPAC.DEBRIEFED_SPCOMP_MV 
                                 INNER JOIN NORPAC.DEBRIEFED_HAUL_MV ON NORPAC.DEBRIEFED_SPCOMP_MV.JOIN_KEY = NORPAC.DEBRIEFED_HAUL_MV.JOIN_KEY 
                                 WHERE      NORPAC.DEBRIEFED_SPCOMP_MV.YEAR BETWEEN ", endyr-3, " AND ", endyr-1," AND 
                                            NORPAC.DEBRIEFED_HAUL_MV.FMP_AREA = 'GOA' AND 
                                            NORPAC.DEBRIEFED_SPCOMP_MV.SPECIES = ", species,
                                 sep = ""))
write.csv(catch_data,paste(path,"/catch data used/catch_data_", group_code, " .csv", sep = ""))
write.csv(obs_data,paste(path,"/catch data used/obs_catch_data_", group_code, ".csv", sep = ""))

catch_data %>% 
  rename_all(tolower) %>% 
  group_by(year) %>% 
  dplyr::summarise(catch = sum(weight_posted))

catch_data %>% 
  rename_all(tolower) %>% 
  mutate(regulatory_area_name = case_when(fmp_subarea %in% c('WY', 'SE') ~ 'EASTERN GOA',
                                          fmp_subarea == 'WG' ~ 'WESTERN GOA',
                                          fmp_subarea == 'CG' ~ 'CENTRAL GOA')) %>% 
  group_by(year, regulatory_area_name) %>% 
  dplyr::summarise(catch = sum(weight_posted)) %>% 
  tidyr::pivot_wider(id_cols = year, values_from = catch, names_from = regulatory_area_name) %>% 
  ungroup() %>% 
  slice_tail(n = 2)

gearcatch <- catch_data %>% 
  rename_all(tolower) %>% 
  group_by(year, fmp_gear) %>% 
  dplyr::summarise(gear_catch = sum(weight_posted)) %>% 
  group_by(year) %>% 
  mutate(catch = sum(gear_catch),
         p_catch = gear_catch / catch) %>% 
  tidyr::pivot_wider(id_cols = year, names_from = fmp_gear, values_from = p_catch)
gearcatch
mean(gearcatch$HAL)
mean(gearcatch$TRW)

# Estimate ratio of catch from current date to end of year (Endyr_ratio)
yr<-seq(endyr-3,endyr-1)
Endyr_C<-matrix(nrow=length(yr),ncol=2)
colnames(Endyr_C)<-c("Oct_C","Total_C")
rownames(Endyr_C)<-yr
for(y in 1:length(yr)){
  Data<-subset(obs_data,obs_data$YEAR==yr[y])
  Data_pre<-subset(Data,as.Date(Data$HAUL_DATE)<=paste(yr[y],substr(max(as.Date(catch_data$WEEK_END_DATE)),5,10),sep=""))
  Endyr_C[y,1]<-sum(Data_pre$EXTRAPOLATED_WEIGHT, na.rm = T)
  Endyr_C[y,2]<-sum(Data$EXTRAPOLATED_WEIGHT, na.rm = T)
  }
Endyr_ratio<-1+(sum(Endyr_C[,2])-sum(Endyr_C[,1]))/sum(Endyr_C[,1])
Catch_end_date<-max(as.Date(catch_data$WEEK_END_DATE))

# Compute total catch and estimate yeild ratio of TAC to catch
yr<-seq(endyr-3,endyr)
C<-matrix(nrow=length(yr),ncol=1)
rownames(C)<-yr
for(y in 1:length(yr)){
  Data<-subset(catch_data,catch_data$YEAR==yr[y])
  C[y,1]<-sum(Data$WEIGHT_POSTED)
}
C[length(C[,1]),1]<-C[length(C[,1]),1]*Endyr_ratio
yld_rat<-mean(C[(length(C)-3):(length(C)-1)]/TAC)
yld_rat

#====================================================================================================
# Step 2: Get initial projection model data files set up
#====================================================================================================

# Read in proj.dat and write to projection model data file
proj<-readLines(paste(path,"/FINAL assmnts/RE/proj.dat",sep=""),warn=FALSE)
DAT_NAME<-"goa_rebs.dat"
write.table(proj,file=paste(path,"/model/data/",DAT_NAME,sep=""),,quote=F,,,,,row.names=F,col.names=F)

# Get setup.dat file setup
setup<-readLines(paste(path,"/model/setup.dat",sep=""),warn=FALSE)
L_endyr<-grep("#_Begin Year",setup)
# test if endyr is even (projection only year) or odd (full assessment year)
if(endyr %% 2 == 0){
  setup[L_endyr]<-paste(endyr-1," #_Begin Year")
}else{
  setup[L_endyr]<-paste(endyr," #_Begin Year")
}
write.table(setup,file=paste(path,"/model/setup.dat",sep=""),,quote=F,,,,,row.names=F,col.names=F)


#=================================================================================
# Step 3: Run Max F projection scenario
#====================================================================================================

# Setup spp_catch.dat file (toggled to test for on/off year)
L_1<-"#_Number_of_years with specified catch"
if(endyr %% 2 == 0){
  L_2<-2}else{
    L_2<-1
    }
L_3<-"# Number of species"
L_4<-1
L_5<-"# data files for each species"
L_6<-paste("data/goa_rebs.dat",sep="")
L_7<-"# ABC Multipliers"
L_8<-1
L_9<-"# Population scalars"
L_10<-1000
L_11<-"# Number of TAC model categories"
L_12<-1
L_13<-"# TAC model indices (for aggregating)"
L_14<-1
L_15<-"# Catch in each future year" # Includes toggle for on/off year
if(endyr %% 2 == 0){
  L_16<-paste(paste(endyr-1,round(C[which(as.numeric(rownames(C))==(endyr-1)),1],digits=4),sep="\t"),"# Finalized previous year catch",sep=" ")
  L_17<-paste(paste(endyr,round(C[which(as.numeric(rownames(C))==(endyr)),1],digits=4),sep="\t"),"# Estimated from catch thru",Catch_end_date,"with expansion factor =",Endyr_ratio,sep=" ")
  spp_catch<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17)}else{
    L_16<-paste(paste(endyr,round(C[which(as.numeric(rownames(C))==(endyr)),1],digits=4),sep="\t"),"# Estimated from catch thru",Catch_end_date,"with expansion factor =",Endyr_ratio,sep=" ")
    spp_catch<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16)}
write.table(spp_catch,file=paste(path,"/model/spp_catch.dat",sep=""),,quote=F,,,,,row.names=F,col.names=F)
write.table(spp_catch,file=paste(path,"/model/data/goa_rebs_max_spcat.dat",sep=""),,quote=F,,,,,row.names=F,col.names=F)

# Run model
#	NOTE: there's a warning message that comes up with this, you can ignore it,
#		it has to do with the ADMB version the projection model was originally
#		compiled with but has not effect on results
setwd(paste(path,"/model",sep=""))
shell("main.exe")

# Read results from Max scenario
bigfile_write<-readLines(paste(path,"/model/bigfile.out",sep=""),warn=FALSE)
bigfile<-read.delim(paste(path,"/model/bigfile.out",sep=""),sep="",header=T)
F_profile<-readLines(paste(path,"/model/F_profile.out",sep=""),warn=FALSE)
means<-readLines(paste(path,"/model/means.out",sep=""),warn=FALSE)
percentdb<-readLines(paste(path,"/model/percentdb.out",sep=""),warn=FALSE)
percentiles<-readLines(paste(path,"/model/percentiles.out",sep=""),warn=FALSE)

# Make bigsum file
bigsum<-matrix(nrow=98,ncol=9)
colnames(bigsum)<-c("Alt","Stock","Year","ABC","OFL","Catch","SSB","F","Total_Biom")
Alt1<-matrix(nrow=14,ncol=9);Alt1[,1]<-1;Alt1[,2]<-as.character(bigfile$Spp[1]);yrs<-sort(unique(bigfile$Yr));Alt1[,3]<-yrs
for(y in 1:length(yrs)){bigsum_data<-subset(bigfile,bigfile$Alternative==1);bigsum_data<-subset(bigsum_data[,4:9],bigsum_data$Yr==yrs[y]);Alt1[y,4:9]<-colMeans(bigsum_data)}
Alt2<-matrix(nrow=14,ncol=9);Alt2[,1]<-2;Alt2[,2]<-as.character(bigfile$Spp[1]);Alt2[,3]<-yrs
for(y in 1:length(yrs)){bigsum_data<-subset(bigfile,bigfile$Alternative==2);bigsum_data<-subset(bigsum_data[,4:9],bigsum_data$Yr==yrs[y]);Alt2[y,4:9]<-colMeans(bigsum_data)}
Alt3<-matrix(nrow=14,ncol=9);Alt3[,1]<-3;Alt3[,2]<-as.character(bigfile$Spp[1]);Alt3[,3]<-yrs
for(y in 1:length(yrs)){bigsum_data<-subset(bigfile,bigfile$Alternative==3);bigsum_data<-subset(bigsum_data[,4:9],bigsum_data$Yr==yrs[y]);Alt3[y,4:9]<-colMeans(bigsum_data)}
Alt4<-matrix(nrow=14,ncol=9);Alt4[,1]<-4;Alt4[,2]<-as.character(bigfile$Spp[1]);Alt4[,3]<-yrs
for(y in 1:length(yrs)){bigsum_data<-subset(bigfile,bigfile$Alternative==4);bigsum_data<-subset(bigsum_data[,4:9],bigsum_data$Yr==yrs[y]);Alt4[y,4:9]<-colMeans(bigsum_data)}
Alt5<-matrix(nrow=14,ncol=9);Alt5[,1]<-5;Alt5[,2]<-as.character(bigfile$Spp[1]);Alt5[,3]<-yrs
for(y in 1:length(yrs)){bigsum_data<-subset(bigfile,bigfile$Alternative==5);bigsum_data<-subset(bigsum_data[,4:9],bigsum_data$Yr==yrs[y]);Alt5[y,4:9]<-colMeans(bigsum_data)}
Alt6<-matrix(nrow=14,ncol=9);Alt6[,1]<-6;Alt6[,2]<-as.character(bigfile$Spp[1]);Alt6[,3]<-yrs
for(y in 1:length(yrs)){bigsum_data<-subset(bigfile,bigfile$Alternative==6);bigsum_data<-subset(bigsum_data[,4:9],bigsum_data$Yr==yrs[y]);Alt6[y,4:9]<-colMeans(bigsum_data)}
Alt7<-matrix(nrow=14,ncol=9);Alt7[,1]<-7;Alt7[,2]<-as.character(bigfile$Spp[1]);Alt7[,3]<-yrs
for(y in 1:length(yrs)){bigsum_data<-subset(bigfile,bigfile$Alternative==7);bigsum_data<-subset(bigsum_data[,4:9],bigsum_data$Yr==yrs[y]);Alt7[y,4:9]<-colMeans(bigsum_data)}
bigsum<-rbind(Alt1,Alt2,Alt3,Alt4,Alt5,Alt6,Alt7)

# Write results from max scenario
write.table(bigfile_write,file=paste(path,"/goa_rebs_max_out/bigfile.out",sep=""),,quote=F,,,,,row.names=F,col.names=F)
write.table(F_profile,file=paste(path,"/goa_rebs_max_out/F_profile.out",sep=""),,quote=F,,,,,row.names=F,col.names=F)
write.table(means,file=paste(path,"/goa_rebs_max_out/means.out",sep=""),,quote=F,,,,,row.names=F,col.names=F)
write.table(percentdb,file=paste(path,"/goa_rebs_max_out/percentdb.out",sep=""),,quote=F,,,,,row.names=F,col.names=F)
write.table(percentiles,file=paste(path,"/goa_rebs_max_out/percentiles.out",sep=""),,quote=F,,,,,row.names=F,col.names=F)
write.table(bigsum,file=paste(path,"/goa_rebs_max_out/bigsum.dat",sep=""),,quote=F,,,,,row.names=F,col.names=c("Alt","Stock","Year","ABC","OFL","Catch","SSB","F","Total_Biom"))


#=================================================================================
# Step 4: Run Author's F scenario
#====================================================================================================

# Setup spp_catch.dat file
L_1<-"#_Number_of_years with specified catch"
if(endyr %% 2 == 0){
L_2<-4}else{
L_2<-3}
L_3<-"# Number of species"
L_4<-1
L_5<-"# data files for each species"
L_6<-paste("data/goa_rebs.dat",sep="")
L_7<-"# ABC Multipliers"
L_8<-1
L_9<-"# Population scalars"
L_10<-1000
L_11<-"# Number of TAC model categories"
L_12<-1
L_13<-"# TAC model indices (for aggregating)"
L_14<-1
L_15<-"# Catch in each future year" # Includes toggle for on/off year
if(endyr %% 2 == 0){
L_16<-paste(paste(endyr-1,round(C[which(as.numeric(rownames(C))==(endyr-1)),1],digits=4),sep="\t"),"# Finalized previous year catch",sep=" ")
L_17<-paste(paste(endyr,round(C[which(as.numeric(rownames(C))==(endyr)),1],digits=4),sep="\t"),"# Estimated from catch thru",Catch_end_date,"with expansion factor =",Endyr_ratio,sep=" ")
p1<-percentiles[grep("Catch",percentiles)[1]:grep("Spawning_Biomass",percentiles)[1]]
L_18<-paste(paste(endyr+1,round(as.numeric(strsplit(p1[5],split=" ")[[1]][8])*1000*yld_rat,digits=4),sep="\t"),"# Estimated as Max F scenario catch*yieldratio =",yld_rat,sep=" ")
L_19<-paste(paste(endyr+2,round(as.numeric(strsplit(p1[6],split=" ")[[1]][8])*1000*yld_rat,digits=4),sep="\t"),"# Estimated as Max F scenario catch*yieldratio",yld_rat,sep=" ")
spp_catch<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17,L_18,L_19)}else{
L_16<-paste(paste(endyr,round(C[which(as.numeric(rownames(C))==(endyr)),1],digits=4),sep="\t"),"# Estimated from catch thru",Catch_end_date,"with expansion factor =",Endyr_ratio,sep=" ")
p1<-percentiles[grep("Catch",percentiles)[1]:grep("Spawning_Biomass",percentiles)[1]]
L_17<-paste(paste(endyr+1,round(as.numeric(strsplit(p1[4],split=" ")[[1]][8])*1000*yld_rat,digits=4),sep="\t"),"# Estimated as Max F scenario catch*yieldratio",yld_rat,sep=" ")
L_18<-paste(paste(endyr+2,round(as.numeric(strsplit(p1[5],split=" ")[[1]][8])*1000*yld_rat,digits=4),sep="\t"),"# Estimated as Max F scenario catch*yieldratio",yld_rat,sep=" ")
spp_catch<-c(L_1,L_2,L_3,L_4,L_5,L_6,L_7,L_8,L_9,L_10,L_11,L_12,L_13,L_14,L_15,L_16,L_17,L_18)}
write.table(spp_catch,file=paste(path,"/model/spp_catch.dat",sep=""),,quote=F,,,,,row.names=F,col.names=F)
write.table(spp_catch,file=paste(path,"/model/data/goa_rebs_spcat.dat",sep=""),,quote=F,,,,,row.names=F,col.names=F)

# Run model
#	NOTE: there's a warning message that comes up with this, you can ignore it,
#		it has to do with the ADMB version the projection model was originally
#		compiled with but has not effect on results
setwd(paste(path,"/model",sep=""))
shell("main.exe")

# Read results from Author's F scenario
bigfile_write<-readLines(paste(path,"/model/bigfile.out",sep=""),warn=FALSE)
bigfile<-read.delim(paste(path,"/model/bigfile.out",sep=""),sep="",header=T)
F_profile<-readLines(paste(path,"/model/F_profile.out",sep=""),warn=FALSE)
means<-readLines(paste(path,"/model/means.out",sep=""),warn=FALSE)
percentdb<-readLines(paste(path,"/model/percentdb.out",sep=""),warn=FALSE)
percentiles<-readLines(paste(path,"/model/percentiles.out",sep=""),warn=FALSE)

# Make bigsum file
bigsum<-matrix(nrow=98,ncol=9)
colnames(bigsum)<-c("Alt","Stock","Year","ABC","OFL","Catch","SSB","F","Total_Biom")
Alt1<-matrix(nrow=14,ncol=9);Alt1[,1]<-1;Alt1[,2]<-as.character(bigfile$Spp[1]);yrs<-sort(unique(bigfile$Yr));Alt1[,3]<-yrs
for(y in 1:length(yrs)){bigsum_data<-subset(bigfile,bigfile$Alternative==1);bigsum_data<-subset(bigsum_data[,4:9],bigsum_data$Yr==yrs[y]);Alt1[y,4:9]<-colMeans(bigsum_data)}
Alt2<-matrix(nrow=14,ncol=9);Alt2[,1]<-2;Alt2[,2]<-as.character(bigfile$Spp[1]);Alt2[,3]<-yrs
for(y in 1:length(yrs)){bigsum_data<-subset(bigfile,bigfile$Alternative==2);bigsum_data<-subset(bigsum_data[,4:9],bigsum_data$Yr==yrs[y]);Alt2[y,4:9]<-colMeans(bigsum_data)}
Alt3<-matrix(nrow=14,ncol=9);Alt3[,1]<-3;Alt3[,2]<-as.character(bigfile$Spp[1]);Alt3[,3]<-yrs
for(y in 1:length(yrs)){bigsum_data<-subset(bigfile,bigfile$Alternative==3);bigsum_data<-subset(bigsum_data[,4:9],bigsum_data$Yr==yrs[y]);Alt3[y,4:9]<-colMeans(bigsum_data)}
Alt4<-matrix(nrow=14,ncol=9);Alt4[,1]<-4;Alt4[,2]<-as.character(bigfile$Spp[1]);Alt4[,3]<-yrs
for(y in 1:length(yrs)){bigsum_data<-subset(bigfile,bigfile$Alternative==4);bigsum_data<-subset(bigsum_data[,4:9],bigsum_data$Yr==yrs[y]);Alt4[y,4:9]<-colMeans(bigsum_data)}
Alt5<-matrix(nrow=14,ncol=9);Alt5[,1]<-5;Alt5[,2]<-as.character(bigfile$Spp[1]);Alt5[,3]<-yrs
for(y in 1:length(yrs)){bigsum_data<-subset(bigfile,bigfile$Alternative==5);bigsum_data<-subset(bigsum_data[,4:9],bigsum_data$Yr==yrs[y]);Alt5[y,4:9]<-colMeans(bigsum_data)}
Alt6<-matrix(nrow=14,ncol=9);Alt6[,1]<-6;Alt6[,2]<-as.character(bigfile$Spp[1]);Alt6[,3]<-yrs
for(y in 1:length(yrs)){bigsum_data<-subset(bigfile,bigfile$Alternative==6);bigsum_data<-subset(bigsum_data[,4:9],bigsum_data$Yr==yrs[y]);Alt6[y,4:9]<-colMeans(bigsum_data)}
Alt7<-matrix(nrow=14,ncol=9);Alt7[,1]<-7;Alt7[,2]<-as.character(bigfile$Spp[1]);Alt7[,3]<-yrs
for(y in 1:length(yrs)){bigsum_data<-subset(bigfile,bigfile$Alternative==7);bigsum_data<-subset(bigsum_data[,4:9],bigsum_data$Yr==yrs[y]);Alt7[y,4:9]<-colMeans(bigsum_data)}
bigsum<-rbind(Alt1,Alt2,Alt3,Alt4,Alt5,Alt6,Alt7)

# Write results from max scenario
write.table(bigfile_write,file=paste(path,"/goa_rebs_out/bigfile.out",sep=""),,quote=F,,,,,row.names=F,col.names=F)
write.table(F_profile,file=paste(path,"/goa_rebs_out/F_profile.out",sep=""),,quote=F,,,,,row.names=F,col.names=F)
write.table(means,file=paste(path,"/goa_rebs_out/means.out",sep=""),,quote=F,,,,,row.names=F,col.names=F)
write.table(percentdb,file=paste(path,"/goa_rebs_out/percentdb.out",sep=""),,quote=F,,,,,row.names=F,col.names=F)
write.table(percentiles,file=paste(path,"/goa_rebs_out/percentiles.out",sep=""),,quote=F,,,,,row.names=F,col.names=F)
write.table(bigsum,file=paste(path,"/goa_rebs_out/bigsum.dat",sep=""),,quote=F,,,,,row.names=F,col.names=c("Alt","Stock","Year","ABC","OFL","Catch","SSB","F","Total_Biom"))

# catch to biomass
biomass <- read.delim(paste(path,"/FINAL assmnts/RE/updated_REBS.std",sep=""),sep="",header=T) %>% 
  filter(name == 'tot_biom') %>% 
  mutate(year = 1977:(endyr-1)) %>% 
  dplyr::select(year, biomass = value) #, sd = std.dev)

biomass <- biomass %>% 
  bind_rows(
    # pull manually from Projections spreadsheet/Spec table
    data.frame(year = endyr, biomass = 26037))

#TOTALCATCH - Total catch by year in metric tons 
catch <- data.frame(year = 1977:(endyr-1),
                    # from RE_SARA.dat
                    catch = c(1431.34, 571.935, 647.44, 1338.1, 719.078, 571.229, 628.565, 761.884, 134.417,
                              442.381, 524.304, 1558.79, 2070.4, 2250.09, 350.828, 1085.71, 568.265, 576.517, 
                              700.11, 555.732, 543.457, 661.19, 319.492, 528.613, 589.443, 273.123, 394.131,
                              301.337, 293.936, 370.984, 439.427, 381.963, 275.518, 426.997, 535.953, 566.525, 570.929,
                              743.146, 535.392, 645.445, 524.931, 749.863, 751.913, 384.786, 391.11)) %>% 
                      bind_rows(data.frame(year = endyr, catch = C[4]))

biomass <- biomass %>% left_join(catch)
biomass <- biomass %>% 
  mutate(ratio = catch / biomass)
ggplot(biomass, aes(x = year, y = ratio)) +
  geom_point() +
  geom_line() +
  labs(x = NULL, y = 'Catch/biomass') +
  geom_hline(yintercept = mean(biomass$ratio), lty = 2, col = 'red')
mean(biomass$ratio)
min(biomass$ratio);
max(biomass$ratio)
biomass 
ggsave(paste0(path, '/REBS_catch2biomass.png'), dpi = 300, units = 'in',
       height = 4, width = 7, bg = 'white')
