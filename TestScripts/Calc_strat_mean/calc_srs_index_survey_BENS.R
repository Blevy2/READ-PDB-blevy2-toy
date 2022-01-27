# calculate design-based stratified random sampling indices
#
# liz brooks
# 15 june 2020
# added 
# last update: august 2021
# december 2021: added scalar for total area to get absolute index

#edited by Ben Levy January 2022


#read in results if needed
res <- readRDS("C:\\Users\\benjamin.levy\\Desktop\\Github\\READ-PDB-blevy2-toy\\Results\\ConstPop_ConstTemp\\res_TempCon_PopCon.rds")

rm(list=ls(all=TRUE))      # Remove all variables, etc. from the session memory


library(tibble)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(here)


source("TestScripts/Calc_strat_mean/fn_srs_survey_BENS.R")


# 
# # # # names of species files to read in  (GBhaddock)
# # file.spp <- "ADIOS_SV_164744_GBK_NONE_survey_dist_map_fixed.csv"
# 
# 
# # # names of species files to read in  (GByellowtail flounder)
# file.spp <- "C:/Users/benjamin.levy/Desktop/NOAA/GIS_Stuff/From_Liz/survey_tow_latlon-20220107T192759Z-001/survey_tow_latlon/ADIOS_SV_172909_GBK_NONE_survey_dist_map_fixed.csv"
# 
# 
# # # names of files to read in  (GBcod)
# # file.spp <- "ADIOS_SV_164712_GBK_NONE_survey_dist_map_fixed.csv"
# 
# 
# # # names of files to read in  (GOMcod)
# # file.spp <- "ADIOS_SV_164712_GOM_NONE_survey_dist_map_fixed.csv"
# 
# 
# # note: instead of manually pointing to individual input files ====
# # can also use this code to get a list of all input files matching pattern, and then loop over those species
# sp.list <- list.files(here("C:/Users/benjamin.levy/Desktop/NOAA/GIS_Stuff/From_Liz/survey_tow_latlon-20220107T192759Z-001/survey_tow_latlon/"), pattern="*_survey_dist_map_fixed.csv")
# 


# name of stratum area file  ====
#file.sv.area <- "C:/Users/benjamin.levy/Desktop/NOAA/GIS_Stuff/From_Liz/strata_area.csv"


# read in survey area ====
#sv.area <- as_tibble(read.csv(here(file.sv.area) ) ) %>%
 # select(-STRATUM_NAME)  #I think this is removing the column "stratum_name"

#DEFINE INDIVIDUAL STRATUM AREAS IN SIMILAR MANNER AS ABOVE
stratum <- c(1,2,3,4)
STRATUM_AREA <- c(2500,2500,2500,2500) #100x100 grid so each corner has area 2500
sv.area <- as_tibble(data.frame(stratum,STRATUM_AREA))

# read in species file of tow by tow data from simulation
spp <-  as_tibble(res$survey$log.mat,header=T)


spp.name <- as.character("spp1")
spp.stock <- as.character("Generic")
#spp.itis <- unique(spp$SPECIES_ITIS)



# get total area of stock ====
spp.strata <- unique(spp$stratum)
spp.area <- sum(sv.area$STRATUM_AREA[sv.area$stratum %in% spp.strata]) #TOTAL AREA OF ALL STRATA


# calculate SRS estimates ====
spp.srs <- srs_survey(df=spp, sa=sv.area, str=NULL, ta=0.01, sppname = spp.name  )  # if strata=NULL, the function will use the unique strata set found in df
                                                       # if strata!=NULL, the function will use that vector of strata
spp.srs.rel.and.absolute <- spp.srs %>%
  mutate(mean.yr.absolute=mean.yr*spp.area, sd.mean.yr.absolute=sd.mean.yr*spp.area,
         CV.absolute=sd.mean.yr.absolute/mean.yr.absolute)

write.csv(spp.srs.rel.and.absolute, file=paste(spp.name, spp.stock, "_SRS_from_Rcode.csv", sep="."), row.names=F)

# Note: in spp.srs.rel.and.absolute, the relative mean index and sd are "mean.yr" and "sd.mean.yr"
# the mean index scaled by total area ("absolute abundance index") are "mean.yr.absolute" and "sd.mean.yr.absolute"
