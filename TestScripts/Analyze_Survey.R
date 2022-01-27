# read in csv with stratified mean calculation and plot things

#read in csv

library(tidyverse)
library(readxl)
library(here)


##################################
## Load data
##################################

#ALWAYS LOAD ALL DATA. UPDATE THIS CSV FILE AS THEY COME OUT
all_data_spp1 <- read_csv(here("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/Results", "spp1.Generic._SRS_from_Rcode_ALLDATA.csv"))
all_data_spp2 <- read_csv(here("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/Results", "spp2.Generic._SRS_from_Rcode_ALLDATA.csv"))


#CHOOSE JUST ONE OF THE FOLLOWING
#constant pop, constant temp
data_spp1 <- read_csv(here("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/Results/ConstPop_ConstTemp", "spp1.Generic._SRS_from_Rcode.csv"))
data_spp2 <- read_csv(here("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/Results/ConstPop_ConstTemp", "spp2.Generic._SRS_from_Rcode.csv"))

#constant pop, increasing temp
#data_spp1 <- read_csv(here("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/Results/ConstPop_IncrTemp", "spp1.Generic._SRS_from_Rcode.csv"))
#data_spp2 <- read_csv(here("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/Results/ConstPop_IncrTemp", "spp2.Generic._SRS_from_Rcode.csv"))



#names(data_spp1)
#head(data)
#summary(data)



##########################################
# plot some things
##########################################

#spp 1 single plot, 2 lines
ggplot(data = data_spp1,aes(x=year,y=mean.yr.absolute, group=Season,color=Season))+
  geom_point()+
  geom_line()+
  labs(title="Spp1")



#spp2 single plot, 2 lines
ggplot(data = data_spp2,aes(x=year,y=mean.yr.absolute,group=Season,color=Season))+
  geom_point()+
  geom_line()+
  labs(title="Spp2")


#setup groups by season
gg_season_spp1 <- ggplot(data=data_spp1,aes(x=year,y=mean.yr,group=Season))
gg_season_spp2 <- ggplot(data=data_spp2,aes(x=year,y=mean.yr,group=Season))

#plot spp1
gg_season_spp1 +
  geom_point()+
  geom_line()+
  labs(x="year",y="mean.yr", title = "Spp1") +
  facet_wrap(~ Season)

#plot spp2
gg_season_spp2 +
  geom_point()+
  geom_line() +
  labs(x="year",y="mean.yr", title = "Spp2") +
  facet_wrap(~ Season)



#setup groups by scenario
gg_spp1_scen <- ggplot(data=all_data_spp1,aes(x=year,y=mean.yr,group=Scenario))
gg_spp2_scen <- ggplot(data=all_data_spp2,aes(x=year,y=mean.yr,group=Scenario))


#plot scenario groups by season
#spp1
gg_spp1_scen +
  geom_point()+
  geom_line(aes(color=Scenario))+
  labs(x="year",y="mean.yr", title = "Spp1") +
  facet_wrap(~ Season)

# spp2
gg_spp2_scen +
  geom_point()+
  geom_line(aes(color=Scenario))+
  labs(x="year",y="mean.yr", title = "Spp2") +
  facet_wrap(~ Season)
