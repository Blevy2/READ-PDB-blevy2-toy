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
#data_spp1 <- read_csv(file="Results/ConstPop_ConstTemp/spp1_SRS_16strata.csv")
#data_spp2 <- read_csv(file="Results/ConstPop_ConstTemp/spp2_SRS_16strata.csv")

#constant pop, increasing temp
#data_spp1 <- read_csv(file="Results/ConstPop_IncrTemp/spp1_SRS_16strata.csv")
#data_spp2 <- read_csv(file="Results/ConstPop_IncrTemp/spp2_SRS_16strata.csv")

#increasing pop, constant temp
#data_spp1 <- read_csv(here("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/Results/IncrPop_ConstTemp", "spp1.Generic._SRS_from_Rcode.csv"))
#data_spp2 <- read_csv(here("C:/Users/benjamin.levy/Desktop/Github/READ-PDB-blevy2-toy/Results/IncrPop_ConstTemp", "spp2.Generic._SRS_from_Rcode.csv"))

#increasing pop, increasing temp
# data_spp1 <- read_csv(file="Results/IncrPop_IncrTemp/spp1_SRS_16strata.csv")
# data_spp2 <- read_csv(file="Results/IncrPop_IncrTemp/spp2_SRS_16strata.csv")

# #decreasing pop, constant temp
# data_spp1 <- read_csv(file="Results/DecrPop_ConstTemp/spp1_SRS_16strata.csv")
# data_spp2 <- read_csv(file="Results/DecrPop_ConstTemp/spp2_SRS_16strata.csv")

#decreasing pop, increasing temp
data_spp1 <- read_csv(file="Results/DecrPop_IncrTemp/spp1_SRS_16strata.csv")
data_spp2 <- read_csv(file="Results/DecrPop_IncrTemp/spp2_SRS_16strata.csv")

#names(data_spp1)
#head(data)
#summary(data)



##########################################
# plot some things
##########################################

#spp 1 single plot, 2 lines
ggplot(data = data_spp1,aes(x=year,y=mean.yr.absolute, group=season,color=season))+
  geom_point()+
  geom_line()+
  labs(title="Spp1")



#spp2 single plot, 2 lines
ggplot(data = data_spp2,aes(x=year,y=mean.yr.absolute,group=season,color=season))+
  geom_point()+
  geom_line()+
  labs(title="Spp2")


#setup groups by season
gg_season_spp1 <- ggplot(data=data_spp1,aes(x=year,y=mean.yr.absolute,group=season))
gg_season_spp2 <- ggplot(data=data_spp2,aes(x=year,y=mean.yr.absolute,group=season))

#plot spp1
gg_season_spp1 +
  geom_errorbar(aes(ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute)),width=.3) +
  geom_point()+
  geom_line()+
  labs(x="year",y="mean.yr.absolute", title = "Spp1") +
  facet_wrap(~ season)

#plot spp2
gg_season_spp2 +
  geom_errorbar(aes(ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute)),width=.3) +
  geom_point()+
  geom_line() +
  labs(x="year",y="mean.yr.absolute", title = "Spp2") +
  facet_wrap(~ season)


#PLOTTING ALL DATA BY SPECIES AND SEASON (EACH 1X2 GRID)

#setup groups by scenario
gg_spp1_scen <- ggplot(data=all_data_spp1,aes(x=year,y=mean.yr.absolute,group=Scenario, color=Scenario))
gg_spp2_scen <- ggplot(data=all_data_spp2,aes(x=year,y=mean.yr.absolute,group=Scenario,color=Scenario))


#plot scenario groups by season
#spp1
gg_spp1_scen +
  #calculating 95% confidence intervals with geom_errorbar. N_samples_strat could be fixed at 10. Added it to calculation after
  geom_errorbar(aes(ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute)),width=.3) +
  geom_point()+
  geom_line(aes(color=Scenario))+
  labs(x="year",y="mean.yr.absolute", title = "Spp1") +
  facet_wrap(~ season)

# spp2
gg_spp2_scen +
  #calculating 95% confidence intervals with geom_errorbar. N_samples_strat could be fixed at 10. Added it to calculation after
  geom_errorbar(aes(ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute)),width=.3) +
  geom_point()+
  geom_line(aes(color=Scenario))+
  labs(x="year",y="mean.yr.absolute", title = "Spp2") +
  facet_wrap(~ season)



#PLOTTING ALL DATA BY SPECIES AND Scearnio and season (EACH 2X3 GRID)

#setup groups by scenario
gg_spp1_scen <- ggplot(data=all_data_spp1,aes(x=year,y=mean.yr.absolute,group=season, color=season))
gg_spp2_scen <- ggplot(data=all_data_spp2,aes(x=year,y=mean.yr.absolute,group=season,color=season))


#plot scenario groups by season
#spp1
gg_spp1_scen +
  #calculating 95% confidence intervals with geom_errorbar. N_samples_strat could be fixed at 10. Added it to calculation after
  geom_errorbar(aes(ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute)),width=.3) +
  geom_point()+
  geom_line(aes(color=season))+
  labs(x="year",y="mean.yr.absolute", title = "Spp1") +
  facet_wrap(~ Scenario)

# spp2
gg_spp2_scen +
  #calculating 95% confidence intervals with geom_errorbar. N_samples_strat could be fixed at 10. Added it to calculation after
  geom_errorbar(aes(ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute)),width=.3) +
  geom_point()+
  geom_line(aes(color=season))+
  labs(x="year",y="mean.yr.absolute", title = "Spp2") +
  facet_wrap(~ Scenario)





#setup groups by scenario
gg_spp1_scen <- ggplot(data=all_data_spp1,aes(x=year,y=mean.yr.absolute,group=Scenario, color=Scenario))
gg_spp2_scen <- ggplot(data=all_data_spp2,aes(x=year,y=mean.yr.absolute,group=Scenario,color=Scenario))


#plot scenario groups by season
#spp1
gg_spp1_scen +
  #calculating 95% confidence intervals with geom_errorbar. N_samples_strat could be fixed at 10. Added it to calculation after
  geom_errorbar(aes(ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute)),width=.3) +
  geom_point()+
  geom_line(aes(color=Scenario))+
  labs(x="year",y="mean.yr.absolute", title = "Spp1") +
  facet_wrap( c("season","ScenarioA"), scales = "free_y")

# spp2
gg_spp2_scen +
  #calculating 95% confidence intervals with geom_errorbar. N_samples_strat could be fixed at 10. Added it to calculation after
  geom_errorbar(aes(ymin=mean.yr.absolute-(1.96*sd.mean.yr.absolute), ymax=mean.yr.absolute+(1.96*sd.mean.yr.absolute)),width=.3) +
  geom_point()+
  geom_line(aes(color=Scenario))+
  labs(x="year",y="mean.yr.absolute", title = "Spp2") +
  facet_wrap(c("season","ScenarioA"), scales = "free_y")



