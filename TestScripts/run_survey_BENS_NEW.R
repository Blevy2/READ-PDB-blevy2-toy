# after simulation takes place, go through and do a random survey


#RUN BELOW IF JUST RAN SINGLE ITERATION
#result <- list()
#result[[1]] <- res



source("R/BENS_init_survey.R")


#CURRENTLY NEED TO MAKE SURE THAT N_STATIONS*#YEARS / #STRATA IS A WHOLE NUMBER OTHERWISE DAY, TOW, YEAR WONT LINEUP WITH NUMBER OF STATIONS
#ALSO NEED N_STATION TO BE DIVISIBLE BY STATIONS_PER_DAY
#ALSO NEED N_STATIONS / STATIONS_PER_DAY <= 52 otherwise wont get to all of them in a year results in NA in the matrix

#setup catch log
#MAY NEED TO RUN A FEW TIMES TO GET MATRIX THAT IS CORRECT SIZE
surv_random <- BENS_init_survey(sim_init = sim,design = 'random_station', n_stations = 80, #this is total per year (20 in each of 4 strata)
                                start_day = 1, stations_per_day = 1, Qs = c("spp1" = 0.1, "spp2"= 0.2),
                                strata_coords = hab$strata, strata_num = hab$stratas, 
                                years_cut = 2 #if running 22 years, remove first 2 years 
)



#use first 4 columns of surv_random$log.mat as a basis for this sampling
#
# column 1: station_no- ranges from 1 to rows*col = 10,000 and represents matrix index of given sample
# column 2: x- x-value of matrix location being sampled
# column 3: y- y-value of matrix location being sampled
# column 4: strata- strata number of location being sampled (all with 1 listed first, then 2, then 3, then 4)


# results stored in res$pop_bios$sppNUMBER$WEEKNUMBER


# suppose we sample in 1st 2 weeks of April (13&14) and 1st 2 weeks of October (wk 37&38)



sample_per_sn <- 10   #samples per season per strata
#10 per season for 2 seasons is 20 samples in each strata over a year, or 80 total per year This would be 1600 over 20 years 

n_spp <- 2     #number of species

nstrata <- 4   #number of strata

strat_samp_tot <- 400 #total number of samples to collect in each strata over entire simulation

nyears <- 20 #number of simulation years

years_cut <- 2 #number of extra years cut off the front


strata_surv <- list()
#pull out each strata survey info and store separately  

temp <- matrix(unlist(surv_random$log.mat),ncol=9, nrow=nstrata*strat_samp_tot)
idx <- 1
for(i in seq(nstrata)){
  
  strata_surv[[i]] <- temp[idx:(idx+strat_samp_tot-1),1:9]
  
  idx <- idx + strat_samp_tot
  # print(idx)
}





#add a column to each strata_surv that will contain the week to sample from


#######################################################
# FIRST CHUNK IS FOR ALL SAMPLES IN EACH STRATA REMOVED IN SINGLE WEEK
######################################################
# S1_wks <- c(13,37)  #strata1 sample weeks- 1st week in each season
# S2_wks <- c(13,37)  #strata2 sample weeks- 1st week in each season
# S3_wks <- c(14,38)  #strata3 sample weeks- 2nd week in each season
# S4_wks <- c(14,38)  #strata4 sample weeks- 2nd week in each season
# 
# S1_seq <- rep(c(rep(S1_wks[1],sample_per_sn),rep(S1_wks[2],sample_per_sn)),nyears)
# S2_seq <- rep(c(rep(S2_wks[1],sample_per_sn),rep(S2_wks[2],sample_per_sn)),nyears) #create sequence that will fit in matrix based on above input
# S3_seq <- rep(c(rep(S3_wks[1],sample_per_sn),rep(S3_wks[2],sample_per_sn)),nyears)
# S4_seq <- rep(c(rep(S4_wks[1],sample_per_sn),rep(S4_wks[2],sample_per_sn)),nyears)
# 
# strata_surv[[1]]<-cbind(strata_surv[[1]],S1_seq)
# strata_surv[[2]]<-cbind(strata_surv[[1]],S2_seq) #add the above sequence as new column in sample matrix
# strata_surv[[3]]<-cbind(strata_surv[[1]],S3_seq)
# strata_surv[[4]]<-cbind(strata_surv[[1]],S4_seq)
#################################################



#######################################################
# SECOND CHUNK SPLITS 10 SAMPLES PER STRATA AS 7 IN ONE WEEK 3 IN THE OTHER 
######################################################
S1_wks <- c(13,13,13,13,13,13,13,14,14,14,37,37,37,37,37,37,37,38,38,38)  #strata1 sample weeks- 7 in 1st week, 3 in second week in each season
S2_wks <- c(13,13,13,13,13,13,13,14,14,14,37,37,37,37,37,37,37,38,38,38)  #strata2 sample weeks- 7 in 1st week, 3 in second week in each season
S3_wks <- c(14,14,14,15,15,15,15,15,15,15,38,38,38,39,39,39,39,39,39,39)  #strata3 sample weeks- 3 in 1st week, 7 in second week in each season
S4_wks <- c(14,14,14,15,15,15,15,15,15,15,38,38,38,39,39,39,39,39,39,39)  #strata4 sample weeks- 3 in 1st week, 7 in second week in each season

S1_seq <- rep(S1_wks,nyears)
S2_seq <- rep(S2_wks,nyears) #create sequence that will fit in matrix based on above input
S3_seq <- rep(S3_wks,nyears)
S4_seq <- rep(S4_wks,nyears)

#################################################



#ADD COLUMN FOR SEASON FOR STRAT MEAN

#season
S1_sn <- c("SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","FALL","FALL","FALL","FALL","FALL","FALL","FALL","FALL","FALL","FALL")  #weeks 13&14 in spring 37&38 FALL
# S2_sn <- c("SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","FALL","FALL","FALL","FALL","FALL","FALL","FALL","FALL","FALL","FALL")  #weeks 13&14 in spring 37&38 FALL
# S3_sn <- c("SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","FALL","FALL","FALL","FALL","FALL","FALL","FALL","FALL","FALL","FALL")  #weeks 13&14 in spring 37&38 FALL
# S4_sn <- c("SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","SPRING","FALL","FALL","FALL","FALL","FALL","FALL","FALL","FALL","FALL","FALL")  #weeks 13&14 in spring 37&38 FALL

#sequence for season
Season <- rep(S1_sn,nyears)




strata_surv[[1]]<-cbind(strata_surv[[1]],S1_seq,Season)
strata_surv[[2]]<-cbind(strata_surv[[2]],S2_seq,Season) #add the above sequence as new column in sample matrix
strata_surv[[3]]<-cbind(strata_surv[[3]],S3_seq,Season)
strata_surv[[4]]<-cbind(strata_surv[[4]],S4_seq,Season)

#name columns
colnames(strata_surv[[1]]) <- c("station_no","x","y","stratum","day","tow","year","spp1","spp2","week","Season")
colnames(strata_surv[[2]]) <- c("station_no","x","y","stratum","day","tow","year","spp1","spp2","week","Season")
colnames(strata_surv[[3]]) <- c("station_no","x","y","stratum","day","tow","year","spp1","spp2","week","Season")
colnames(strata_surv[[4]]) <- c("station_no","x","y","stratum","day","tow","year","spp1","spp2","week","Season")



# #years are out of whack due to init_survey code. Fix it here
# NO, THERE WAS AN ERROR WITH RUNNING INIT_SURVEY WITH NY=22
# for(i in seq(nstrata)){
#   
#   strata_surv[[i]][,"year"] <- rep(3:22,each=length(S1_wks))   #skipping first two years
# }






#then can just run through list and use indices in matrix to populate

# procedure:
# pull out given strata
# run down each strata_surv matrix created above and sample using information in the given matrix

#survey_results <- list("strata 1","strata 2", "strata 3", "strata 4")

all1 <- list()
all2 <- list()
all3 <- list()
all4 <- list()

for(res in seq(length(result))){ #for each simulation result
  
  for(s in seq(nstrata)){  #pull out strata
    
    for(i in seq(length(strata_surv[[s]][,1]))){ #go down list
      
      x <- as.numeric(strata_surv[[s]][i,2])   #x coord in second column
      y <- as.numeric(strata_surv[[s]][i,3])   #y coord in third column  
      year <- as.numeric(strata_surv[[s]][i,7]) #year in 7th column
      week <- as.numeric(strata_surv[[s]][i,10])  #appended sample week into 10th column above
      
      
      strata_surv[[s]][i,8] <- result[[res]][["pop_bios"]][[(week+(52*(year-1)))]][["spp1"]][x,y]   #POPULATIONMATRIX$spp1(week+(52*(year-1))[x,y]   #spp1 in col 8
      strata_surv[[s]][i,9] <- result[[res]][["pop_bios"]][[(week+(52*(year-1)))]][["spp2"]][x,y]   #POPULATIONMATRIX$spp2(week+(52*(year-1))[x,y]   #spp2 in col 9
      
    }
    
    
    
  }
  #store results
  all1[[res]] <- strata_surv[[1]]
  all2[[res]] <- strata_surv[[2]]
  all3[[res]] <- strata_surv[[3]]
  all4[[res]] <- strata_surv[[4]]
  
}





#store above in list to sort throug below
list_all <- vector("list",length(all1))


#PUT ALL RESULTS INTO SINGLE LIST FOR EACH ITERATION. 
#EACH INDIVIDUAL ENTRY WILL CONTAIN ALL SAMPLES FROM ALL STRATA

for(i in seq(length(list_all))){
  
  list_all[[i]] <-rbind(all1[[i]],all2[[i]],all3[[i]],all4[[i]])
    
    
}




#garbage collection
gc()


#BELOW WILL TAKE A SEVERAL MINUTES


####################################################################
# take each survey from each iteration and create more surveys by adding noise to each
####################################################################


samp_per_iter <- 100 #number of times we will add noise to each sample

#procedure for obtaining samp_per_iter samples from each iteration:

# list_all has each strata survey in it. Pull one out, go through each sub list, add noise each time, store

#setup dimensions. 1 for each strata
surv_noise <- vector("list",length(list_all)) 



 
  for(iter in seq(length(list_all))){ #go down each iteration
      temp <- list_all[[iter]]
      
      spp1_sam <- as.numeric(temp[,8])   #spp1 sample in column 8 
      spp2_sam <- as.numeric(temp[,9])   #spp1 sample in column 9
    for(noise_samp in seq(samp_per_iter)){   #add noise to each survey from each iteration samp_per_iter times
    

      
      #adding noise to survey
      temp[,8] <- sapply(spp1_sam,function(x){rlnorm(1,mean=log(x),sdlog=.35)}) #what should sdlog be?
      temp[,9] <- sapply(spp2_sam,function(x){rlnorm(1,mean=log(x),sdlog=.35)})
      
      
      surv_noise[[iter]][[noise_samp]]  <- temp
    
    }
      
  }
  


#surv_noise is a list with the following layers
#1- each strata
  #2- each iteration
    #3- noise added to each iteration



#BELOW WILL TAKE MANY MINUTES


#NOW WE NEED TO CREATE A STRATIFIED MEAN FROM EACH OF THESE SAMPLES
#there are #strata * #iterations * #samp_per_iter total samples

#I AM COPYING FROM CALC_SRS_INDEX_SURVEY_BENS, which was adapted from Liz's code to create below
library(tibble)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(here)

#stop output from below using this option
options(dplyr.summarise.inform = FALSE)

#load file to calculate the stratified mean
source("TestScripts/Calc_strat_mean/fn_srs_survey_BENS.R")



#DEFINE INDIVIDUAL STRATUM AREAS IN SIMILAR MANNER AS ABOVE
stratum <- c(1,2,3,4)
STRATUM_AREA <- c(2500,2500,2500,2500) #100x100 grid so each corner has area 2500
sv.area <- as_tibble(data.frame(stratum,STRATUM_AREA))



#setup dimensions for species 1. 1 for each strata
strat_mean_all_spp1 <- vector("list",length(surv_noise)) 

#setup dimensions for species 2. 1 for each strata
strat_mean_all_spp2 <- vector("list",length(surv_noise)) #4 strata



#go through each strata survey, iteration, sample



  for(iter in seq(length(surv_noise))){
    print(iter)
    for(sample in seq(length(surv_noise[[iter]]))){
      
      spp <- as_tibble(surv_noise[[iter]][[sample]],header=T) #pull out entire survey matrix
      
      # get total area of stock ====
      spp.strata <- unique(spp$stratum)
      spp.area <- sum(sv.area$STRATUM_AREA[sv.area$stratum %in% spp.strata]) #TOTAL AREA OF ALL STRATA
      
      
      # calculate SRS estimates ====
      
      #species 1
      spp.srs.1 <- srs_survey(df=spp, sa=sv.area, str=NULL, ta=1, sppname = "spp1"  )  # if strata=NULL, the function will use the unique strata set found in df
      
      strat_mean_all_spp1[[iter]][[sample]] <- spp.srs.1 %>%
        mutate(mean.yr.absolute=mean.yr*spp.area, sd.mean.yr.absolute=sd.mean.yr*spp.area,
               CV.absolute=sd.mean.yr.absolute/mean.yr.absolute)
      
      #need to convert to matrix so can average later
      strat_mean_all_spp1[[iter]][[sample]] <- data.matrix(strat_mean_all_spp1[[iter]][[sample]])
      
     # strat_mean_all_spp1[[iter]][[sample]] <- as.double(as.matrix(strat_mean_all_spp1[[iter]][[sample]]))
      
      #species 2
      spp.srs.2  <- srs_survey(df=spp, sa=sv.area, str=NULL, ta=1, sppname = "spp2"  )  # if strata=NULL, the function will use the unique strata set found in df
     
      strat_mean_all_spp2[[iter]][[sample]] <- spp.srs.2 %>%
         mutate(mean.yr.absolute=mean.yr*spp.area, sd.mean.yr.absolute=sd.mean.yr*spp.area,
                CV.absolute=sd.mean.yr.absolute/mean.yr.absolute)
      
      #need to convert to matrix so can average later
      strat_mean_all_spp2[[iter]][[sample]] <- data.matrix(strat_mean_all_spp2[[iter]][[sample]])
      
     # strat_mean_all_spp2[[iter]][[sample]] <- as.double(as.matrix(strat_mean_all_spp2[[iter]][[sample]]))
       
    }
    
    
    
  }
  

  
#put them all into single object
strat_mean_all <- list(strat_mean_all_spp1,strat_mean_all_spp2)






  

###########################################################
# First, summarize across all samples within each iteration
###########################################################


sum_survey_iter <- list(list(),list())


for(s in seq(length(strat_mean_all))){ #2 species
  
  for(iter in seq(length(strat_mean_all[[s]]))){
    
    #find mean across each iteration
    sum_survey_iter[[s]][[iter]] <- Reduce("+", strat_mean_all[[s]][[iter]]) / length( strat_mean_all[[s]][[iter]])
    
    #calc standard deviation
    m<- strat_mean_all[[s]][[iter]] #pull out list for given strata
    sd_mat <- matrix(apply(sapply(1:length( strat_mean_all[[s]][[1]][[1]]), 
                                  function(x) unlist(m)[seq(x, length(unlist(m)),
                                                            length( strat_mean_all[[s]][[1]][[1]]) )]), 2, sd), 
                     ncol = length( strat_mean_all[[s]][[1]][[1]][1,]))
    
    
    
    
    #the sd_mat above is mostly 0 since things dont change. just pull out sample columns 2-5 and 7-9 and append them to previous
    sum_survey_iter[[s]][[iter]] <- cbind(sum_survey_iter[[s]][[iter]],sd_mat[,2:5],sd_mat[,7:9])
    
    colnames(sum_survey_iter[[s]][[iter]]) <- c("year","mean.yr","var.mean.yr","sd.mean.yr","CV","season","mean.yr.absolute","sd.mean.yr.absolute","CV.absolute",
                                          "SD.sam.mean.yr","SD.sam.var.mean.yr","SD.sam.sd.mean.yr","SD.sam.CV","SD.sam.mean.yr.absolute","SD.sam.sdmean.yr.absolute","SD.sam.CV.absolute")
  
  }
  
  
}







################################################
# Second, summarize across all iterations
################################################


sum_survey_iter_final <- list(list(),list())


for(s in seq(length(sum_survey_iter))){ #2 species
  

    #find mean across each iteration
    sum_survey_iter_final[[s]] <- Reduce("+", sum_survey_iter[[s]]) / length( sum_survey_iter[[s]])
    
    #calc standard deviation
    m<- sum_survey_iter[[s]] #pull out list for given strata
    sd_mat <- matrix(apply(sapply(1:length( sum_survey_iter[[s]][[1]]), 
                                  function(x) unlist(m)[seq(x, length(unlist(m)),
                                                            length( sum_survey_iter[[s]][[1]]) )]), 2, sd), 
                     ncol = length( sum_survey_iter_final[[s]][1,]))
    
    
    
    
    #the sd_mat above is mostly 0 since things dont change. just pull out sample columns 2-5 and 7-9and append them to previous
    sum_survey_iter_final[[s]] <- cbind(sum_survey_iter[[s]][[iter]],sd_mat[,2:5],sd_mat[,7:9])
    
    colnames(sum_survey_iter_final[[s]]) <- c("year","mean.yr","var.mean.yr","sd.mean.yr","CV","season","mean.yr.absolute","sd.mean.yr.absolute","CV.absolute",
                                                "SD.sam.mean.yr","SD.sam.var.mean.yr","SD.sam.sd.mean.yr","SD.sam.CV","SD.sam.mean.yr.absolute","SD.sam.sdmean.yr.absolute","SD.sam.CV.absolute",
                                                "SD.iter.mean.yr","SD.iter.var.mean.yr","SD.iter.sd.mean.yr","SD.iter.CV","SD.iter.mean.yr.absolute","SD.iter.sdmean.yr.absolute","SD.iter.CV.absolute")
    
    
}

#write csvs
write.csv(sum_survey_iter_final[[1]], file="spp1_SRS.csv", row.names=F)
write.csv(sum_survey_iter_final[[2]], file="spp2_SRS.csv", row.names=F)





############################################################
# Average all survey results to create survey object for res
# averaging across all original iterations (no noise added)
############################################################





#add a column to each strata_surv that will contain the week to sample from


#######################################################
# FIRST CHUNK IS FOR ALL SAMPLES IN EACH STRATA REMOVED IN SINGLE WEEK
######################################################
# S1_wks <- c(13,37)  #strata1 sample weeks- 1st week in each season
# S2_wks <- c(13,37)  #strata2 sample weeks- 1st week in each season
# S3_wks <- c(14,38)  #strata3 sample weeks- 2nd week in each season
# S4_wks <- c(14,38)  #strata4 sample weeks- 2nd week in each season
# 
# S1_seq <- rep(c(rep(S1_wks[1],sample_per_sn),rep(S1_wks[2],sample_per_sn)),nyears)
# S2_seq <- rep(c(rep(S2_wks[1],sample_per_sn),rep(S2_wks[2],sample_per_sn)),nyears) #create sequence that will fit in matrix based on above input
# S3_seq <- rep(c(rep(S3_wks[1],sample_per_sn),rep(S3_wks[2],sample_per_sn)),nyears)
# S4_seq <- rep(c(rep(S4_wks[1],sample_per_sn),rep(S4_wks[2],sample_per_sn)),nyears)
# 
# strata_surv[[1]]<-cbind(strata_surv[[1]],S1_seq)
# strata_surv[[2]]<-cbind(strata_surv[[1]],S2_seq) #add the above sequence as new column in sample matrix
# strata_surv[[3]]<-cbind(strata_surv[[1]],S3_seq)
# strata_surv[[4]]<-cbind(strata_surv[[1]],S4_seq)
#################################################


strata_surv <- list(list(),list(),list(),list())


temp <- matrix(unlist(surv_random$log.mat),ncol=9, nrow=nstrata*strat_samp_tot)
idx <- 1
for(i in seq(nstrata)){
  
  strata_surv[[i]] <- temp[idx:(idx+strat_samp_tot-1),1:9]
  
  idx <- idx + strat_samp_tot
  # print(idx)
}
#pull out each strata survey info and store separately 

#######################################################
# SECOND CHUNK SPLITS 10 SAMPLES PER STRATA AS 7 IN ONE WEEK 3 IN THE OTHER 
######################################################
S1_wks <- c(13,13,13,13,13,13,13,14,14,14,37,37,37,37,37,37,37,38,38,38)  #strata1 sample weeks- 7 in 1st week, 3 in second week in each season
S2_wks <- c(13,13,13,13,13,13,13,14,14,14,37,37,37,37,37,37,37,38,38,38)  #strata2 sample weeks- 7 in 1st week, 3 in second week in each season
S3_wks <- c(14,14,14,15,15,15,15,15,15,15,38,38,38,39,39,39,39,39,39,39)  #strata3 sample weeks- 3 in 1st week, 7 in second week in each season
S4_wks <- c(14,14,14,15,15,15,15,15,15,15,38,38,38,39,39,39,39,39,39,39)  #strata4 sample weeks- 3 in 1st week, 7 in second week in each season

S1_seq <- rep(S1_wks,nyears)
S2_seq <- rep(S2_wks,nyears) #create sequence that will fit in matrix based on above input
S3_seq <- rep(S3_wks,nyears)
S4_seq <- rep(S4_wks,nyears)

#################################################



strata_surv[[1]]<-cbind(strata_surv[[1]],S1_seq)
strata_surv[[2]]<-cbind(strata_surv[[2]],S2_seq) #add the above sequence as new column in sample matrix
strata_surv[[3]]<-cbind(strata_surv[[3]],S3_seq)
strata_surv[[4]]<-cbind(strata_surv[[4]],S4_seq)

#name columns
colnames(strata_surv[[1]]) <- c("station_no","x","y","stratum","day","tow","year","spp1","spp2","week")
colnames(strata_surv[[2]]) <- c("station_no","x","y","stratum","day","tow","year","spp1","spp2","week")
colnames(strata_surv[[3]]) <- c("station_no","x","y","stratum","day","tow","year","spp1","spp2","week")
colnames(strata_surv[[4]]) <- c("station_no","x","y","stratum","day","tow","year","spp1","spp2","week")



# #years are out of whack due to init_survey code. Fix it here
# NO, THERE WAS AN ERROR WITH RUNNING INIT_SURVEY WITH NY=22
# for(i in seq(nstrata)){
#   
#   strata_surv[[i]][,"year"] <- rep(3:22,each=length(S1_wks))   #skipping first two years
# }






#then can just run through list and use indices in matrix to populate

# procedure:
# pull out given strata
# run down each strata_surv matrix created above and sample using information in the given matrix

#survey_results <- list("strata 1","strata 2", "strata 3", "strata 4")

all1 <- list()
all2 <- list()
all3 <- list()
all4 <- list()

for(res in seq(length(result))){ #for each simulation result
  
  for(s in seq(nstrata)){  #pull out strata
    
    for(i in seq(length(strata_surv[[s]][,1]))){ #go down list
      
      x <- strata_surv[[s]][i,2]   #x coord in second column
      y <- strata_surv[[s]][i,3]   #y coord in third column  
      year <- strata_surv[[s]][i,7] #year in 7th column
      week <- strata_surv[[s]][i,10]  #appended sample week into 10th column above
      
      
      strata_surv[[s]][i,8] <- result[[res]][["pop_bios"]][[(week+(52*(year-1)))]][["spp1"]][x,y]   #POPULATIONMATRIX$spp1(week+(52*(year-1))[x,y]   #spp1 in col 8
      strata_surv[[s]][i,9] <- result[[res]][["pop_bios"]][[(week+(52*(year-1)))]][["spp2"]][x,y]   #POPULATIONMATRIX$spp2(week+(52*(year-1))[x,y]   #spp2 in col 9
      
    }
    
    
    
  }
  #store results
  all1[[res]] <- strata_surv[[1]]
  all2[[res]] <- strata_surv[[2]]
  all3[[res]] <- strata_surv[[3]]
  all4[[res]] <- strata_surv[[4]]
  
}

survey_results <- list("strata1","strata2","strata3","strata4")
survey_results[[1]] <- all1
survey_results[[2]] <- all2
survey_results[[3]] <- all3
survey_results[[4]] <- all4 








##################################################################################
#Aggregate results of all survey iterations into single object (mean and sd/variance)
##################################################################################


### FIRST DO IT FOR THE SURVEY RESULTS

#procedure
#1) pull out given strata with all iteration results 
#2) use Reduce to add all items in list together, then divide by total number in list to obtain mean
#3) use info from https://stackoverflow.com/questions/39351013/standard-deviation-over-a-list-of-matrices-in-r to calculate standard deviation

sum_survey_results <- list()


for(i in seq(nstrata)){ #4 strata
  
  #average them
  sum_survey_results[[i]] <- Reduce("+",survey_results[[i]])/length(survey_results[[i]])
  
  #calc standard deviation
  m<-survey_results[[i]] #pull out list for given strata
  sd_mat <- matrix(apply(sapply(1:length(survey_results[[1]][[1]]), 
                                function(x) unlist(m)[seq(x, length(unlist(m)),
                                                          length(survey_results[[1]][[1]]) )]), 2, sd), 
                   ncol = length(survey_results[[1]][[1]][1,]))
  
  
  
  
  #the sd_mat above is mostly 0 since things dont change. just pull out sample columns 8 and 9 and append them to previous
  sum_survey_results[[i]] <- cbind(sum_survey_results[[i]],sd_mat[,8:9])
  
  colnames(sum_survey_results[[i]]) <- c("station_no","x","y","stratum","day","tow","year","spp1","spp2","week","sd_spp1","sd_spp2")
}


#combine back into single log.mat
log.mat <- rbind(sum_survey_results[[1]],sum_survey_results[[2]],sum_survey_results[[3]],sum_survey_results[[4]])













### THIRD SUMMARIZE THE POPULATION RESULTS

#procedure for biomat (sum of population) and recmat (recruitment)
#1) go through each iteration and put individual results in their own list 
#2) use Reduce to add all items in each list together, then by total number in list to obtain mean
#3) use info from https://stackoverflow.com/questions/39351013/standard-deviation-over-a-list-of-matrices-in-r to calculate standard deviation

pop_sum_s1_biomat <- list()
pop_sum_s1_recmat <- list()
pop_sum_s2_biomat <- list()
pop_sum_s2_recmat <- list()


for(i in seq(length(result))){
  
  #make a list of each category from each iteration
  pop_sum_s1_biomat[[i]] <- result[[i]][["pop_summary"]][["spp1"]][["Bio.mat"]] 
  pop_sum_s1_recmat[[i]] <- result[[i]][["pop_summary"]][["spp1"]][["Rec.mat"]] 
  pop_sum_s2_biomat[[i]] <- result[[i]][["pop_summary"]][["spp2"]][["Bio.mat"]]
  pop_sum_s2_recmat[[i]] <- result[[i]][["pop_summary"]][["spp2"]][["Rec.mat"]]
  
}

#FINDING MEANS
sum_pop_sum_s1_biomat <- list()
sum_pop_sum_s1_recmat <- list()
sum_pop_sum_s2_biomat <- list()
sum_pop_sum_s2_recmat <- list()

#biomat matrices of form [year,day]
#recmat are vector of form [recruitment in year]
sum_pop_sum_s1_biomat <- Reduce("+", pop_sum_s1_biomat)/length(pop_sum_s1_biomat)
sum_pop_sum_s1_recmat <- Reduce("+", pop_sum_s1_recmat)/length(pop_sum_s1_recmat)  #FINDING THE MEAN
sum_pop_sum_s2_biomat <- Reduce("+", pop_sum_s2_biomat)/length(pop_sum_s2_biomat) 
sum_pop_sum_s2_recmat <- Reduce("+", pop_sum_s2_recmat)/length(pop_sum_s2_recmat)




#FINDING STANDARD DEV
sd_pop_sum_s1_biomat <- list()
sd_pop_sum_s1_recmat <- list()
sd_pop_sum_s2_biomat <- list()
sd_pop_sum_s2_recmat <- list()


m<-pop_sum_s1_biomat #pull out list to summarize
sd_pop_sum_s1_biomat <- matrix(apply(sapply(1:length(pop_sum_s1_biomat[[1]]), 
                                            function(x) unlist(m)[seq(x, length(unlist(m)),
                                                                      length(pop_sum_s1_biomat[[1]]) )]), 2, sd), 
                               ncol = length(pop_sum_s1_biomat[[1]][1,]))



m<-pop_sum_s1_recmat #pull out list to summarize
sd_pop_sum_s1_recmat <- matrix(apply(sapply(1:length(pop_sum_s1_recmat[[1]]), 
                                            function(x) unlist(m)[seq(x, length(unlist(m)),
                                                                      length(pop_sum_s1_recmat[[1]]) )]), 2, sd), 
                               ncol = length(pop_sum_s1_recmat[[1]][1,]))


m<-pop_sum_s2_biomat #pull out list to summarize
sd_pop_sum_s2_biomat <- matrix(apply(sapply(1:length(pop_sum_s2_biomat[[1]]), 
                                            function(x) unlist(m)[seq(x, length(unlist(m)),
                                                                      length(pop_sum_s2_biomat[[1]]) )]), 2, sd), 
                               ncol = length(pop_sum_s2_biomat[[1]][1,]))


m<-pop_sum_s2_recmat #pull out list to summarize
sd_pop_sum_s2_recmat <- matrix(apply(sapply(1:length(pop_sum_s2_recmat[[1]]), 
                                            function(x) unlist(m)[seq(x, length(unlist(m)),
                                                                      length(pop_sum_s2_recmat[[1]]) )]), 2, sd), 
                               ncol = length(pop_sum_s2_recmat[[1]][1,]))














#procedure for spatial population output
#1) fix week in outer loop
#2) go through entire result list putting each corresponding week in list
#4) summarize that list as PopBios
#5) go on to next week

#was tough calculating SD same as before because list was so long and sapply took a long time
#found info here to update the process for speed: https://stackoverflow.com/questions/38493741/calculating-standard-deviation-of-variables-in-a-large-list-in-r

spat_pop_spp1 <- list()
spat_pop_spp2 <- list()


spat_pop_spp1_sd <- list()
spat_pop_spp2_sd <- list()


for(wk in seq(52*(nyears+years_cut))){ #fix week
  
  temp_wk_spp1 <- list()
  temp_wk_spp2 <- list()
  
  for(res in seq(length(result))){ #go through results
    
    #make list of given week for each species
    temp_wk_spp1[[res]] <-  result[[res]][["pop_bios"]][[wk]][["spp1"]] 
    temp_wk_spp2[[res]] <-  result[[res]][["pop_bios"]][[wk]][["spp2"]] 
    
  }
  
  #average weekly list into single list entry
  spat_pop_spp1[[wk]] <-  Reduce("+", temp_wk_spp1)/length(temp_wk_spp1)
  spat_pop_spp2[[wk]] <-  Reduce("+", temp_wk_spp2)/length(temp_wk_spp2)
  
  
  
  #calculate SD for given week
  
  #spp1
  m<-temp_wk_spp1 #pull out list to summarize
  
  
  list.squared.mean <-  Reduce("+", lapply(temp_wk_spp1, "^", 2)) / length(temp_wk_spp1)
  
  list.mean <- Reduce("+",temp_wk_spp1) / length(temp_wk_spp1)
  
  #list.variance <- list.squared.mean - list.mean^2
  
  list.sd <- sqrt((round(list.squared.mean - list.mean^2,1)))   #sd(x) = sqrt(E(x^2) - (E(x))^2)
  
  spat_pop_spp1_sd[[wk]] <- list.sd
  
  
  
  
  #spp2
  m<-temp_wk_spp2 #pull out list to summarize
  
  list.squared.mean <-  Reduce("+", lapply(temp_wk_spp2, "^", 2)) / length(temp_wk_spp2)
  
  list.mean <- Reduce("+",temp_wk_spp2) / length(temp_wk_spp2)
  
  #list.variance <- list.squared.mean - list.mean^2
  
  list.sd <- sqrt((round(list.squared.mean - list.mean^2,1)))   #sd(x) = sqrt(E(x^2) - (E(x))^2)
  
  spat_pop_spp2_sd[[wk]] <- list.sd
  
  
}

#remove years_cut years from beginning
spat_pop_spp1 <- spat_pop_spp1[(52*years_cut+1):(52*(nyears+years_cut))]
spat_pop_spp2<-spat_pop_spp2[(52*years_cut+1):(52*(nyears+years_cut))]
spat_pop_spp1_sd <- spat_pop_spp1_sd[(52*years_cut+1):(52*(nyears+years_cut))]
spat_pop_spp2_sd <- spat_pop_spp2_sd[(52*years_cut+1):(52*(nyears+years_cut))]








##################################################################
# PUT EVERYTHING INTO OBJECT CALLED res TO MATCH NORMAL OUTPUT SO PLOTTING FUNCTIONS MORE EASILY USED
##################################################################


#build spp1 and spp2 in plot summary
spp1 <- list()
spp2 <- list()
spp1[["Bio.mat"]] <-sum_pop_sum_s1_biomat
spp1[["Rec.mat"]] <- sum_pop_sum_s1_recmat
spp2[["Bio.mat"]] <-sum_pop_sum_s2_biomat
spp2[["Rec.mat"]] <- sum_pop_sum_s2_recmat

spp1[["Bio.mat.sd"]] <-sd_pop_sum_s1_biomat
spp1[["Rec.mat.sd"]] <- sd_pop_sum_s1_recmat
spp2[["Bio.mat.sd"]] <-sd_pop_sum_s2_biomat
spp2[["Rec.mat.sd"]] <- sd_pop_sum_s2_recmat



#put spp1 and spp2 into pop_summary
pop_summary <- list()
pop_summary[["spp1"]] <- spp1
pop_summary[["spp2"]] <- spp2




pop_bios <- list()
for(i in seq(52*nyears)){
  temp <-list()
  temp[["spp1"]] <- spat_pop_spp1[[i]]
  temp[["spp2"]] <- spat_pop_spp2[[i]]
  
  pop_bios[[i]] <- temp
  
}


pop_bios_sd <- list()
for(i in seq(52*nyears)){
  # temp <-list()
  # temp[["spp1.sd"]] <- 
  # temp[["spp2.sd"]] <-
  
  pop_bios_sd[["spp1"]][[i]] <- spat_pop_spp1_sd[[i]]
  pop_bios_sd[["spp2"]][[i]] <- spat_pop_spp2_sd[[i]]
}



#put everything into list res
res <- list()

res[["pop_summary"]] <- pop_summary
res[["pop_bios"]] <- pop_bios
res[["survey"]][["log.mat"]] <- log.mat


res[["pop_bios_sd"]] <- pop_bios_sd




##################
# Preparing things to plot
##################


#NEXT CHUNK CALCULATES APPROPRIATE BREAKS TO USE IN PLOTTING EACH SPECIES
########################################################
#relist so all pop values are in a sublist for each species rather than by week
########################################################
new_pop_bios <- list(list(),list())

new_pop_bios_singleList <- list(matrix(nc=100),matrix(nc=100))

new_pop_bios_SD_singleList <- list(matrix(nc=100),matrix(nc=100))

for(s in seq(length(hab[["hab"]]))){
  
  for(i in seq(length(res[["pop_bios"]]))){
    #print(s)
    #print(i)
    new_pop_bios[[s]][[i]] <- res$pop_bios[[i]][[s]]
    
    new_pop_bios_singleList[[s]] <- rbind(as.matrix(new_pop_bios_singleList[[s]]),as.matrix(res$pop_bios[[i]][[s]]))
    
    new_pop_bios_SD_singleList[[s]] <- rbind(as.matrix(new_pop_bios_SD_singleList[[s]]),as.matrix(res$pop_bios_sd[[s]][[i]]))
    
  }}


#creating new breaks for plotting
Qbreaks_list <- list(vector(),vector())

#first for population values
for(s in seq(length(hab[["hab"]]))){
  Qbreaks <- classInt::classIntervals(var=as.vector(round(new_pop_bios_singleList[[s]],0)), style = "fisher") 
  
  #remove zeros from breaks
  Qbreaks2 <- Qbreaks[["brks"]][!Qbreaks[["brks"]] %in% 0]
  Qbreaks2 <- append(0,Qbreaks2)#put single 0 back to start
  
  Qbreaks_list[[s]] <- Qbreaks2
  
}

#second for SD values
for(s in seq(length(hab[["hab"]]))){
  Qbreaks <- classInt::classIntervals(var=as.vector(round(new_pop_bios_SD_singleList[[s]],0)), style = "fisher") 
  
  #remove zeros from breaks
  Qbreaks2 <- Qbreaks[["brks"]][!Qbreaks[["brks"]] %in% 0]
  Qbreaks2 <- append(0,Qbreaks2)#put single 0 back to start
  
  Qbreaks_list[[s+2]] <- Qbreaks2
  
}


names(Qbreaks_list)  <- c("spp1_pop","spp2_pop","spp1_pop_SD","spp2_pop_SD")










#this plots spatial standard deviation in population. Each page is first week in a month for entire simulation


source("R/Bens_plot_pop_spatiotemp.R")

Bens_plot_pop_spatiotemp(results = res, timestep = 'daily',plot_weekly=TRUE,
                         plot_monthly = TRUE, save.location = "testfolder", 
                         ColBreaks = NULL)
#ColBreaks = Qbreaks_list)



















#################################################################################
# if res["pop_bios"] is out of order due to error in original run_sim 
# use this chunk to reorder correctly
#################################################################################


nyears <- 20
week_p_yr <- 52

res_temp <- list()
res_temp[["pop_bios"]] <- res[["pop_bios"]] #save them just in case

temp_bios <- vector("list", nyears*week_p_yr)
dim(temp_bios) <- c(nyears, week_p_yr)

for(i in seq(nyears*week_p_yr)){
  
  temp_bios[[i]] <- res[["pop_bios"]][[i]] #put them back the way they came out of the simulation
  
}

#reverse order. they are saved above if need them
res[["pop_bios"]] <- t(temp_bios)





