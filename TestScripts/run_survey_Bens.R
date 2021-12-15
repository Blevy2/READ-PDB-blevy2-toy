# after simulation takes place, go through and do a random survey

source("R/BENS_init_survey.R")


#CURRENTLY NEED TO MAKE SURE THAT N_STATIONS*#YEARS / #STRATA IS A WHOLE NUMBER OTHERWISE DAY, TOW, YEAR WONT LINEUP WITH NUMBER OF STATIONS
#ALSO NEED N_STATION TO BE DIVISIBLE BY STATIONS_PER_DAY
#ALSO NEED N_STATIONS / STATIONS_PER_DAY <= 52 otherwise wont get to all of them in a year results in NA in the matrix

#setup catch log
surv_random <- BENS_init_survey(sim_init = sim,design = 'random_station', n_stations = 80, #this is total per year (20 in each of 4 strata)
                                start_day = 1, stations_per_day = 1, Qs = c("spp1" = 0.1, "spp2"= 0.2),
                                strata_coords = hab$strata, strata_num = hab$stratas )



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



strata_surv[[1]]<-cbind(strata_surv[[1]],S1_seq)
strata_surv[[2]]<-cbind(strata_surv[[2]],S2_seq) #add the above sequence as new column in sample matrix
strata_surv[[3]]<-cbind(strata_surv[[3]],S3_seq)
strata_surv[[4]]<-cbind(strata_surv[[4]],S4_seq)

#name columns
colnames(strata_surv[[1]]) <- c("Matrix index","x","y","strata","day","tow","year","spp1","spp2","week")
colnames(strata_surv[[2]]) <- c("Matrix index","x","y","strata","day","tow","year","spp1","spp2","week")
colnames(strata_surv[[3]]) <- c("Matrix index","x","y","strata","day","tow","year","spp1","spp2","week")
colnames(strata_surv[[4]]) <- c("Matrix index","x","y","strata","day","tow","year","spp1","spp2","week")



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
#Aggregate results of all iterations into single object (mean and variance)
##################################################################################


### FIRST DO IT FOR THE SURVEY RESULTS

#procedure
#1) pull out given strata with all iteration results 
#2) use Reduce to add all items in list together, then by total number in list to obtain mean
#3) use info from https://stackoverflow.com/questions/39351013/standard-deviation-over-a-list-of-matrices-in-r to calculate standard deviation

sum_survey_results <- list()


for(i in seq(nstrata)){ #4 strata
  
    sum_survey_results[[i]] <- Reduce("+",survey_results[[i]])/length(survey_results[[i]])

    m<-survey_results[[i]] #pull out list for given strata
    sd_mat <- matrix(apply(sapply(1:length(survey_results[[1]][[1]]), 
                    function(x) unlist(m)[seq(x, length(unlist(m)),
                    length(survey_results[[1]][[1]]) )]), 2, sd), 
                    ncol = length(survey_results[[1]][[1]][1,]))
    
    #the sd_mat above is mostly 0 since things dont change. just pull out sample columns 8 and 9 and append them to previous
    sum_survey_results[[i]] <- cbind(sum_survey_results[[i]],sd_mat[,8:9])
    
    colnames(sum_survey_results[[i]]) <- c("Matrix index","x","y","strata","day","tow","year","spp1","spp2","week","sd_spp1","sd_spp2")
}



### SECOND DO IT FOR THE POPULATION RESULTS

#procedure for biomat (sum of population) and recmat (recruitment)
#1) go through each iteration and put individual results in their own list 
#2) use Reduce to add all items in each list together, then by total number in list to obtain mean
#3) use info from https://stackoverflow.com/questions/39351013/standard-deviation-over-a-list-of-matrices-in-r to calculate standard deviation

pop_sum_s1_biomat <- list()
pop_sum_s1_recmat <- list()
pop_sum_s2_biomat <- list()
pop_sum_s2_recmat <- list()


for(i in seq(length(result))){
  
  pop_sum_s1_biomat[[i]] <- result[[i]][["pop_summary"]][["spp1"]][["Bio.mat"]] 
  pop_sum_s1_recmat[[i]] <- result[[i]][["pop_summary"]][["spp1"]][["Rec.mat"]] 
  pop_sum_s2_biomat[[i]] <- result[[i]][["pop_summary"]][["spp2"]][["Bio.mat"]]
  pop_sum_s2_recmat[[i]] <- result[[i]][["pop_summary"]][["spp2"]][["Rec.mat"]]
  
}

sum_pop_sum_s1_biomat <- list()
sum_pop_sum_s1_recmat <- list()
sum_pop_sum_s2_biomat <- list()
sum_pop_sum_s2_recmat <- list()

#biomat matrices of form [year,day]
#recmat are vector of form [recruitment in year]
sum_pop_sum_s1_biomat <- Reduce("+", pop_sum_s1_biomat)/length(pop_sum_s1_biomat)
sum_pop_sum_s1_recmat <- Reduce("+", pop_sum_s1_recmat)/length(pop_sum_s1_recmat)
sum_pop_sum_s2_biomat <- Reduce("+", pop_sum_s2_biomat)/length(pop_sum_s2_biomat) 
sum_pop_sum_s2_recmat <- Reduce("+", pop_sum_s2_recmat)/length(pop_sum_s2_recmat)



#procedure for spatial population output
#1) fix week in outer loop
#2) go through entire result list putting each corresponding week in list
#4) summarize that list as PopBios
#5) go on to next week

spat_pop_spp1 <- list()
spat_pop_spp2 <- list()


for(wk in seq(52)){ #fix week
  
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
  
}



##################################################################
# PUT EVERYTHING INTO OBJECT CALLED res TO MATCH NORMAL OUTPUT SO PLOTTING FUNCTIONS MORE EASILY USED
##################################################################


#build spp1 and spp2 in plot summary
spp1 <- list()
spp2 <- list()
spp1[["Bio.mat"]] <-sum_pop_sum_s1_biomat
spp1[["Catch.mat"]] <- sum_pop_sum_s1_recmat
spp2[["Bio.mat"]] <-sum_pop_sum_s2_biomat
spp2[["Catch.mat"]] <- sum_pop_sum_s2_recmat

#put spp1 and spp2 into pop_summary
pop_summary <- list()
pop_summary[["spp1"]] <- spp1
pop_summary[["spp2"]] <- spp2



pop_bios <- list()
for(i in seq(52)){
  temp <-list()
  temp[["spp1"]] <- spat_pop_spp1[[i]]
  temp[["spp2"]] <- spat_pop_spp2[[i]]
  
  pop_bios[[i]] <- temp
  
}



#put everything into list res
res <- list()

res[["pop_summary"]] <- pop_summary
res[["pop_bios"]] <- pop_bios
