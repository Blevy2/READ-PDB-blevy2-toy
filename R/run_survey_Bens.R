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
  print(idx)
}




#add a column to each strata_surv that will contain the month to sample from

  S1_wks <- c(13,37)  #strata1 sample weeks- 1st week in each season
  S2_wks <- c(13,37)  #strata2 sample weeks- 1st week in each season
  S3_wks <- c(14,38)  #strata3 sample weeks- 2nd week in each season
  S4_wks <- c(14,38)  #strata4 sample weeks- 2nd week in each season
  
  S1_seq <- rep(c(rep(S1_wks[1],sample_per_sn),rep(S1_wks[2],sample_per_sn)),nyears)
  S2_seq <- rep(c(rep(S2_wks[1],sample_per_sn),rep(S2_wks[2],sample_per_sn)),nyears) #create sequence that will fit in matrix based on above input
  S3_seq <- rep(c(rep(S3_wks[1],sample_per_sn),rep(S3_wks[2],sample_per_sn)),nyears)
  S4_seq <- rep(c(rep(S4_wks[1],sample_per_sn),rep(S4_wks[2],sample_per_sn)),nyears)
  
  strata_surv[[1]]<-cbind(strata_surv[[1]],S1_seq)
  strata_surv[[2]]<-cbind(strata_surv[[1]],S2_seq) #add the above sequence as new column in sample matrix
  strata_surv[[3]]<-cbind(strata_surv[[1]],S3_seq)
  strata_surv[[4]]<-cbind(strata_surv[[1]],S4_seq)






#then can just run through list and use indices in matrix to populate

# procedure:
  # pull out given strata
  # run down each strata_surv matrix created above and sample using information in the given matrix

for(s in seq(nstrata)){  #pull out strata
  
  for(i in seq(length(strata_surv[[s]][,1]))){ #go down list
    
    x <- strata_surv[[s]][i,2]   #x coord in second column
    y <- strata_surv[[s]][i,3]   #y coord in third column  
    year <- strata_surv[[s]][i,7] #year in 7th column
    month <- strata_surv[[s]][i,10]  #appended sample month into 10th column above

    
    strata_surv[[s]][i,8] <- POPULATIONMATRIX$spp1(month+(52*(year-1))[x,y]   #spp1 in col 8
    strata_surv[[s]][i,9] <- POPULATIONMATRIX$spp2(month+(52*(year-1))[x,y]   #spp2 in col 9
    
  }
  
}




  
  
  
  
  
  
  
# procedure:
# pull out strata survey matrix (created above)
# populate each strata survey values based on strat_surv_wks samp_per_sn
# first week of each season, sample stratas 1 and 2
# second week of each season, sample starats 3 and 4
# use info in surv_random$log.mat to paramaterize sample 
# 

# 1st 2 weeks of April (13&14) and 1st 2 weeks of October (wk 37&38)

#set survey weeks for each strata
strat_surv_wks <- list()
strat_surv_wks[[1]] <- c(13,37)  #strata 1
strat_surv_wks[[2]] <- c(13,37)  #strata 1
strat_surv_wks[[3]] <- c(14,38)  #strata 1
strat_surv_wks[[4]] <- c(14,38)  #strata 1


for(s in seq(nstrata)){
  for(j in strat_surv_wks[[s]]){
    for(i in seq(sample_per_sn)){
  
      strata_surv[[s]][i,8] <- POPULATIONMATRIX() strat_surv_wks[[s]]      #spp1 in col 8
      strata_surv[[s]][i,9] <-     #spp2 in col 9
  }
}
}


for(w in sample_wks){
  
  
  if(w==sample_wks[1] | w==sample_wks[3]){
    
  }
  
  
  
   # For each strata
  for(s in seq_len(strata)) {
  
 

    for(j in seq(sample_per_sn))
    
        strata_surv[s][j,"spp1"] <- 
    
    log.mat[log.mat[,"day"]==doy & log.mat[,"year"]==y,paste0("spp",s)][i]  <-  	B[[s]][x_loc[i],y_loc[i]] * as.numeric(survey[["survey_settings"]][[paste0("Qs.spp",s)]])
  }
  surv_random["log.mat"] <- res$pop_bios$sppNUMBER$WEEKNUMBER
  
  
  
  
  
  
  
}








if(sim_init[["brk.idx"]][["day.breaks"]][t] %in% survey[["log.mat"]][,"day"] & !is.null(survey)) {

##print("undertaking scientific survey")

# doy and y
  doy <- sim_init[["brk.idx"]][["day.breaks"]][t]
  y   <- sim_init[["brk.idx"]][["year.breaks"]][t]

	# survey log
	log.mat <- survey[["log.mat"]]

	# survey locations
	x_loc <- log.mat[log.mat[,"day"] == doy & log.mat[,"year"] == y, "x"]
	y_loc <- log.mat[log.mat[,"day"] == doy & log.mat[,"year"] == y, "y"]

	# For each set of locations
	for(i in seq_len(length(x_loc))) {

# For each species
for(s in seq_len(n_spp)) {
log.mat[log.mat[,"day"]==doy & log.mat[,"year"]==y,paste0("spp",s)][i]  <-  	B[[s]][x_loc[i],y_loc[i]] * as.numeric(survey[["survey_settings"]][[paste0("Qs.spp",s)]])
}
	}

	# return log.mat to the list
	survey[["log.mat"]] <- log.mat

}


} # end if statement