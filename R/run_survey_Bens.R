# after simulation takes place, go through and do a random survey


#use first 4 columns of surv_random$log.mat as a basis for this sampling
#
# column 1: station_no- ranges from 1 to rows*col = 10,000 and represents matrix index of given sample
# column 2: x- x-value of matrix location being sampled
# column 3: y- y-value of matrix location being sampled
# column 4: strata- strata number of location being sampled (all with 1 listed first, then 2, then 3, then 4)


# results stored in res$pop_bios$sppNUMBER$WEEKNUMBER


# suppose we sample in 1st 2 weeks of June (21&22) and 1st 2 weeks of October (wk 37&38)

sample_wks <- c(21,22,37,38)

sample_per_sn <- 5   #samples per season. 5 per season makes 10 per year


# go through each sample week and pull out sample_per_sn from each strata. Record in log.mat

for(w in sample_wks){
  
  
  # For each species
  for(s in seq_len(n_spp)) {
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