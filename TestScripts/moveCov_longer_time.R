#This script is for creating and testing temperature covariate matrices


library(MixFishSim)



#############################################################################################
#Init Sim
#############################################################################################

#OLD VERSION
sim <- init_sim(nrows = 100, ncols = 100, n_years = 20, n_tows_day = 4,
                n_days_wk_fished = 5, n_fleets = 2, n_vessels = 20, n_species = 2,
                move_freq = 1)




#NEW VERSION that has week breaks for entire simulation
source("R/init_sim_Bens.R")
sim <- init_sim_Bens(nrows = 100, ncols = 100, n_years = 20, n_tows_day = 4,
                n_days_wk_fished = 5, n_fleets = 2, n_vessels = 20, n_species = 2,
                move_freq = 1)




#############################################################################################
###Habitat setup
#############################################################################################

source("R/BENS_create_hab.R")

source("R/BENS_plot_habitat.R")

#values settled on from anisotropy and habtest scripts

spp.ctrl = list(
  "spp.1" = list('nu' = 1/0.05, 
                 'var' = 1,
                 'scale' = 2, 
                 'Aniso' = matrix(nc = 2, c(1, -2, 1, 2))),
  "spp.2" = list('nu' = 1/0.015, 
                 'var'  = 1,
                 'scale' = 40, 
                 'Aniso' = matrix(nc = 2, c(1.5, -3, 3, 4))),
  plot.dist = TRUE, 
  plot.file = "testfolder"
)


hab <- BENS_create_hab(sim_init = sim, 
                       spp.ctrl = spp.ctrl,
                       
                       spawn_areas = list(
                         "spp1" = list(
                           'area1' = c(40,50,40,50),   #of the form  c(x1, x2, y1, y2) THESE ARE BOUNDARIES OF MATRIX VALUES
                           'area2' = c(80,90,60,70)   #need to revisit closure areas
                         ),
                         "spp2" = list(
                           'area1' = c(50,60,30,40),
                           'area2' = c(80,90,90,90)
                         ),
                         spwn_mult = 10, 
                         plot.dist = TRUE, 
                         plot.file = "testfolder" ),
                       
                       
                       #created this new part defining strata
                       
                       strata = list(                              #Form: upper left, upper right, lower left, lower right
                         "strata1" = c(1,sim$idx[["nrows"]]/2,1,sim$idx[["ncols"]]/2),   #of the form  c(x1, x2, y1, y2) THESE ARE BOUNDARIES OF STRATA VALUES
                         
                         "strata2" = c(1,sim$idx[["nrows"]]/2,sim$idx[["ncols"]]/2+1,sim$idx[["ncols"]]),
                         
                         "strata3" = c(sim$idx[["nrows"]]/2 + 1,sim$idx[["nrows"]], 1 ,sim$idx[["ncols"]]/2),
                         
                         "strata4" = c(sim$idx[["nrows"]]/2 + 1,sim$idx[["nrows"]],sim$idx[["ncols"]]/2 +1 , sim$idx[["ncols"]])
                       )
)


#old version
plot_habitat(hab$hab)

## Plot the adjusted habitat fields
plot_habitat(hab$spwn_hab)




plot_spatiotemp_hab(hab = hab, moveCov = moveCov, spwn_wk = list("spp1" = 16:18, "spp2" = 16:19), plot.file =  "testfolder")



#to plot just the temp preferences over time
source("R/BENS_plot_spatiotemp_hab_justtemp.R")

BENS_plot_spatiotemp_hab_justtemp(plot_wk = seq(1,20*52,20),hab = hab, moveCov = moveCov, spwn_wk = list("spp1" = 16:18, "spp2" = 16:19), plot.file =  "testfolder")





#############################################################################################
###Init moveCov

#############################################################################################

#first try
source("R/init_moveCov_Bens.R")

steps <- 52*20 #must be multiple of 52
moveCov <- init_moveCov_Bens(sim_init = sim, steps = steps,
                        spp_tol = list("spp1" = list("mu" = 12, "va" = 8),
                                       "spp2" = list("mu" = 15, "va" = 7)
                                      )
                            )



#second try
source("R/init_moveCov_Bens2.R")

steps <- 52*20 #must be multiple of 52
moveCov <- init_moveCov_Bens2(sim_init = sim, steps = steps,
                             spp_tol = list("spp1" = list("mu" = 12, "va" = 8),
                                            "spp2" = list("mu" = 15, "va" = 7)
                             )
)




#third try
source("R/init_moveCov_Bens3.R")

steps <- 52*20 #must be multiple of 52
moveCov <- init_moveCov_Bens3(sim_init = sim, steps = steps,
                              spp_tol = list("spp1" = list("mu" = 12, "va" = 8),
                                             "spp2" = list("mu" = 15, "va" = 7)
                              )
)





#############################################################################################
###take each single original covariate map and apply sine to create 52 different 1-year oscilating pattern
#############################################################################################

  all_moveCov <- list()  #create list to store all 52 sequences

for(j in seq(length(moveCov[[1]]))){

  #assign(paste0('moveCov',j),list())  #create list using index j
  
  idx <- 1
  
  temp <- list()

for(i in seq(pi/2,2*pi+pi/2,2*pi/51)){
  
 # print(j)
# print(idx)
  
  temp[[idx]] <- 0.45*(sin(i)+2)*moveCov[[1]][[j]] #creates list of size 52
  
    
  idx <- idx + 1
  
}

  all_moveCov[[j]] <- temp   #stores previous list as list
  
}


#plot results


pdf(file="testfolder/moveCov_plots.pdf")

for(j in seq(52)){
#plot each weekly mean
wk_meantemp <- vector()
yr_meantemp <- vector()
wk_mintemp <- vector()
wk_maxtemp <- vector()
for(i in seq(52)){
  wk_meantemp <- c(wk_meantemp,mean(as.vector(as.matrix(all_moveCov[[j]][[i]]))))
  
  wk_mintemp <- c(wk_mintemp,min(as.vector(as.matrix(all_moveCov[[j]][[i]]))))
  
  wk_maxtemp <- c(wk_maxtemp,max(as.vector(as.matrix(all_moveCov[[j]][[i]]))))
  
  #record yearly temp
  if(i %% 51 == 0){    
    p <- i - 50
    yr_meantemp <- c(yr_meantemp,mean(as.vector(wk_meantemp[p:i])))
  }
  
}
  
  plot(wk_meantemp)

plot(wk_mintemp)
plot(wk_maxtemp)
#plot(yr_meantemp)


min(yr_meantemp)
max(yr_meantemp)

max(yr_meantemp)-min(yr_meantemp)
}

dev.off() #close pdf




#############################################################################################
###manipulate single temp for re-use VERSION #1
#############################################################################################
#############################################################################################
###this version creates oscillating effect that widens as time goes on (due to scaling)
#############################################################################################

#take initial yearly run and extend it so that temp growth over time is 5 degrees
new_moveCov <- list()

moveCov <- list()
steps <- 52*20 #total months
inc <- 1.02 #percent increase each year

for(i in seq(steps)){
  
  #create first stretched sequence
  if(i<=52){
  
  #first half the year keep the same while they are decreasing
  # second half of the year stretch the values by inc
  
  #decrease
  if((i %% 52 < 27) & (i %% 52 !=0)){
  moveCov[["cov.matrix"]][[i]] <- Good_moveCov[[i %% 52]]
                   }
  
  #increase/stretch
  if((i %% 52 >= 27)  & (i %% 52 !=0)){
    moveCov[["cov.matrix"]][[i]] <- inc*Good_moveCov[[i %% 52]]
  }
  
  #increase/stretch
  if(i %% 52 == 0){
    moveCov[["cov.matrix"]][[i]] <- inc*Good_moveCov[[52]]
  }
  }
  
  #after creating first stretched sequence, use previous one as basis for next one
  if(i>52){
    
    #first half the year keep the same while they are decreasing
    # second half of the year stretch the values by inc
    
    moveCov[["cov.matrix"]][[i]] <- inc*moveCov[["cov.matrix"]][[i-52]]


  }
  
}







#############################################################################################
###manipulate single temp for re-use  VERSION #2
#############################################################################################
#############################################################################################
###this version creates oscilatting effect that remains consistent width, but its not smooth
#############################################################################################

#take initial yearly run and extend it so that temp growth over time is 5 degrees
new_moveCov <- list()

moveCov <- list()
steps <- 52*20 #total months
inc <- 1.025 #percent increase each year

for(i in seq(steps)){
  
  #create first stretched sequence
  if(i<=52){
    
    #first half the year keep the same while they are decreasing
    # second half of the year stretch the values by inc
    
    #decrease
    if((i %% 52 < 27) & (i %% 52 !=0)){
      moveCov[["cov.matrix"]][[i]] <- Good_moveCov[[i %% 52]]
    }
    
    #increase/stretch
    if((i %% 52 >= 27)  & (i %% 52 !=0)){
      moveCov[["cov.matrix"]][[i]] <- inc*Good_moveCov[[i %% 52]]
    }
    
    #increase/stretch
    if(i %% 52 == 0){
      moveCov[["cov.matrix"]][[i]] <- inc*Good_moveCov[[52]]
    }
  }
  
  #after creating first stretched sequence, use previous one as basis for next one
  if(i>52){
    
    #first half the year keep the same while they are decreasing
    # second half of the year stretch the values by inc
    
  moveCov[["cov.matrix"]][[i]] <- .25+moveCov[["cov.matrix"]][[i-52]]
    
    
  }
  
}





#############################################################################################
###manipulate single temp for re-use  VERSION #3
#############################################################################################
#############################################################################################
###this version tries to lift up the first copy the correct amount and then copy away the rest
#############################################################################################

#take initial yearly run and extend it so that temp growth over time is 5 degrees
new_moveCov <- list()

#moveCov <- list()
steps <- 52*20 #total months
inc <- 1.02 #percent increase each year

for(i in seq(steps)){
  
  #create first stretched sequence
  if(i<=52){
    
    #first half the year keep the same while they are decreasing
    # second half of the year stretch the values by inc
    
    #decrease
    if((i %% 52 < 27) & (i %% 52 !=0)){
      moveCov[["cov.matrix"]][[i]] <- Good_moveCov[[i %% 52]]
    }
    
    #increase/stretch
    if((i %% 52 >= 27)  & (i %% 52 !=0)){
      moveCov[["cov.matrix"]][[i]] <- inc*Good_moveCov[[i %% 52]]
    }
    
    #increase/stretch
    if(i %% 52 == 0){
      moveCov[["cov.matrix"]][[i]] <- inc*Good_moveCov[[52]]
    }
  }
  
  #after creating first stretched sequence, see how far first and last mean values are
  if(i==52){

    #rasie up by this amount
    diff <- mean(as.vector(as.matrix(moveCov[["cov.matrix"]][[52]])))-mean(as.vector(as.matrix(moveCov[["cov.matrix"]][[1]])))
       
  }
  
  
  #after creating first stretched sequence, use previous one as basis for next one
  if(i>52){

    moveCov[["cov.matrix"]][[i]] <- diff+moveCov[["cov.matrix"]][[i-52]]
    
    
  }
  
}








#############################################################################################
##PLots
#############################################################################################
#plot mean in covariate values 
nyears <- round(steps/52,0)


#plot each weekly mean
wk_meantemp <- vector()
yr_meantemp <- vector()
wk_mintemp <- vector()
wk_maxtemp <- vector()

for(i in seq(steps)){
  wk_meantemp <- c(wk_meantemp,mean(as.vector(as.matrix(moveCov[["cov.matrix"]][[i]]))))
  
  wk_mintemp <- c(wk_mintemp,min(as.vector(as.matrix(moveCov[["cov.matrix"]][[i]]))))
  
  wk_maxtemp <- c(wk_maxtemp,max(as.vector(as.matrix(moveCov[["cov.matrix"]][[i]]))))
  
  #record yearly temp
  if(i %% 51 == 0){    
    p <- i - 50
    yr_meantemp <- c(yr_meantemp,mean(as.vector(wk_meantemp[p:i])))
  }
  
}
plot(wk_meantemp)
plot(wk_meantemp[1:51])
plot(wk_mintemp)
plot(wk_maxtemp)
plot(yr_meantemp)


min(yr_meantemp)
max(yr_meantemp)

max(yr_meantemp)-min(yr_meantemp)





# FINISH BELOW

#compare mean for given week
nt <- 52  #number of weeks to plot
par(mfrow = c(ceiling(sqrt(nt)), ceiling(nt/ceiling(sqrt(nt)))), mar = c(1, 1, 1, 1))


#seq(1,steps,52) = 1 to steps by size 51
for(i in seq(52)){
  
  meantemp_jan[i] <- c(meantemp,mean(as.matrix(moveCov[["cov.matrix"]][[i]])))
  
}



