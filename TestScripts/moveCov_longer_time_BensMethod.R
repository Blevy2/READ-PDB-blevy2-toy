#This script is for creating and testing temperature covariate matrices


library(MixFishSim)



#############################################################################################
#Init Sim
#############################################################################################




#NEW VERSION that has week breaks for entire simulation and allows no fishing
source("R/init_sim_Bens.R")
sim <- init_sim_Bens(nrows = 100, ncols = 100, n_years = 20, n_tows_day = 4,
                n_days_wk_fished = 1, n_fleets = 1, n_vessels = 1, n_species = 2,
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
                           'area2' = c(70,90,80,90)
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

BENS_plot_spatiotemp_hab_justtemp(plot_wk = c(1,10,20,30,40,50),hab = hab, moveCov = moveCov, spwn_wk = list("spp1" = 16:18, "spp2" = 16:19), plot.file =  "testfolder")

dev.off()



###############################################################################################
###############################################################################################
##mean of initial moveCov matrix is 10. seeing how many values are less tahn 10 or more than 10
mean(moveCov$cov.matrix[[1]])

test_moveCov <- matrix(0, nrow = 100, ncol = 100)

#check where the 10s are
test_moveCov <- ifelse(moveCov$cov.matrix[[1]]==10,NA,1)  #puts NA on antidiagonal
#10s exist along the antidiagonal of the matrix


#put 1 for <10 and 99 for >10
test_moveCov <- ifelse(moveCov$cov.matrix[[1]]<10,1,99)

###FINAL TAKEAWAY: NUMBERS ABOVE ANTIDIAGONAL < 10, NUMBERS BELOW > 10, NUMBERS ON IT = 10
###############################################################################################
###############################################################################################




###############################################################################################
###############################################################################################
## I will attempt to create my own temperature gradient that starts from min value in upper right corner
## and progress to max value in upper right corner
###############################################################################################
###############################################################################################

#this function extracts the sub or super diagonal of an entry using the option sub/super to go down/up and offset tell how far to go
#obtained function from https://stackoverflow.com/questions/15672585/looping-through-diagonal1-of-a-matrix

#didnt end up using this function, instead used other info from the above website
# diags <- function(m, type = c("sub", "super"), offset = 1) {
#   type <- match.arg(type)
#   FUN <-
#     if(isTRUE(all.equal(type, "sub")))
#       `+`
#   else
#     `-`
#   m[row(m) == FUN(col(m), offset)] 
# }




B_moveCov <- matrix(0, nrow = 100, ncol = 100)

#number of diagonals a matrix has is ncol + nrow -1 = 2ncol - 1 for square matrix
ndiag <- 199     #2*100 - 1 total diagonals
nabove <- 99     #198/2 = half the diagonals are above the middle one. Use this to start in upper right corner
nbelow <- -99

mintemp<- 8.5  #10 added a little more range to extend past the corners and provide more spatial variation 
maxtemp<- 15.5 #15
tempchng <- maxtemp - mintemp

#create values to go on diagonals
tempscale <- seq(mintemp, maxtemp, tempchng/(ndiag-1) )

  
idx<-1
for(i in seq(nabove,nbelow,-1)){
  
  B_moveCov[row(B_moveCov) == col(B_moveCov) - i] <- tempscale[idx]
  idx <- idx+1
  
}

#see what it looks like
fields::image.plot(t(B_moveCov[nrow(B_moveCov):1,]) )  #the inside t part changes the orientation so it is plotted correctly






#############################################################################################
###take single original covariate map and apply sine to create 1-year oscilating pattern
#############################################################################################

  all_moveCov <- list()  #create list to store all 52 sequences

  idx <- 1
  
for(i in  seq(pi/2 + 22*2*pi/51 , 2*pi+pi/2 + 22*2*pi/51, 2*pi/51) ){  #to start in Januarny use seq(pi/2 + 22*2*pi/51 , 2*pi+pi/2 + 22*2*pi/51, 2*pi/51). Old one that starts in August: seq(pi/2,2*pi+pi/2,2*pi/51)
  
 # print(j)
# print(idx)
  
  all_moveCov[[idx]] <- 0.35*(sin(i)+2)*B_moveCov #creates list of size 52  used to be 0.45* and +2
  
    
  idx <- idx + 1
  
}

  
  
  
  


#plot results


pdf(file="testfolder/moveCov_plots.pdf")

#plot each weekly mean
wk_meantemp <- vector()
yr_meantemp <- vector()
wk_mintemp <- vector()
wk_maxtemp <- vector()
for(i in seq(52)){
  wk_meantemp <- c(wk_meantemp,mean(as.vector(as.matrix(all_moveCov[[i]]))))
  
  wk_mintemp <- c(wk_mintemp,min(as.vector(as.matrix(all_moveCov[[i]]))))
  
  wk_maxtemp <- c(wk_maxtemp,max(as.vector(as.matrix(all_moveCov[[i]]))))
  
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

min(wk_maxtemp)
max(wk_maxtemp)

min(wk_mintemp)
max(wk_mintemp)

max(yr_meantemp)-min(yr_meantemp)


dev.off() #close pdf



#plot weekly temp to see what it looks like

source("R/BENS_plot_spatiotemp_hab_justtemp.R")
moveCov[["cov.matrix"]] <- all_moveCov
colrange <- range(all_moveCov) #set colorbar range for plots as range of all possibly values
BENS_plot_spatiotemp_hab_justtemp(plot_wk = seq(1,49,4),hab = hab, moveCov = moveCov,
                                  spwn_wk = list("spp1" = 16:18, "spp2" = 16:19),
                                  plot.file = "testfolder", colrange = colrange)

dev.off()




#############################################################################################
###manipulate single yearly temp gradient for re-use  VERSION #3 (others in original file)
#############################################################################################
#############################################################################################
###this version tries to lift up the first copy the correct amount and then copy away the rest
#############################################################################################
Good_moveCov <- all_moveCov  #to be used below


#take initial yearly run and extend it so that temp growth over time is 5 degrees


#moveCov <- list()
steps <- 52*20 #total months
inc <- 1.019 #percent increase each year
new_moveCov <- list()


for(i in seq(steps)){
  


    #week 1 is 1st week january
    #temp decreases until week 5 (increases again starting week 6)
    #temp increases through week 30 (decreases again starting week 31)
    
    #run for 5 weeks, start stretching from week 6 through week 30 then raise up remainin points by difference between stretchd week 30 and original week 30
    
    
    #decrease
    if(i <= 5){
      new_moveCov[["cov.matrix"]][[i]] <- Good_moveCov[[i]]
      
    }
    
    #increase/stretch
    if( (i >= 6)  & (i <= 30 ) ){
      new_moveCov[["cov.matrix"]][[i]] <- inc*Good_moveCov[[i]]
        }
    

  
  
  #after creating first stretched sequence, see how far first and last mean values are
  if(i==31){

    #rasie up by this amount
    diff <- mean(as.vector(as.matrix(new_moveCov[["cov.matrix"]][[30]])))-mean(as.vector(as.matrix(Good_moveCov[[31]])))
       
  }
  
  #finish off the first year
  
  if((i >=31) & (i<=52)){
    
    new_moveCov[["cov.matrix"]][[i]] <- diff+Good_moveCov[[i]]
    
  }
  
  
  
  #after creating first stretched sequence, use previous one as basis for next one
  if(i>52){

    new_moveCov[["cov.matrix"]][[i]] <- diff+new_moveCov[["cov.matrix"]][[i-52]]
    
    
  }
  
}



#pass to moveCov to be used elsewhere
moveCov[["cov.matrix"]] <- new_moveCov[["cov.matrix"]]








#to plot just the temp preferences over time
source("R/BENS_plot_spatiotemp_hab_justtemp.R")
colrange <- range(moveCov[["cov.matrix"]] ) #set colorbar range for plots as range of all possibly values

BENS_plot_spatiotemp_hab_justtemp(plot_wk = seq(1,1040,52),
                                  hab = hab, 
                                  moveCov = moveCov, 
                                  spwn_wk = list("spp1" = 16:18, "spp2" = 16:19), 
                                  plot.file = "testfolder", colrange = colrange)

dev.off()


#plot spatiotemperal distribution (combine temp and spatial preferences)
source("R/BENS_plot_spatiotemp_hab.R")
colrange = range(moveCov[["cov.matrix"]])
plot_spatiotemp_hab(plot_monthly = TRUE, plot_wk = seq(1,1040,50),
                    hab = hab, moveCov = moveCov,
                    spwn_wk = list("spp1" = 16:18, "spp2" = 16:19), 
                    plot.file = "testfolder", colrange = colrange)




#############################################################################################
##PLots
#############################################################################################

#plot mean in covariate values 



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
  if(i %% 52 == 0){    
    p <- i - 51
    yr_meantemp <- c(yr_meantemp,mean(as.vector(wk_meantemp[p:i])))
  }
  
}
plot(wk_meantemp)
plot(wk_meantemp[1:60])
plot(wk_mintemp)
plot(wk_maxtemp)
plot(yr_meantemp)


min(yr_meantemp)
max(yr_meantemp)

max(yr_meantemp)-min(yr_meantemp)



##################################################################
# PLOTTING THE ABOVE BY QUADRANT
##################################################################

#first break into strata
Strata1 <- list()  #matrix(0,nrow=50,ncol=100)
Strata2 <- list() #matrix(0,nrow=50,ncol=100)
Strata3 <- list()  #matrix(0,nrow=50,ncol=100)
Strata4 <- list()

for(i in seq(52*20)){
  
#Strata1
Strata1[[i]]<-moveCov$cov.matrix[[i]][1:50,1:50]
#Strata2
Strata2[[i]]<-moveCov$cov.matrix[[i]][1:50,51:100]
#Strata3
Strata3[[i]]<-moveCov$cov.matrix[[i]][51:100,1:50]
#Strata4
Strata4[[i]]<-moveCov$cov.matrix[[i]][51:100,51:100]

}

#copied above for each strata
wk_meantemps1 <- vector()
yr_meantemps1 <- vector()
wk_mintemps1 <- vector()
wk_maxtemps1 <- vector()

wk_meantemps2 <- vector()
yr_meantemps2 <- vector()
wk_mintemps2 <- vector()
wk_maxtemps2 <- vector()

wk_meantemps3 <- vector()
yr_meantemps3 <- vector()
wk_mintemps3 <- vector()
wk_maxtemps3 <- vector()

wk_meantemps4 <- vector()
yr_meantemps4 <- vector()
wk_mintemps4 <- vector()
wk_maxtemps4 <- vector()

for(i in seq(steps)){
  wk_meantemps1 <- c(wk_meantemps1,mean(as.vector(as.matrix(Strata1[[i]]))))
  wk_mintemps1 <- c(wk_mintemps1,min(as.vector(as.matrix(Strata1[[i]]))))
  wk_maxtemps1 <- c(wk_maxtemps1,max(as.vector(as.matrix(Strata1[[i]]))))
  
  wk_meantemps2 <- c(wk_meantemps2,mean(as.vector(as.matrix(Strata2[[i]]))))
  wk_mintemps2 <- c(wk_mintemps2,min(as.vector(as.matrix(Strata2[[i]]))))
  wk_maxtemps2 <- c(wk_maxtemps2,max(as.vector(as.matrix(Strata2[[i]]))))
  
  wk_meantemps3 <- c(wk_meantemps3,mean(as.vector(as.matrix(Strata3[[i]]))))
  wk_mintemps3 <- c(wk_mintemps3,min(as.vector(as.matrix(Strata3[[i]]))))
  wk_maxtemps3 <- c(wk_maxtemps3,max(as.vector(as.matrix(Strata3[[i]]))))
  
  wk_meantemps4 <- c(wk_meantemps4,mean(as.vector(as.matrix(Strata4[[i]]))))
  wk_mintemps4 <- c(wk_mintemps4,min(as.vector(as.matrix(Strata4[[i]]))))
  wk_maxtemps4 <- c(wk_maxtemps4,max(as.vector(as.matrix(Strata4[[i]]))))
  
  #record yearly temp
  if(i %% 52 == 0){    
    p <- i - 51
    yr_meantemps1 <- c(yr_meantemps1,mean(as.vector(wk_meantemps1[p:i])))
    yr_meantemps2 <- c(yr_meantemps2,mean(as.vector(wk_meantemps2[p:i])))
    yr_meantemps3 <- c(yr_meantemps3,mean(as.vector(wk_meantemps3[p:i])))
    yr_meantemps4 <- c(yr_meantemps4,mean(as.vector(wk_meantemps4[p:i])))
  }
  
}

par(mfrow = c(2,2),mar = c(4, 4, 4, 4))
plot(wk_meantemps1)
plot(wk_mintemps1)
plot(wk_maxtemps1)
plot(yr_meantemps1)
mtext("Strata 1- NW", side = 3, line = -1, outer = TRUE)

par(mfrow = c(2,2),mar = c(4,4,4,4))
plot(wk_meantemps2)
plot(wk_mintemps2)
plot(wk_maxtemps2)
plot(yr_meantemps2)
mtext("Strata 2- NE", side = 3, line = -1, outer = TRUE)

par(mfrow = c(2,2),mar = c(4,4,4,4))
plot(wk_meantemps3)
plot(wk_mintemps3)
plot(wk_maxtemps3)
plot(yr_meantemps3)
mtext("Strata 3- SW", side = 3, line = -1, outer = TRUE)

par(mfrow = c(2,2),mar = c(4,4,4,4))
plot(wk_meantemps4)
plot(wk_mintemps4)
plot(wk_maxtemps4)
plot(yr_meantemps4)
mtext("Strata 4- SE", side = 3, line = -1, outer = TRUE)




