#This script is for creating and testing temperature covariate matrices


library(MixFishSim)
library(raster)

#read into GB temp data
WeeklyTempData <- readRDS(file="TestScripts/FVCOM_GB/YearlyTemp_GB.RDS")





#plot results


#pdf(file="testfolder/moveCov_plots.pdf")

#plot each weekly mean
wk_meantemp <- vector()
yr_meantemp <- vector()
wk_mintemp <- vector()
wk_maxtemp <- vector()


for(yr in seq(length(WeeklyTempData))){
for(i in seq(52)){
  wk_meantemp <- c(wk_meantemp,mean(as.vector(as.matrix(WeeklyTempData[[yr]][[i]])),na.rm=T))
  
  wk_mintemp <- c(wk_mintemp,min(as.vector(as.matrix(WeeklyTempData[[yr]][[i]])),na.rm=T))
  
  wk_maxtemp <- c(wk_maxtemp,max(as.vector(as.matrix(WeeklyTempData[[yr]][[i]])),na.rm=T))
  
  #record yearly temp
  if(i %% 52 == 0){    
    p <- i - 51
    yr_meantemp <- c(yr_meantemp,mean(as.vector(wk_meantemp[p:i])))
  }
  
}
}
par(mfrow = c(1,1),mar = c(2.5, 2.5, 2.5, 2.5))
plot(wk_meantemp)

plot(wk_mintemp)
plot(wk_maxtemp)
plot(yr_meantemp)


min(yr_meantemp)
max(yr_meantemp)

min(wk_maxtemp)
max(wk_maxtemp)

min(wk_mintemp)
max(wk_mintemp)

max(yr_meantemp)-min(yr_meantemp)


#dev.off() #close pdf


#PLOT SINGLE YEAR TO CHOOSE ONE TO EXTEND FOR 20 YEARS

#plot each weekly mean


#NUMBER 8 LOOKS BEST
for(yr in c(7,8,9,11)){
  wk_meantemp <- vector()
yr_meantemp <- vector()
wk_mintemp <- vector()
wk_maxtemp <- vector()
  for(i in seq(52)){
    wk_meantemp <- c(wk_meantemp,mean(as.vector(as.matrix(WeeklyTempData[[yr]][[i]])),na.rm=T))
    
    wk_mintemp <- c(wk_mintemp,min(as.vector(as.matrix(WeeklyTempData[[yr]][[i]])),na.rm=T))
    
    wk_maxtemp <- c(wk_maxtemp,max(as.vector(as.matrix(WeeklyTempData[[yr]][[i]])),na.rm=T))
    
    #record yearly temp
    if(i %% 52 == 0){    
      p <- i - 51
      yr_meantemp <- c(yr_meantemp,mean(as.vector(wk_meantemp[p:i])))
    }
    
  }
  par(mfrow = c(1,1),mar = c(1.5, 1.5, 1.5, 1.5))
plot(wk_meantemp)
}


plot(wk_mintemp)
plot(wk_maxtemp)
plot(yr_meantemp)


min(yr_meantemp)
max(yr_meantemp)

min(wk_maxtemp)
max(wk_maxtemp)

min(wk_mintemp)
max(wk_mintemp)

max(yr_meantemp)-min(yr_meantemp)




all_moveCov <- list()

#CHOOSE ONE TO WORK WITH. #8 looked good, which is 2012
choice<-8
for(i in seq(length(WeeklyTempData[[choice]]))){
all_moveCov[["cov.matrix"]][[i]] <- as.matrix(WeeklyTempData[[choice]][[i]])
}



#plot weekly temp to see what it looks like
moveCov <- list()
source("R/BENS_plot_spatiotemp_hab_justtemp.R")
moveCov[["cov.matrix"]] <- all_moveCov
colrange <- c(4,19.5) #set colorbar range for plots as range of all possibly values
BENS_plot_spatiotemp_hab_justtemp(plot_wk = seq(1,52),hab = all_moveCov, moveCov = all_moveCov,
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
inc <- 1.045 #percent increase each year
new_moveCov <- list()


for(i in seq(steps)){
  


    #week 1 is 1st week january
    #temp decreases until week 5 (increases again starting week 6)
    #temp increases through week 30 (decreases again starting week 31)
    
    #run for 5 weeks, start stretching from week 6 through week 30 then raise up remainin points by difference between stretchd week 30 and original week 30
    
    
    #decrease
    if(i <= 5){
      new_moveCov[["cov.matrix"]][[i]] <- Good_moveCov[["cov.matrix"]][[i]]
      
    }
    
    #increase/stretch
    if( (i >= 6)  & (i <= 30 ) ){
      new_moveCov[["cov.matrix"]][[i]] <- inc*Good_moveCov[["cov.matrix"]][[i]]
        }
    

  
  
  #after creating first stretched sequence, see how far first and last mean values are
  if(i==31){

    #rasie up by this amount
    diff <- mean(as.vector(as.matrix(new_moveCov[["cov.matrix"]][[30]])),na.rm=T)-mean(as.vector(as.matrix(Good_moveCov[["cov.matrix"]][[31]])),na.rm=T)
       
  }
  
  #finish off the first year
  
  if((i >=31) & (i<=52)){
    
    new_moveCov[["cov.matrix"]][[i]] <- diff+Good_moveCov[["cov.matrix"]][[i]]
    
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
                                  moveCov = move_Cov, 
                                  spwn_wk = list("spp1" = 15:18, "spp2" = 15:18), 
                                  plot.file = "testfolder", colrange = colrange)

dev.off()


#plot spatiotemperal distribution (combine temp and spatial preferences)
source("R/BENS_plot_spatiotemp_hab.R")
colrange = range(moveCov[["cov.matrix"]])
plot_spatiotemp_hab(plot_monthly = TRUE, plot_wk = seq(1,1040,52),
                    hab = hab, moveCov = moveCov,
                    spwn_wk = list("spp1" = 15:18, "spp2" = 15:18), 
                    plot.file = "testfolder")




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
  wk_meantemp <- c(wk_meantemp,mean(as.vector(as.matrix(moveCov[["cov.matrix"]][[i]])),na.rm=T))
  
  wk_mintemp <- c(wk_mintemp,min(as.vector(as.matrix(moveCov[["cov.matrix"]][[i]])),na.rm=T))
  
  wk_maxtemp <- c(wk_maxtemp,max(as.vector(as.matrix(moveCov[["cov.matrix"]][[i]])),na.rm=T))
  
  #record yearly temp
  if(i %% 52 == 0){    
    p <- i - 51
    yr_meantemp <- c(yr_meantemp,mean(as.vector(wk_meantemp[p:i]),na.rm=T))
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





































#############################################################################################
###manipulate single yearly temp gradient for re-use  VERSION #3 (others in original file)
#############################################################################################
#############################################################################################
###THIS VERSION IS TO SIMPLY COPY THE SAME PATTERN WITH NO TEMP INCREASE
#############################################################################################
Good_moveCov <- all_moveCov  #to be used below


#take initial yearly run and extend it so that temp growth over time is 5 degrees


#moveCov <- list()
steps <- 52*20 #total months
inc <- 1 #percent increase each year (IE, NONE)
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
    diff <- 0
    
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






