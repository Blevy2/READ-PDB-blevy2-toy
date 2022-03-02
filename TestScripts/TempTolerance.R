
# haddock = 164744 
 
fishIDX <- 164744 
#read in point data and convert to ppp
gis.name <- paste("C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\Plot_survey\\ADIOS_SV_",fishIDX,"_GBK_NONE_survey_dist_map_fixed.csv",sep="")

gis=as.data.frame( read.csv(file= gis.name, header=T) )
gis$CatchWt <- gis$CATCH_WT_CAL
gis$CatchWt[is.na(gis$CatchWt)] <- 0
gis$Year <- gis$YEAR
gis$Longitude <- gis$LONGITUDE
gis$Latitude <- gis$LATITUDE

#replace NA with 0
gis$CATCH_WT_CAL[is.na(gis$CATCH_WT_CAL)] <- 0

#spring.gis <- gis[gis$SEASON=='SPRING',]
#fall.gis   <- gis[gis$SEASON=='FALL',]


#ONLY TAKE MORE RECENT ONES?
#spring.gis <- spring.gis[spring.gis$Year>2009,]
gis <- gis[gis$Year>=2009,]


#create histograms
all_hist <- hist(gis$BOT_TEMP)
spr_hist <- hist(gis$BOT_TEMP[gis$SEASON=="SPRING"])
fall_hist <- hist(gis$BOT_TEMP[gis$SEASON=="FALL"])
  
plot(all_hist)
plot(spr_hist)
plot(fall_hist)


#fit normal distribution to histogram
Tparams <- MASS::fitdistr(gis$BOT_TEMP[!is.na(gis$BOT_TEMP)],"normal")

plot(all_hist)

plot(MixFishSim::norm_fun(x = 0:25, mu = Tparams$estimate[[1]], va = Tparams$estimate[[2]])/max(norm_fun(0:25,  Tparams$estimate[[1]],  Tparams$estimate[[2]])),
     type = "l", xlab = "Temperature", ylab = "Tolerance", lwd = 2, add=TRUE)



#SHOULD WE LOG THE DATA FIRST TO MAKE IT LOOK MORE NORMAL?
all_hist <- hist(log(gis$BOT_TEMP))

Tparams <- MASS::fitdistr(log(gis$BOT_TEMP[!is.na(gis$BOT_TEMP)]),"normal")

plot(MixFishSim::norm_fun(x = 0:25, mu = exp(Tparams$estimate[[1]]), va = exp(Tparams$estimate[[2]]))/max(norm_fun(0:25,  exp(Tparams$estimate[[1]]),  exp(Tparams$estimate[[2]]))),
     type = "l", xlab = "Temperature", ylab = "Tolerance", lwd = 2, add=TRUE)

