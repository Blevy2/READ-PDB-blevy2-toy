# yellowtail = 172909
# cod = 164712
# haddock = 164744 
 
fishIDX <- 172909 
#read in point data and convert to ppp
gis.name <- paste("C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\Plot_survey\\ADIOS_SV_",fishIDX,"_GBK_NONE_survey_dist_map_fixed.csv",sep="")

gis=as.data.frame( read.csv(file= gis.name, header=T) )
gis$CatchWt <- gis$CATCH_WT_CAL
gis$Year <- gis$YEAR
gis$Longitude <- gis$LONGITUDE
gis$Latitude <- gis$LATITUDE


#spring.gis <- gis[gis$SEASON=='SPRING',]
#fall.gis   <- gis[gis$SEASON=='FALL',]


#ONLY TAKE MORE RECENT ONES?
#spring.gis <- spring.gis[spring.gis$Year>2009,]
gis <- gis[gis$Year>=2009,]

#remove NAs and split by season (view to see their relative size)
all_tows <- gis[!is.na(gis$BOT_TEMP),]
spr_tows <- gis[(gis$SEASON=="SPRING") & !is.na(gis$BOT_TEMP),]
fall_tows <- gis[(gis$SEASON=="FALL") & !is.na(gis$BOT_TEMP),]

#figure out which has more tows and sample smaller one to even them out

#for yellowtail
#fall=551 spring=553 so 2 more in spring
rand_num <- sample(551,2)
for(i in rand_num){
  fall_tows <- rbind(fall_tows,fall_tows[i,])
}

#for cod,
#fall=755 spring=770 so 15 more in spring
# rand_num <- sample(755,15)
# for(i in rand_num){
#   fall_tows <- rbind(fall_tows,fall_tows[i,])
# }

#for haddock,
#fall=865 spring=893 so 28 more in spring
#rand_num <- sample(865,28)
#for(i in rand_num){
#  fall_tows <- rbind(fall_tows,fall_tows[i,])
#}

#combine fall and spring into all_tows
all_tows <- rbind(fall_tows,spr_tows)

#create histograms
all_hist <- hist(all_tows$BOT_TEMP)
spr_hist <- hist(spr_tows$BOT_TEMP)
fall_hist <- hist(fall_tows$BOT_TEMP)
  



#fit normal distribution to histogram
Tparams <- MASS::fitdistr(all_tows$BOT_TEMP,"normal")

#with ggplot
library(ggplot2)
d <- ggplot(data=all_tows,aes(BOT_TEMP)) +
  geom_histogram(aes(x = BOT_TEMP, ..density..)) +
  stat_function(fun = dnorm, args = list(mean = Tparams$estimate[[1]], sd = Tparams$estimate[[2]]),colour="red")

d
#plot(all_hist)
# 
# 
# 
# #SHOULD WE LOG THE DATA FIRST TO MAKE IT LOOK MORE NORMAL?
# all_hist <- hist(log(!is.na(gis$BOT_TEMP)))
# 
# Tparams <- MASS::fitdistr(log(gis$BOT_TEMP[!is.na(gis$BOT_TEMP)]),"normal")
# 
# plot(MixFishSim::norm_fun(x = 0:25, mu = exp(Tparams$estimate[[1]]), va = exp(Tparams$estimate[[2]]))/max(norm_fun(0:25,  exp(Tparams$estimate[[1]]),  exp(Tparams$estimate[[2]]))),
#      type = "l", xlab = "Temperature", ylab = "Tolerance", lwd = 2, add=TRUE)



#trying lognormal
Tparams <- MASS::fitdistr(all_tows$BOT_TEMP,"log-normal")

#with ggplot
library(ggplot2)
d <- ggplot(data=all_tows,aes(BOT_TEMP)) +
  geom_histogram(aes(x = BOT_TEMP, ..density..)) +
  stat_function(fun = dlnorm, args = list(meanlog = Tparams$estimate[[1]], sdlog = Tparams$estimate[[2]]),colour="red")
  
d


#use function to determine which distribution is best
library(gamlss)
library(gamlss.dist)
library(gamlss.add)

fit <- fitDist(all_tows$BOT_TEMP, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)

summary(fit)


#testing for bimodal data (yellowtail has 2 peaks)
#NOT WORKING. CANT DOWLOAD MCLUST PACKAGE WHICH APPEARS TO BE A DEPENDENCY 
multimode::modetest(all_tows$BOT_TEMP)
