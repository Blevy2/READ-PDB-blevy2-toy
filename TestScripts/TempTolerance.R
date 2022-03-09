# yellowtail = 172909
# cod = 164712
# haddock = 164744 
 
fishIDX <- 164744 
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
#2009 and beyond: fall=755 spring=770 so 15 more in spring
#all tows: fall=3661 spring=3284 so 377 more in FALL
rand_num <- sample(755,15)
for(i in rand_num){
  fall_tows <- rbind(fall_tows,fall_tows[i,])
}

#for haddock,
#fall=865 spring=893 so 28 more in spring
rand_num <- sample(865,28)
for(i in rand_num){
 fall_tows <- rbind(fall_tows,fall_tows[i,])
}

#combine fall and spring into all_tows
all_tows <- rbind(fall_tows,spr_tows)
mean(all_tows$BOT_TEMP)

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




#######################################################
#use function to determine which distribution is best
library(gamlss)
library(gamlss.dist)
library(gamlss.add)

fit <- fitDist(all_tows$BOT_TEMP, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)

summary(fit)


#testing for bimodal data (yellowtail has 2 peaks)
#NOT WORKING. CANT DOWLOAD MCLUST PACKAGE WHICH APPEARS TO BE A DEPENDENCY 
multimode::modetest(all_tows$BOT_TEMP)
#######################################################




#######################################################
# making my own distribution based on experimental info
#######################################################

#HADDOCK

#Ess. Fish Hab Table 2: Haddock Occur 0-13 °C but 
                        #most abundant at 2-9, 
                        #and prefer 4-7. Mortality <1 and avoid >10
#see real species modeling notes for calculation, results in mean 5.5, SD 2.5

#with ggplot
library(ggplot2)
ggplot(data=all_tows,aes(BOT_TEMP)) +
  geom_histogram(aes(x = BOT_TEMP, ..density..)) +
  stat_function(fun = dnorm, args = list(mean = 5.5, sd = 2.5),colour="red")


#using norm_fun from mixfishsim
plot(norm_fun(x = seq(0,25,.1), mu = 5.5, va = 2.5)/max(norm_fun(0:25, 5.5, 2.5)),
     type = "l", xlab = "Temperature", ylab = "Tolerance", lwd = 2,xaxt="n")
axis(side = 1, at = seq(length(seq(0,25,.1))),labels = seq(0,25,.1))

norm_fun(0,6,3)






#YELLOWTAIL FLOUNDER

#1- scotian shelf temp document: preference of 2-6, so we can assume a mean of 4
                            # 78% of catches less than 7 degrees.
#using z score formula we can calculate mean 4, SD 3.896
#see real species modeling notes for calculation


#with ggplot
library(ggplot2)
ggplot(data=all_tows,aes(BOT_TEMP)) +
  geom_histogram(aes(x = BOT_TEMP, ..density..)) +
  stat_function(fun = dnorm, args = list(mean = 4, sd = 3.896),colour="red")


#using norm_fun from mixfishsim
plot(norm_fun(x = seq(-4,25,.1), mu = 4, va = 3.896)/max(norm_fun(-4:25, 4, 3.896)),
     type = "l", xlab = "Temperature", ylab = "Tolerance", lwd = 2,xaxt="n")
axis(side = 1, at = seq(length(seq(-4,25,.1))),labels = seq(-4,25,.1))




#2- this document :https://buzzardsbay.org/wp-content/uploads/2017/03/yellowtail-flounder.pdf
#using z score formula we can calculate mean 8.5, SD 3.08
#see real species modeling notes for calculation


#with ggplot
library(ggplot2)
ggplot(data=all_tows,aes(BOT_TEMP)) +
  geom_histogram(aes(x = BOT_TEMP, ..density..)) +
  stat_function(fun = dnorm, args = list(mean = 8.5, sd = 3.08),colour="red")


#using norm_fun from mixfishsim
plot(norm_fun(x = seq(-4,25,.1), mu = 8.5, va = 3.08)/max(norm_fun(-4:25, 8.5, 3.08)),
     type = "l", xlab = "Temperature", ylab = "Tolerance", lwd = 2,xaxt="n")
axis(side = 1, at = seq(length(seq(-4,25,.1))),labels = seq(-4,25,.1))




#COD

#1- studies claim cod inhabit temps from -1.5 to 19.
#based on above try mean 8.75, SD
#using z score formula we can calculate mean 8.75, SD 3.33
#see real species modeling notes for calculation


#with ggplot
library(ggplot2)
ggplot(data=all_tows,aes(BOT_TEMP)) +
  geom_histogram(aes(x = BOT_TEMP, ..density..)) +
  stat_function(fun = dnorm, args = list(mean = 8.75, sd = 3.3),colour="red")


#using norm_fun from mixfishsim
plot(norm_fun(x = seq(-4,25,.1), mu = 8.75, va = 3.33)/max(norm_fun(-4:25, 8.75, 3.33)),
     type = "l", xlab = "Temperature", ylab = "Tolerance", lwd = 2,xaxt="n")
axis(side = 1, at = seq(length(seq(-4,25,.1))),labels = seq(-4,25,.1))




