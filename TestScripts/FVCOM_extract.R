library(readxl)
library(sdm)
library(raster)
library(sf)
library(ncdf4)
library(maptools)
library(rgdal)
library(maps)
library(mapdata)
library(dplyr)
library(viridis)
library(sp)
library(rgeos)
library(writexl)
library(SDMTools)
library(classInt)
library(raster)
library(akima)
library(chron)
library(mgcv)

#This code can be used to extract depth, temp & salinity data for any year up through 2015. FVCOM changed format after that so 2016-2017 requires additional code which you will find below the -2015 code. No publicly available FVCOM data exists for years past 2017 at this time.

Catch_Data=read.csv("C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\Plot_survey\\ADIOS_SV_172909_GBK_NONE_survey_dist_map_fixed.csv") #The survey catch file you are using to build your model


#only pull out the ones you want and set them as the dates to extract
gis=Catch_Data
gis$CatchWt <- gis$CATCH_WT_CAL
gis$CatchWt[is.na(gis$CatchWt)] <- 0
gis$Year <- gis$YEAR
gis$Longitude <- gis$LONGITUDE
gis$Latitude <- gis$LATITUDE

#replace NA with 0
gis$CATCH_WT_CAL[is.na(gis$CATCH_WT_CAL)] <- 0

#after gear change in 2008.
Catch_Data <- gis[(gis$Year>=2009) & (gis$Year<=2015),]  #Also,format changes in 2016-2017 (see chunk below this one for extracting those dates) 

#extract day 
Catch_Data$Day <- substr(Catch_Data$TOWDATE,1,2) #2digit day in locations 1-2 of TOWDATE

#extract month and change into number
month <- substr(Catch_Data$TOWDATE,3,5) #3 letter month abbrev in locations 3-5 of TOWDATE
month_match <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
Catch_Data$Month <- match(month,month_match)

#merge 3 individual date columns into 1
Catch_Data$Date <- as.Date(with(Catch_Data,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")



#### Grid time ID#### 

# You will complete one season at a time (assuming your survey runs for >1 season per year)
# Make sure that you are in a working directory specifically for the season in which you are running the analysis (...FVCOM/Fall/), some of the files will have the same names for different seasons (unless you change them) and you don't want to overwrite them and lose your work

##Fall
# # Define the dates in which the survey ran for each season each year. 
# # "by=" how often you want days... e.g by=2 means you will get data every 2  days 9/6/2000, 9/8/2000, 9/10/2000, etc... I recommend this if you are getting data for many months/years because I have had R crash because it was just too much for it to handle, getting daily data. 
# # len= defines the range of dates as 9/6 (there are 24 additional days in September) to 10/20 (add one to your last day. 20+1=21))
# dates <- format(c(seq(as.Date("9/6/2000", "%m/%d/%Y"), by=1, len=24+21), #date range from stations in fall 2000
#                   seq(as.Date("9/5/2001", "%m/%d/%Y"), by=1, len=25+23), #9/5 to 10/22
#                   seq(as.Date("9/4/2002", "%m/%d/%Y"), by=1, len=26+26), #9/4 to 10/25 etc
#                   seq(as.Date("9/7/2003", "%m/%d/%Y"), by=1, len=23+32),
#                   seq(as.Date("9/10/2004", "%m/%d/%Y"), by=1, len=20+28),
#                   seq(as.Date("9/7/2005", "%m/%d/%Y"), by=1, len=23+31+5),
#                   seq(as.Date("9/6/2006", "%m/%d/%Y"), by=1, len=24+26),
#                   seq(as.Date("9/5/2007", "%m/%d/%Y"), by=1, len=25+32),
#                   seq(as.Date("9/3/2008", "%m/%d/%Y"), by=1, len=27+31+9),
#                   seq(as.Date("9/13/2009", "%m/%d/%Y"), by=1, len=17+31+19),
#                   seq(as.Date("9/9/2010", "%m/%d/%Y"), by=1, len=21+31+30+3),
#                   seq(as.Date("9/10/2011", "%m/%d/%Y"), by=1, len=20+31+15),
#                   seq(as.Date("9/7/2012", "%m/%d/%Y"), by=1, len=23+31+11),
#                   seq(as.Date("9/6/2013", "%m/%d/%Y"), by=1, len=24+31+20),
#                   seq(as.Date("9/10/2014", "%m/%d/%Y"), by=1, len=20+31+13),
#                   seq(as.Date("9/2/2015", "%m/%d/%Y"), by=1, len=28+31+6)), format="%m/%d/%Y")
# 
# 
# dates <- as.data.frame(dates)

#INSERT MY DATES INSTEAD OF THE ONES ROBYN PROVIDED
dates <- as.data.frame(Catch_Data$Date)




#At this point check the "dates" file to ensure that the date ranges for each year are correct

colnames(dates) <- "date"
dates$y <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "-", fixed = TRUE))[1]))
dates$m <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "-", fixed = TRUE))[2]))
dates$d <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "-", fixed = TRUE))[3]))
#RECORD 2 DIGIT YEAR TO SET INDICES LATER
dates$y2 <- unlist(lapply(dates$y, function(x) paste0(unlist(strsplit(as.character(x),""))[3],unlist(strsplit(as.character(x),""))[4])))

#INDEX TO USE LATER
dates$index <- seq(length(dates$y))

dates$modified_julian_date <- julian(as.numeric(dates$m), as.numeric(dates$d), as.numeric(dates$y),c(month = 11, day = 17, year = 1858)) + 0.5
dates$Season <- Catch_Data$SEASON

#### Download FVCOM time ID ####
fvcom_data <-as.data.frame(read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?time[0:1:342347]"))
fvcom_time <- as.data.frame(fvcom_data$Dataset..[5:nrow(fvcom_data)])
names(fvcom_time) <-"modified_julian_date"
fvcom_time$modified_julian_date <- as.numeric(as.character(fvcom_time$modified_julian_date))
fvcom_time$id <- 0:(nrow(fvcom_time)-1)

#### Match time ####
time_id <- c()
for(i in 1:nrow(dates)){
  temp <- fvcom_time$id[which(round(fvcom_time$modified_julian_date,2)==round(dates$modified_julian_date[i],2))]
  if(length(temp)==0) time_id[i] <- NA
  else time_id[i] <- temp
}
#### Download FVCOM location data ####
lat <- read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?lat[0:1:48450]")
lon <- read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?lon[0:1:48450]")
names(lat) <- "lat"
names(lon) <- "lon"

latitude <- lat$lat[5:nrow(lat)]
longitude <- lon$lon[5:nrow(lon)]
latitude <- as.numeric(as.character(latitude))
longitude <- as.numeric(as.character(longitude))


#### Grid location#### 
# Can also define specific lat and lon coordinates that you want to map instead of based on survey
start_x <- min(Catch_Data$Longitude)
end_x <- max(Catch_Data$Longitude)
start_y <-  min(Catch_Data$Latitude)
end_y <- max(Catch_Data$Latitude)
my_mesh=expand.grid(seq(start_x, end_x, by=0.01), seq(start_y, end_y, by=0.01)) #making a grid of every lat/lon combination to the 0.01 degree, can chose alternative resolution, but I wouldn't recommend any finer-scale
coordinates(my_mesh) <- ~Var1 + Var2
grid_data <-as.data.frame(my_mesh@coords)
colnames(grid_data) <-c("lon", "lat")
write.csv(grid_data, file="TestScripts/FVCOM/grid_data.csv")


####Download FVCOM Depth Data####
FVcom_depth <- read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?h[0:1:48450]")
names(FVcom_depth) <- "FVcom_depth"
FVcom_depth <- FVcom_depth$FVcom_depth[5:nrow(FVcom_depth)]
FVcom_depth <- as.numeric(as.character(FVcom_depth))
FVcom_depth <- cbind(longitude, latitude, FVcom_depth)
colnames(FVcom_depth) <- c("lon", "lat", "FVcom_depth")

FVcom_depth.list <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- as.data.frame(FVcom_depth)
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, FVcom_depth, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FVcom_depth.list[[i]] <- raster::extract(rast, grid_data)
}
save(FVcom_depth.list, file="TestScripts/FVCOM/FVcom_depth.list.RData")

grid_data=as.data.frame(grid_data)
FVdepth<-cbind(grid_data,FVcom_depth.list[[1]])
names(FVdepth)[3]<-"AvgDepth"
write.csv(FVdepth, file="TestScripts/FVCOM/FVdepth.csv")
#Assuming that you are using the same grid between seasons, the same FVdepth file will apply to both seasons

#### Download temperature data ####
## This will take a while, especially if you are doing several years
temperature_fvcom_data<-list()
for (i in 1:length(time_id)){
  print(i)
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?temp[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep="")) # 44:1:44 is only taking the bottom layer of water temps. If you wanted the entire water column you would do 0:1:44. 0:1:48450 does not need to be changed (basically saying that we want all of the data available from that layer for the dates we defined)
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  temperature_fvcom_data[[i]] <- cbind(longitude, latitude, temp_data)
  colnames(temperature_fvcom_data[[i]]) <- c("lon", "lat", "temperature")
}
save(temperature_fvcom_data, file="TestScripts/FVCOM/temperature_data.RData")

#load("TestScripts/FVCOM/temperature_data.RData")

#all years to collect
yrs <- unique(dates$y2)
sns <- c("FALL","SPRING")

#INSTEAD OF LISTING EACH YEAR & SEASON, LOOP THROUGH UNIQUE YEARS & SEASONS

TRD_all <- list()

for(sn in sns){
  for(yr in yrs){
    
    # #define object as ex TRD_FA09 
    # assign(paste("TRD_",sn,yr,sep = "") ,list())
    
    temp_lst <- list()
    
    #pull out year and season you want samples for
    sub_set <- subset(dates,(y2==yr) & (Season == sn), select=date:index )
    
    ind <- sub_set$index #pulling indices 
    
    print("max ind is")
    print(max(ind))
    
    lst_idx <- 1
    
    for(i in ind){ #go down the list
      print(i)
      
      temp_data <- as.data.frame(temperature_fvcom_data[[i]])
      rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)# Make sure this matches your resolution
      rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
      akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
      rast <- raster(akima.smooth)
      temp_lst[[lst_idx]] <- raster::extract(rast, grid_data)
      
      lst_idx <- lst_idx + 1
      
    }
    save(temp_lst, file=paste("TestScripts/FVCOM/TRD_",sn,yr,".RData",sep = ""))
    TRD_all[[paste("TRD_",sn,yr,sep = "")]] <- temp_lst
  }
  
}


### Average Temp Data For each year and season## 
for(sn in sns){
  for(yr in yrs){
   
    assign(paste("MeanTemp",sn,yr,sep = ""), as.data.frame(list(Reduce(`+`, TRD_all[[paste("TRD_",sn,yr,sep = "")]]) / length(TRD_all[[paste("TRD_",sn,yr,sep = "")]]))))
  
    temp <- list(Reduce(`+`, TRD_all[[paste("TRD_",sn,yr,sep = "")]]) / length(TRD_all[[paste("TRD_",sn,yr,sep = "")]]))
    
    
    write.csv(temp, file=paste("TestScripts/FVCOM/MeanTemp",sn,yr,".csv",sep = ""))
    }
}












































###Download Salinity Data
salinity_fvcom_data<-list()
for (i in 1:length(time_id)){
  print(i)
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?salinity[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  salinity_fvcom_data[[i]] <- cbind(longitude, latitude, temp_data)
  colnames(salinity_fvcom_data[[i]]) <- c("lon", "lat", "salinity")
}
save(salinity_fvcom_data, file="TestScripts/FVCOM/salinity_data.RData")

#load("...FVCOM/Fall/salinity_data.RData")


###Take salinity data and interpolate and snap it to "grid_data" grid that was made###
### Fall
## Fall 2000
# the time ID is the same so it should be the same as for your temp. In this case, 1:45 and so on
SRD_FA00 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:45){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA00[[i]] <- raster::extract(rast, grid_data)
}
save(SRD_FA00, file="SRD_FA00.RData")

###Fall 2001
SRD_FA01 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 46:93){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA01[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA01<- SRD_FA01[-c(1:45)] 
save(SRD_FA01, file="SRD_FA01.RData")

###Fall 2002
SRD_FA02 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 94:145){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA02[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA02<- SRD_FA02[-c(1:93)]
save(SRD_FA02, file="SRD_FA02.RData")

###Fall 2003
SRD_FA03 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 146:200){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA03[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA03<- SRD_FA03[-c(1:145)]
save(SRD_FA03, file="SRD_FA03.RData")

###Fall 2004
SRD_FA04 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 201:248){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA04[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA04<- SRD_FA04[-c(1:200)]
save(SRD_FA04, file="SRD_FA04.RData")

###Fall 2005
SRD_FA05 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 249:307){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA05[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA05<- SRD_FA05[-c(1:248)]
save(SRD_FA05, file="SRD_FA05.RData")

###Fall 2006
SRD_FA06 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 308:357){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA06[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA06<- SRD_FA06[-c(1:307)]
save(SRD_FA06, file="SRD_FA06.RData")

###Fall 2007
SRD_FA07 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 358:414){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA07[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA07<- SRD_FA07[-c(1:357)]
save(SRD_FA07, file="SRD_FA07.RData")

###Fall 2008
SRD_FA08 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 415:481){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA08[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA08<- SRD_FA08[-c(1:414)]
save(SRD_FA08, file="SRD_FA08.RData")

###Fall 2009
SRD_FA09 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 482:548){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA09[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA09<- SRD_FA09[-c(1:481)]
save(SRD_FA09, file="SRD_FA09.RData")

###Fall 2010
SRD_FA10 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 549:633){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA10[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA10<- SRD_FA10[-c(1:548)]
save(SRD_FA10, file="SRD_FA10.RData")


###Fall 2011
SRD_FA11 <- list()
for(i in 634:699){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA11[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA11<- SRD_FA11[-c(1:633)]

save(SRD_FA11, file="SRD_FA11.RData")


### Fall 2012
SRD_FA12 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 700:764){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA12[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA12<- SRD_FA12[-c(1:699)]
save(SRD_FA12, file="SRD_FA12.RData")


### Fall 2013
SRD_FA13 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 765:839){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA13[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA13<- SRD_FA13[-c(1:764)]
save(SRD_FA13, file="SRD_FA13.RData")


### Fall 2014
SRD_FA14 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 840:903){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA14[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA14<- SRD_FA14[-c(1:839)]
save(SRD_FA14, file="SRD_FA14.RData")


### Fall 2015
SRD_FA15 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 904:968){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA15[[i]] <- raster::extract(rast, grid_data)
}
SRD_FA15<- SRD_FA15[-c(1:903)]
save(SRD_FA15, file="SRD_FA15.RData")

# Average for Fall Salinity

MeanSalfa00<- list(Reduce(`+`, SRD_FA00) / length(SRD_FA00))###taking averages of each day during the season at each location
MeanSalfa01<- list(Reduce(`+`, SRD_FA01) / length(SRD_FA01))
MeanSalfa02<- list(Reduce(`+`, SRD_FA02) / length(SRD_FA02))
MeanSalfa03<- list(Reduce(`+`, SRD_FA03) / length(SRD_FA03))
MeanSalfa04<- list(Reduce(`+`, SRD_FA04) / length(SRD_FA04))
MeanSalfa05<- list(Reduce(`+`, SRD_FA05) / length(SRD_FA05))
MeanSalfa06<- list(Reduce(`+`, SRD_FA06) / length(SRD_FA06))
MeanSalfa07<- list(Reduce(`+`, SRD_FA07) / length(SRD_FA07))
MeanSalfa08<- list(Reduce(`+`, SRD_FA08) / length(SRD_FA08))
MeanSalfa09<- list(Reduce(`+`, SRD_FA09) / length(SRD_FA09))
MeanSalfa10<- list(Reduce(`+`, SRD_FA10) / length(SRD_FA10)) 
MeanSalfa11<- list(Reduce(`+`, SRD_FA11) / length(SRD_FA11))
MeanSalfa12<- list(Reduce(`+`, SRD_FA12) / length(SRD_FA12))
MeanSalfa13<- list(Reduce(`+`, SRD_FA13) / length(SRD_FA13))
MeanSalfa14<- list(Reduce(`+`, SRD_FA14) / length(SRD_FA14))
MeanSalfa15<- list(Reduce(`+`, SRD_FA15) / length(SRD_FA15))

write.csv(MeanSalfa00, file="MeanSalfa00.csv")
write.csv(MeanSalfa01, file="MeanSalfa01.csv")
write.csv(MeanSalfa02, file="MeanSalfa02.csv")
write.csv(MeanSalfa03, file="MeanSalfa03.csv")
write.csv(MeanSalfa04, file="MeanSalfa04.csv")
write.csv(MeanSalfa05, file="MeanSalfa05.csv")
write.csv(MeanSalfa06, file="MeanSalfa06.csv")
write.csv(MeanSalfa07, file="MeanSalfa07.csv")
write.csv(MeanSalfa08, file="MeanSalfa08.csv")
write.csv(MeanSalfa09, file="MeanSalfa09.csv")
write.csv(MeanSalfa10, file="MeanSalfa10.csv")
write.csv(MeanSalfa11, file="MeanSalfa11.csv")
write.csv(MeanSalfa12, file="MeanSalfa12.csv")
write.csv(MeanSalfa13, file="MeanSalfa13.csv")
write.csv(MeanSalfa14, file="MeanSalfa14.csv")
write.csv(MeanSalfa15, file="MeanSalfa15.csv")


#### Fall FVCOM Files through 2015 ###
ts2000= data.frame(FVdepth)
ts2000$BTemp=MeanTempfa00[,c(2)]
ts2000$Salinity=MeanSalfa01[,c(2)]
# You can add your sediment file here as well assuming that the coordinates correspond
write.csv(ts2000, file="ts2000.csv")

ts2001= data.frame(FVdepth)
ts2001$BTemp=MeanTempfa01[,c(2)]
ts2001$Salinity=MeanSalfa01[,c(2)]
write.csv(ts2001, file="ts2001.csv")

ts2002= data.frame(FVdepth)
ts2002$BTemp=MeanTempfa02[,c(2)]
ts2002$Salinity=MeanSalfa02[,c(2)]
write.csv(ts2002, file="ts2002.csv")

ts2003= data.frame(FVdepth)
ts2003$BTemp=MeanTempfa03[,c(2)]
ts2003$Salinity=MeanSalfa03[,c(2)]
write.csv(ts2003, file="ts2003.csv")

ts2004= data.frame(FVdepth)
ts2004$BTemp=MeanTempfa04[,c(2)]
ts2004$Salinity=MeanSalfa04[,c(2)]
write.csv(ts2004, file="ts2004.csv")

ts2005= data.frame(FVdepth)
ts2005$BTemp=MeanTempfa05[,c(2)]
ts2005$Salinity=MeanSalfa05[,c(2)]
write.csv(ts2005, file="ts2005.csv")

ts2006= data.frame(FVdepth)
ts2006$BTemp=MeanTempfa06[,c(2)]
ts2006$Salinity=MeanSalfa06[,c(2)]
write.csv(ts2006, file="ts2006.csv")

ts2007= data.frame(FVdepth)
ts2007$BTemp=MeanTempfa07[,c(2)]
ts2007$Salinity=MeanSalfa07[,c(2)]
write.csv(ts2007, file="ts2007.csv")

ts2008= data.frame(FVdepth)
ts2008$BTemp=MeanTempfa08[,c(2)]
ts2008$Salinity=MeanSalfa08[,c(2)]
write.csv(ts2008, file="ts2008.csv")

ts2009= data.frame(FVdepth)
ts2009$BTemp=MeanTempfa09[,c(2)]
ts2009$Salinity=MeanSalfa09[,c(2)]
write.csv(ts2009, file="ts2009.csv")

ts2010= data.frame(FVdepth)
ts2010$BTemp=MeanTempfa10[,c(2)]
ts2010$Salinity=MeanSalfa10[,c(2)]
write.csv(ts2010, file="ts2010.csv")

ts2011= data.frame(FVdepth)
ts2011$BTemp=MeanTempfa11[,c(2)]
ts2011$Salinity=MeanSalfa11[,c(2)]
write.csv(ts2011, file="ts2011.csv")


ts2012= data.frame(FVdepth)
ts2012$BTemp=MeanTempfa12[,c(2)]
ts2012$Salinity=MeanSalfa12[,c(2)]
write.csv(ts2012, file="ts2012.csv")


ts2013= data.frame(FVdepth)
ts2013$BTemp=MeanTempfa13[,c(2)]
ts2013$Salinity=MeanSalfa13[,c(2)]
write.csv(ts2013, file="ts2013.csv")


ts2014= data.frame(FVdepth)
ts2014$BTemp=MeanTempfa14[,c(2)]
ts2014$Salinity=MeanSalfa14[,c(2)]
write.csv(ts2014, file="ts2014.csv")


ts2015= data.frame(FVdepth)
ts2015$BTemp=MeanTempfa15[,c(2)]
ts2015$Salinity=MeanSalfa15[,c(2)]
write.csv(ts2015, file="ts2015.csv")


####Spring####
## Be sure to switch working directories to "Spring" or any files with the same name (example: "temperature_fvcom_data") will be overwritten

dates <- format(c(seq(as.Date("3/16/2000", "%m/%d/%Y"), by=1, len=15+30+4), # spring 2000
                  seq(as.Date("2/28/2001", "%m/%d/%Y"), by=1, len=1+31+30), 
                  seq(as.Date("3/6/2002", "%m/%d/%Y"), by=1, len=25+25), 
                  seq(as.Date("3/6/2003", "%m/%d/%Y"), by=1, len=25+28), 
                  seq(as.Date("3/3/2004", "%m/%d/%Y"), by=1, len=28+22), 
                  seq(as.Date("3/4/2005", "%m/%d/%Y"), by=1, len=27+22),
                  seq(as.Date("3/8/2006", "%m/%d/%Y"), by=1, len=23+20),
                  seq(as.Date("3/8/2007", "%m/%d/%Y"), by=1, len=23+28),
                  seq(as.Date("3/7/2008", "%m/%d/%Y"), by=1, len=24+30+4),
                  seq(as.Date("2/28/2009", "%m/%d/%Y"), by=1, len=1+31+30+8),
                  seq(as.Date("2/28/2010", "%m/%d/%Y"), by=1, len=1+31+30+1),
                  seq(as.Date("3/3/2011", "%m/%d/%Y"), by=1, len=28+30+11),
                  seq(as.Date("2/29/2012", "%m/%d/%Y"), by=1, len=1+31+30+3),
                  seq(as.Date("3/15/2013", "%m/%d/%Y"), by=1, len=16+30+9),
                  seq(as.Date("3/31/2014", "%m/%d/%Y"), by=1, len=1+30+31),
                  seq(as.Date("3/14/2015", "%m/%d/%Y"), by=1, len=17+30+7)), format="%m/%d/%Y") 

dates <- as.data.frame(dates)
colnames(dates) <- "date"
dates$y <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[3]))
dates$m <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[1]))
dates$d <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[2]))
dates$modified_julian_date <- julian(as.numeric(dates$m), as.numeric(dates$d), as.numeric(dates$y),c(month = 11, day = 17, year = 1858)) + 0.5

#### Download FVCOM time ID ####
fvcom_data <-as.data.frame(read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?time[0:1:342347]"))
fvcom_time <- as.data.frame(fvcom_data$Dataset..[5:nrow(fvcom_data)])
names(fvcom_time) <-"modified_julian_date"
fvcom_time$modified_julian_date <- as.numeric(as.character(fvcom_time$modified_julian_date))
fvcom_time$id <- 0:(nrow(fvcom_time)-1)

#### Match time ####
time_id <- c()
for(i in 1:nrow(dates)){
  temp <- fvcom_time$id[which(round(fvcom_time$modified_julian_date,2)==round(dates$modified_julian_date[i],2))]
  if(length(temp)==0) time_id[i] <- NA
  else time_id[i] <- temp
} 

##Depth and grid files are the same as above so you don't need to run those again, but if completing on a different day you may need to re-run and read in a few files

# latitude, longitude (lines 76-84)
# grid_data=read.csv("TestScripts/FVCOM/grid_data.csv")
# FVdepth= read.csv("TestScripts/FVCOM/FVdepth.csv")

#### Download temperature data ####
temperature_fvcom_data<-list()
for (i in 1:length(time_id)){
  print(i)
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?temp[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  temperature_fvcom_data[[i]] <- cbind(longitude, latitude, temp_data)
  colnames(temperature_fvcom_data[[i]]) <- c("lon", "lat", "temperature")
}
save(temperature_fvcom_data, file="temperature_data.RData")

#load("TestScripts/FVCOM/temperature_data.RData")


###Spring 2000
TRD_SP00 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:49){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP00[[i]] <- raster::extract(rast, grid_data)
}
save(TRD_SP00, file="TRD_SP00.RData")

###Spring 2001
TRD_SP01 <- list()
for(i in 50:111){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP01[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP01<- TRD_SP01[-c(1:49)]
save(TRD_SP01, file="TRD_SP01.RData")

###Spring 2002
TRD_SP02 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 112:161){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP02[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP02<- TRD_SP02[-c(1:111)]
save(TRD_SP02, file="TRD_SP02.RData")

#Spring 2003
TRD_SP03 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 162:214){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP03[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP03<- TRD_SP03[-c(1:161)]
save(TRD_SP03, file="TRD_SP03.RData")

###Spring 2004
TRD_SP04 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 215:264){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP04[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP04<- TRD_SP04[-c(1:214)]
save(TRD_SP04, file="TRD_SP04.RData")

###Spring 2005
TRD_SP05 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 265:313){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP05[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP05<- TRD_SP05[-c(1:264)]
save(TRD_SP05, file="TRD_SP05.RData")

###Spring 2006
TRD_SP06 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 314:356){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP06[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP06<- TRD_SP06[-c(1:313)]
save(TRD_SP06, file="TRD_SP06.RData")

#2007
TRD_SP07 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 357:407){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP07[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP07<- TRD_SP07[-c(1:356)]
save(TRD_SP07, file="TRD_SP07.RData")

#2008
TRD_SP08 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 408:465){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP08[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP08<- TRD_SP08[-c(1:407)]
save(TRD_SP08, file="TRD_SP08.RData")

#2009
TRD_SP09 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 466:535){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP09[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP09<- TRD_SP09[-c(1:465)]
save(TRD_SP09, file="TRD_SP09.RData")

#2010
TRD_SP10 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 536:598){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP10[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP10<- TRD_SP10[-c(1:535)]
save(TRD_SP10, file="TRD_SP10.RData")

#2011
TRD_SP11 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 599:667){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP11[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP11<- TRD_SP11[-c(1:598)]
save(TRD_SP11, file="TRD_SP11.RData")

#2012
TRD_SP12 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 668:732){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP12[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP12<- TRD_SP12[-c(1:667)]
save(TRD_SP12, file="TRD_SP12.RData")

#2013
TRD_SP13 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 733:787){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP13[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP13<- TRD_SP13[-c(1:732)]
save(TRD_SP13, file="TRD_SP13.RData")

#2014
TRD_SP14 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 788:849){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP14[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP14<- TRD_SP14[-c(1:787)]
save(TRD_SP14, file="TRD_SP14.RData")

#2015
TRD_SP15 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 850:903){
  print(i)
  temp_data <- as.data.frame(temperature_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP15[[i]] <- raster::extract(rast, grid_data)
}
TRD_SP15<- TRD_SP15[-c(1:849)]
save(TRD_SP15, file="TRD_SP15.RData")


MeanTempsp00<- list(Reduce(`+`, TRD_SP00) / length(TRD_SP00)) ###taking averages of each day during the season at each location
MeanTempsp01<- list(Reduce(`+`, TRD_SP01) / length(TRD_SP01))
MeanTempsp02<- list(Reduce(`+`, TRD_SP02) / length(TRD_SP02))
MeanTempsp03<- list(Reduce(`+`, TRD_SP03) / length(TRD_SP03))
MeanTempsp04<- list(Reduce(`+`, TRD_SP04) / length(TRD_SP04))
MeanTempsp05<- list(Reduce(`+`, TRD_SP05) / length(TRD_SP05))
MeanTempsp06<- list(Reduce(`+`, TRD_SP06) / length(TRD_SP06))
MeanTempsp07<- list(Reduce(`+`, TRD_SP07) / length(TRD_SP07))
MeanTempsp08<- list(Reduce(`+`, TRD_SP08) / length(TRD_SP08))
MeanTempsp09<- list(Reduce(`+`, TRD_SP09) / length(TRD_SP09))
MeanTempsp10<- list(Reduce(`+`, TRD_SP10) / length(TRD_SP10))
MeanTempsp11<- list(Reduce(`+`, TRD_SP11) / length(TRD_SP11))
MeanTempsp12<- list(Reduce(`+`, TRD_SP12) / length(TRD_SP12))
MeanTempsp13<- list(Reduce(`+`, TRD_SP13) / length(TRD_SP13))
MeanTempsp14<- list(Reduce(`+`, TRD_SP14) / length(TRD_SP14))
MeanTempsp15<- list(Reduce(`+`, TRD_SP15) / length(TRD_SP15))

write.csv(MeanTempsp00, file="MeanTempsp00.csv")
write.csv(MeanTempsp01, file="MeanTempsp01.csv")
write.csv(MeanTempsp02, file="MeanTempsp02.csv")
write.csv(MeanTempsp03, file="MeanTempsp03.csv")
write.csv(MeanTempsp04, file="MeanTempsp04.csv")
write.csv(MeanTempsp05, file="MeanTempsp05.csv")
write.csv(MeanTempsp06, file="MeanTempsp06.csv")
write.csv(MeanTempsp07, file="MeanTempsp07.csv")
write.csv(MeanTempsp08, file="MeanTempsp08.csv")
write.csv(MeanTempsp09, file="MeanTempsp09.csv")
write.csv(MeanTempsp10, file="MeanTempsp10.csv")
write.csv(MeanTempsp11, file="MeanTempsp11.csv")
write.csv(MeanTempsp12, file="MeanTempsp12.csv")
write.csv(MeanTempsp13, file="MeanTempsp13.csv")
write.csv(MeanTempsp14, file="MeanTempsp14.csv")
write.csv(MeanTempsp15, file="MeanTempsp15.csv")


###Download Salinity Data###
salinity_fvcom_data<-list()
for (i in 1:length(time_id)){
  print(i)
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?salinity[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  salinity_fvcom_data[[i]] <- cbind(longitude, latitude, temp_data)
  colnames(salinity_fvcom_data[[i]]) <- c("lon", "lat", "salinity")
}
save(salinity_fvcom_data, file="salinity_data.RData")

#load("salinity_data.RData")

###Spring2000.
SRD_SP00 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:49){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP00[[i]] <- raster::extract(rast, grid_data)
}
save(SRD_SP00, file="SRD_SP00.RData")

###Spring2001
SRD_SP01 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 50:111){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP01[[i]] <- raster::extract(rast, grid_data)
}
SRD_SP01<- SRD_SP01[-c(1:49)]
save(SRD_SP01, file="SRD_SP01.RData")

###Spring2002
SRD_SP02 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 112:161){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP02[[i]] <- raster::extract(rast, grid_data)
}
SRD_SP02<- SRD_SP02[-c(1:111)]
save(SRD_SP02, file="SRD_SP02.RData")

###Spring2003
SRD_SP03 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 162:214){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP03[[i]] <- raster::extract(rast, grid_data)
}
SRD_SP03<- SRD_SP03[-c(1:161)]
save(SRD_SP03, file="SRD_SP03.RData")

###Spring2004
SRD_SP04 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 215:264){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP04[[i]] <- raster::extract(rast, grid_data)
}
SRD_SP04<- SRD_SP04[-c(1:214)]
save(SRD_SP04, file="SRD_SP04.RData")

###Spring2005
SRD_SP05 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 265:313){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP05[[i]] <- raster::extract(rast, grid_data)
}
SRD_SP05<- SRD_SP05[-c(1:264)]
save(SRD_SP05, file="SRD_SP05.RData")

###Spring2006
SRD_SP06 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 314:356){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP06[[i]] <- raster::extract(rast, grid_data)
}
SRD_SP06<- SRD_SP06[-c(1:313)]
save(SRD_SP06, file="SRD_SP06.RData")

###Spring2007
SRD_SP07 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 357:407){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP07[[i]] <- raster::extract(rast, grid_data)
}
SRD_SP07<- SRD_SP07[-c(1:356)]
save(SRD_SP07, file="SRD_SP07.RData")

###Spring2008
SRD_SP08 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 408:465){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP08[[i]] <- raster::extract(rast, grid_data)
}
SRD_SP08<- SRD_SP08[-c(1:407)]
save(SRD_SP08, file="SRD_SP08.RData")

###Spring2009
SRD_SP09 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 466:535){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP09[[i]] <- raster::extract(rast, grid_data)
}
SRD_SP09<- SRD_SP09[-c(1:465)]
save(SRD_SP09, file="SRD_SP09.RData")

###Spring2010
SRD_SP10 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 536:598){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP10[[i]] <- raster::extract(rast, grid_data)
}
SRD_SP10<- SRD_SP10[-c(1:535)]
save(SRD_SP10, file="SRD_SP10.RData")


###Spring 2011
SRD_SP11 <- list()
for(i in 599:667){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP11[[i]] <- raster::extract(rast, grid_data)
}
SRD_SP11<- SRD_SP11[-c(1:598)]
save(SRD_SP11, file="SRD_SP11.RData")


### Spring 2012
SRD_SP12 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 668:732){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FA12[[i]] <- raster::extract(rast, grid_data)
}
SRD_SP12<- SRD_SP12[-c(1:667)]
save(SRD_SP12, file="SRD_SP12.RData")


### Spring 2013
SRD_SP13 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 733:787){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP13[[i]] <- raster::extract(rast, grid_data)
}
SRD_SP13<- SRD_SP13[-c(1:732)]
save(SRD_SP13, file="SRD_SP13.RData")


### Spring 2014
SRD_SP14 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 788:849){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP14[[i]] <- raster::extract(rast, grid_data)
}
SRD_SP14<- SRD_SP14[-c(1:787)]
save(SRD_SP14, file="SRD_SP14.RData")


### Spring 2015
SRD_SP15 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 850:903){
  print(i)
  temp_data <- as.data.frame(salinity_fvcom_data[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP15[[i]] <- raster::extract(rast, grid_data)
}
SRD-SP15<- SRD_SP15[-c(1:849)]
save(SRD-SP15, file="SRD_SP15.RData")


##Average Spring Salinity
MeanSalsp00<- list(Reduce(`+`, SRD_SP00) / length(SRD_SP00))###taking averages of each day during the season at each location
MeanSalsp01<- list(Reduce(`+`, SRD_SP01) / length(SRD_SP01))
MeanSalsp02<- list(Reduce(`+`, SRD_SP02) / length(SRD_SP02))
MeanSalsp03<- list(Reduce(`+`, SRD_SP03) / length(SRD_SP03))
MeanSalsp04<- list(Reduce(`+`, SRD_SP04) / length(SRD_SP04))
MeanSalsp05<- list(Reduce(`+`, SRD_SP05) / length(SRD_SP05))
MeanSalsp06<- list(Reduce(`+`, SRD_SP06) / length(SRD_SP06))
MeanSalsp07<- list(Reduce(`+`, SRD_SP07) / length(SRD_SP07))
MeanSalsp08<- list(Reduce(`+`, SRD_SP08) / length(SRD_SP08))
MeanSalsp09<- list(Reduce(`+`, SRD_SP09) / length(SRD_SP09))
MeanSalsp10<- list(Reduce(`+`, SRD_SP10) / length(SRD_SP10)) 
MeanSalsp11<- list(Reduce(`+`, SRD_SP11) / length(SRD_SP11))
MeanSalsp12<- list(Reduce(`+`, SRD_SP12) / length(SRD_SP12))
MeanSalsp13<- list(Reduce(`+`, SRD_SP13) / length(SRD_SP13))
MeanSalsp14<- list(Reduce(`+`, SRD_SP14) / length(SRD_SP14))
MeanSalsp15<- list(Reduce(`+`, SRD_SP15) / length(SRD_SP15))

write.csv(MeanSalsp00, file="MeanSalsp00.csv")
write.csv(MeanSalsp01, file="MeanSalsp01.csv")
write.csv(MeanSalsp02, file="MeanSalsp02.csv")
write.csv(MeanSalsp03, file="MeanSalsp03.csv")
write.csv(MeanSalsp04, file="MeanSalsp04.csv")
write.csv(MeanSalsp05, file="MeanSalsp05.csv")
write.csv(MeanSalsp06, file="MeanSalsp06.csv")
write.csv(MeanSalsp07, file="MeanSalsp07.csv")
write.csv(MeanSalsp08, file="MeanSalsp08.csv")
write.csv(MeanSalsp09, file="MeanSalsp09.csv")
write.csv(MeanSalsp10, file="MeanSalsp10.csv")
write.csv(MeanSalsp11, file="MeanSalsp11.csv")
write.csv(MeanSalsp12, file="MeanSalsp12.csv")
write.csv(MeanSalsp13, file="MeanSalsp13.csv")
write.csv(MeanSalsp14, file="MeanSalsp14.csv")
write.csv(MeanSalsp15, file="MeanSalsp15.csv")

##Spring FVCOM files through 2015
ts2000= data.frame(FVdepth)
ts2000$BTemp=MeanTempsp00[,c(2)]
ts2000$Salinity=MeanSalsp00[,c(2)]
write.csv(ts2000, file="ts2000.csv")

ts2001= data.frame(FVdepth)
ts2001$BTemp=MeanTempsp01[,c(2)]
ts2001$Salinity=MeanSalsp01[,c(2)]
write.csv(ts2001, file="ts2001.csv")

ts2002= data.frame(FVdepth)
ts2002$BTemp=MeanTempsp02[,c(2)]
ts2002$Salinity=MeanSalsp02[,c(2)]
write.csv(ts2002, file="ts2002.csv")

ts2003= data.frame(FVdepth)
ts2003$BTemp=MeanTempsp03[,c(2)]
ts2003$Salinity=MeanSalsp03[,c(2)]
write.csv(ts2003, file="ts2003.csv")

ts2004= data.frame(FVdepth)
ts2004$BTemp=MeanTempsp04[,c(2)]
ts2004$Salinity=MeanSalsp04[,c(2)]
write.csv(ts2004, file="ts2004.csv")

ts2005= data.frame(FVdepth)
ts2005$BTemp=MeanTempsp05[,c(2)]
ts2005$Salinity=MeanSalsp05[,c(2)]
write.csv(ts2005, file="ts2005.csv")

ts2006= data.frame(FVdepth)
ts2006$BTemp=MeanTempsp06[,c(2)]
ts2006$Salinity=MeanSalsp06[,c(2)]
write.csv(ts2006, file="ts2006.csv")

ts2007= data.frame(FVdepth)
ts2007$BTemp=MeanTempsp07[,c(2)]
ts2007$Salinity=MeanSalsp07[,c(2)]
write.csv(ts2007, file="ts2007.csv")

ts2008= data.frame(FVdepth)
ts2008$BTemp=MeanTempsp08[,c(2)]
ts2008$Salinity=MeanSalsp08[,c(2)]
write.csv(ts2008, file="ts2008.csv")

ts2009= data.frame(FVdepth)
ts2009$BTemp=MeanTempsp09[,c(2)]
ts2009$Salinity=MeanSalsp09[,c(2)]
write.csv(ts2009, file="ts2009.csv")

ts2010= data.frame(FVdepth)
ts2010$BTemp=MeanTempsp10[,c(2)]
ts2010$Salinity=MeanSalsp10[,c(2)]
write.csv(ts2010, file="ts2010.csv")

ts2011= data.frame(FVdepth)
ts2011$BTemp=MeanTempsp11[,c(2)]
ts2011$Salinity=MeanSalsp11[,c(2)]
write.csv(ts2011, file="ts2011.csv")

ts2012= data.frame(FVdepth)
ts2012$BTemp=MeanTempsp12[,c(2)]
ts2012$Salinity=MeanSalsp12[,c(2)]
write.csv(ts2012, file="ts2012.csv")

ts2013= data.frame(FVdepth)
ts2013$BTemp=MeanTempsp13[,c(2)]
ts2013$Salinity=MeanSalsp13[,c(2)]
write.csv(ts2013, file="ts2013.csv")

ts2014= data.frame(FVdepth)
ts2014$BTemp=MeanTempsp14[,c(2)]
ts2014$Salinity=MeanSalsp14[,c(2)]
write.csv(ts2014, file="ts2014.csv")

ts2015= data.frame(FVdepth)
ts2015$BTemp=MeanTempsp15[,c(2)]
ts2015$Salinity=MeanSalsp15[,c(2)]
write.csv(ts2015, file="ts2015.csv")


#### 2016 & 2017 FVCOM Data ####

##Fall##

dates <- format(c(seq(as.Date("9/1/2016", "%m/%d/%Y"), by=1, len=30), # September 2016. The new organization of data is by month so it is easier to separate this way
                  seq(as.Date("10/1/2016", "%m/%d/%Y"), by=1, len=31),  #October 2016
                  seq(as.Date("10/1/2017", "%m/%d/%Y"), by=1, len=31),
                  seq(as.Date("11/1/2017", "%m/%d/%Y"), by=1, len=30)), format="%m/%d/%Y")

dates <- as.data.frame(dates)
colnames(dates) <- "date"
dates$y <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[3]))
dates$m <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[1]))
dates$d <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[2]))
dates$modified_julian_date <- julian(as.numeric(dates$m), as.numeric(dates$d), as.numeric(dates$y),c(month = 11, day = 17, year = 1858)) + 0.5 ###convert time to Julian. "+0.5" means I want data from only the noon hour to be retrieved

##Sep 2016
fvcom_time_916 <-nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201609.nc?Itime[0:1:720]") #The 201609 refers to September 2016. 0:1:720 refers to the number of hours of data (30days*24hrs=720). SO you will need to change these numbers for the months you want
fvcom_time_916<-ncvar_get(fvcom_time_916)
fvcom_time_916=data.frame((fvcom_time_916))
fvcom_time_916=distinct(fvcom_time_916)
colnames(fvcom_time_916) = c("modified_julian_date")
fvcom_time_916$modified_julian_date<-as.numeric(fvcom_time_916$modified_julian_date)+0.5

##Oct 2016
fvcom_time_1016 <-nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201610.nc?Itime[0:1:744]")
fvcom_time_1016<-ncvar_get(fvcom_time_1016)
fvcom_time_1016=data.frame((fvcom_time_1016))
fvcom_time_1016=distinct(fvcom_time_1016)
colnames(fvcom_time_1016) = c("modified_julian_date")
fvcom_time_1016$modified_julian_date<-as.numeric(fvcom_time_1016$modified_julian_date)+0.5

##Oct 2017
fvcom_time_1017 <-nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201710.nc?Itime[0:1:744]")
fvcom_time_1017<-ncvar_get(fvcom_time_1017)
fvcom_time_1017=data.frame((fvcom_time_1017))
fvcom_time_1017=distinct(fvcom_time_1017)
colnames(fvcom_time_1017) = c("modified_julian_date")
fvcom_time_1017$modified_julian_date<-as.numeric(fvcom_time_1017$modified_julian_date)+0.5

##Nov 2017
fvcom_time_1117 <-nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201711.nc?Itime[0:1:720]")
fvcom_time_1117<-ncvar_get(fvcom_time_1117)
fvcom_time_1117=data.frame((fvcom_time_1117))
fvcom_time_1117=distinct(fvcom_time_1117)
colnames(fvcom_time_1117) = c("modified_julian_date")
fvcom_time_1117$modified_julian_date<-as.numeric(fvcom_time_1117$modified_julian_date)+0.5


fvcom_time<-rbind(fvcom_time_916, fvcom_time_1016, fvcom_time_1017, fvcom_time_1117)
fvcom_time<- distinct(fvcom_time)
fvcom_time$modified_julian_date <- as.numeric(as.character(fvcom_time$modified_julian_date))
fvcom_time$id <- 0:(nrow(fvcom_time)-1)

#### Match time ####

time_id <- c()
for(i in 1:nrow(dates)){
  temp <- fvcom_time$id[which(round(fvcom_time$modified_julian_date,1)==round(dates$modified_julian_date[i],1))]
  if(length(temp)==0) time_id[i] <- NA
  else time_id[i] <- temp
}

#### Download FVCOM location data ####
## 2016 and 2017 are organized over different lat/lon meshes (gom3 for 2016 and gom5 for 2017) so we must look at each year separately to extract depth, temp and sal before we apply them to the grid we defined before

lon2016 <- as.data.frame(read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201604.nc.ascii?lon[0:1:48450]"))# #[0:1:48450] is used in GOM3 mesh and [0:1:136431] is used in GOM5 (2017) mesh. make sure you have these correct when you are trying to coource links because if not, it would give you a "400 Bad Request" or cannot open the connection type error
lat2016 <- as.data.frame(read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201604.nc.ascii?lat[0:1:48450]"))
names(lat2016) <- "lat"
names(lon2016) <- "lon"

latitude2016 <- lat2016$lat[5:nrow(lat2016)]
longitude2016 <- lon2016$lon[5:nrow(lon2016)]
latitude2016 <- as.numeric(as.character(latitude2016))
longitude2016 <- as.numeric(as.character(longitude2016))

lon2017 <- as.data.frame(read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201704.nc.ascii?lon[0:1:136431]"))
lat2017 <- as.data.frame(read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201704.nc.ascii?lat[0:1:136431]"))
names(lat2017) <- "lat"
names(lon2017) <- "lon"

latitude2017 <- lat2017$lat[5:nrow(lat2017)]
longitude2017 <- lon2017$lon[5:nrow(lon2017)]
latitude2017 <- as.numeric(as.character(latitude2017))
longitude2017 <- as.numeric(as.character(longitude2017))


#### Download depth data ####
library(SDMTools)
#
#### 
grid_data <- read.csv("grid_data") #This is the same file from the 2000-2015 FVCOM above

####Download FVCOM Depth Data####
###Even though depth data would be the same from 2016-2017 (and 2000-2015), you still need to download separate depth data for each year because they use different meshes

FVcom_depth2016 <- data.frame(ncvar_get(nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201605.nc?h[0:1:48450]")))
names(FVcom_depth2016) <- "FVcom_depth"
FVcom_depth2016 <- cbind(longitude2016, latitude2016, FVcom_depth2016)
colnames(FVcom_depth2016) <- c("lon", "lat", "FVcom_depth")

FVcom_depth2016.list <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- as.data.frame(FVcom_depth2016)
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, FVcom_depth, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FVcom_depth2016.list[[i]] <- raster::extract(rast, grid_data)
}
save(FVcom_depth2016.list, file="FVcom_depth2016.list.RData")

FVdepth2016<-cbind(grid_data[2:3],FVcom_depth2016.list[[1]])
names(FVdepth2016)[3]<-"AvgDepth"
save(FVdepth2016, file="FVdepth2016.RData")

FVcom_depth2017<- data.frame(ncvar_get(nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201705.nc?h[0:1:136431]")))
names(FVcom_depth2017) <- "FVcom_depth"
FVcom_depth2017 <- cbind(longitude2017, latitude2017, FVcom_depth2017)
colnames(FVcom_depth2017) <- c("lon", "lat", "FVcom_depth")

FVcom_depth2017.list <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:1){
  print(i)
  temp_data <- as.data.frame(FVcom_depth2017)
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, FVcom_depth, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  FVcom_depth2017.list[[i]] <- raster::extract(rast, grid_data)
}
save(FVcom_depth2017.list, file="FVcom_depth2017.list.RData")

FVdepth2017<-cbind(grid_data[2:3],FVcom_depth2017.list[[1]])
names(FVdepth2017)[3]<-"AvgDepth"
save(FVdepth2017, file="FVdepth2017.RData")


###Download Temperature Data###
##Keep an eye on the "time_id[i...]" section of each line. It is essentially removing the dates that are before the current month on the time_id list. 
# Sep 2016
sepfvcomtemp2016<-list()
for (i in 1:30){ #1:30 because there are 30 days in the month of September
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201609.nc.ascii?temp[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep="")) 
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  sepfvcomtemp2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(sepfvcomtemp2016[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(sepfvcomtemp2016, file="sepfvcomtemp2016.RData")

# Oct 2016
octfvcomtemp2016<-list()
for (i in 1:31){ #1:30 because there are 30 days in the month of April
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201610.nc.ascii?temp[", time_id[i+30], ":1:", time_id[i+30], "][44:1:44][0:1:48450]", sep="")) # time_id[i+30] because there are 30 days of data before this
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  octfvcomtemp2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(octfvcomtemp2016[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(octfvcomtemp2016, file="octfvcomtemp2016.RData")


# Oct 2017
octfvcomtemp2017<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201710.nc.ascii?temp[", time_id[i+61], ":1:", time_id[i+61], "][44:1:44][0:1:136431]", sep="")) #61 days of data before this
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  octfvcomtemp2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(octfvcomtemp2017[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(octfvcomtemp2017, file="octfvcomtemp2017.RData")

# Nov 2017
novfvcomtemp2017<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201711.nc.ascii?temp[", time_id[i+92], ":1:", time_id[i+92], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  novfvcomtemp2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(novfvcomtemp2017[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(novfvcomtemp2017, file="novfvcomtemp2017.RData")

FL16temp<-append(sepfvcomtemp2016, octfvcomtemp2016, after=30)#use append to combine different months. Don't need to do this if you're only using a single month. Just rename the file. "after 30" because there are 30 days in the first month (september)
FL17temp<-append(octfvcomtemp2017, novfvcomtemp2017, after=31)

####Take temp data and interpolate and snap it to "grid_data" grid that was made
###Fall2016
TRD_FL16 <- list()
for(i in 1:61){  #Pay  attention to the length of this for loop, it should be as long as your "FL16temp" list
  print(i)
  temp_data <- as.data.frame(FL16temp[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_FL16[[i]] <- raster::extract(rast, grid_data)
}
save(TRD_FL16, file="TRD_FL16.RData")

###Fall2017
TRD_FL17 <- list()
for(i in 1:61){
  print(i)
  temp_data <- as.data.frame(FL17temp[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_FL17[[i]] <- raster::extract(rast, grid_data)
}
save(TRD_FL17, file="TRD_FL17.RData")

##Taking average temps ##
MeanTempfl16<- list(Reduce(`+`, TRD_FL16) / length(TRD_FL16))
MeanTempfl17<- list(Reduce(`+`, TRD_FL17) / length(TRD_FL17))

write.csv(MeanTempfl16, file="MeanTempfl16.csv")
write.csv(MeanTempfl17, file="MeanTempfl17.csv")


### Download Salinity Data ###
## Bottom Layer Salinity ##

# Sep 2016
sepfvcomsal2016<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201609.nc.ascii?salinity[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  sepfvcomsal2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(sepfvcomsal2016[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(sepfvcomsal2016, file="sepfvcomsal2016.RData")

octfvcomsal2016<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201610.nc.ascii?salinity[", time_id[i+30], ":1:", time_id[i+30], "][44:1:44][0:1:48450]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  octfvcomsal2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(octfvcomsal2016[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(octfvcomsal2016, file="octfvcomsal2016.RData")

# Oct 2017
octfvcomsal2017<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201710.nc.ascii?salinity[", time_id[i+61], ":1:", time_id[i+61], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  octfvcomsal2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(octfvcomsal2017[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(octfvcomsal2017, file="octfvcomsal2017.RData")

# Nov 2017
novfvcomsal2017<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201711.nc.ascii?salinity[", time_id[i+92], ":1:", time_id[i+92], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  novfvcomsal2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(novfvcomsal2017[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(novfvcomsal2017, file="novfvcomsal2017.RData")

FL16sal<- append(sepfvcomsal2016, octfvcomsal2016, after=30)
FL17sal<- append(octfvcomsal2017, novfvcomsal2017, after=31)


####Take salinity data and interpolate and snap it to "grid_data" grid that was made
###Fall2016
SRD_FL16 <- list()
for(i in 1:61){
  print(i)
  temp_data <- as.data.frame(FL16sal[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FL16[[i]] <- raster::extract(rast, grid_data)
}
save(SRD_FL16, file="SRD_FL16.RData")

###Fall2017
SRD_FL17 <- list()
for(i in 1:61){
  print(i)
  temp_data <- as.data.frame(FL17sal[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_FL17[[i]] <- raster::extract(rast, grid_data)
}
save(SRD_FL17, file="SRD_FL17.RData")

MeanSalfl16<- list(Reduce(`+`, SRD_FL16) / length(SRD_FL16))
MeanSalfl17<- list(Reduce(`+`, SRD_FL17) / length(SRD_FL17))

write.csv(MeanSalfl16, file="MeanSalfl16.csv")
write.csv(MeanSalfl17, file="MeanSalfl17.csv")

ts2016= data.frame(FVdepth2016)
ts2016$BTemp=MeanTempfl16[,c(2)]
ts2016$Salinity=MeanSalfl16[,c(2)]
#Add your sediment column
write.csv(ts2016, file="ts2016.csv")

ts2017= data.frame(FVdepth2017)
ts2017$BTemp=MeanTempfl17[,c(2)]
ts2017$Salinity=MeanSalfl17[,c(2)]
write.csv(ts2017, file="ts2017.csv")


##Spring##

dates <- format(c(seq(as.Date("4/1/2016", "%m/%d/%Y"), by=1, len=30), 
                  seq(as.Date("5/1/2016", "%m/%d/%Y"), by=1, len=31),
                  seq(as.Date("3/1/2017", "%m/%d/%Y"), by=1, len=31),
                  seq(as.Date("4/1/2017", "%m/%d/%Y"), by=1, len=30),
                  seq(as.Date("5/1/2017", "%m/%d/%Y"), by=1, len=31)), format="%m/%d/%Y")


dates <- as.data.frame(dates)
colnames(dates) <- "date"
dates$y <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[3]))
dates$m <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[1]))
dates$d <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[2]))
dates$modified_julian_date <- julian(as.numeric(dates$m), as.numeric(dates$d), as.numeric(dates$y),c(month = 11, day = 17, year = 1858)) + 0.5 

##April 2016
fvcom_time_416 <-nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201604.nc?Itime[0:1:720]")
fvcom_time_416<-ncvar_get(fvcom_time_416)
fvcom_time_416=data.frame((fvcom_time_416))
fvcom_time_416=distinct(fvcom_time_416)
colnames(fvcom_time_416) = c("modified_julian_date")
fvcom_time_416$modified_julian_date<-as.numeric(fvcom_time_416$modified_julian_date)+0.5

##May 2016
fvcom_time_516 <-nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201605.nc?Itime[0:1:744]")
fvcom_time_516<-ncvar_get(fvcom_time_516)
fvcom_time_516=data.frame((fvcom_time_516))
fvcom_time_516=distinct(fvcom_time_516)
colnames(fvcom_time_516) = c("modified_julian_date")
fvcom_time_516$modified_julian_date<-as.numeric(fvcom_time_516$modified_julian_date)+0.5


##March 2017
fvcom_time_317 <-nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201703.nc?Itime[0:1:744]")
fvcom_time_317<-ncvar_get(fvcom_time_317)
fvcom_time_317=data.frame((fvcom_time_317))
fvcom_time_317=distinct(fvcom_time_317)
colnames(fvcom_time_317) = c("modified_julian_date")
fvcom_time_317$modified_julian_date<-as.numeric(fvcom_time_317$modified_julian_date)+0.5

##April 2017
fvcom_time_417 <-nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201704.nc?Itime[0:1:720]")
fvcom_time_417<-ncvar_get(fvcom_time_417)
fvcom_time_417=data.frame((fvcom_time_417))
fvcom_time_417=distinct(fvcom_time_417)
colnames(fvcom_time_417) = c("modified_julian_date")
fvcom_time_417$modified_julian_date<-as.numeric(fvcom_time_417$modified_julian_date)+0.5

##May 2017
fvcom_time_517 <-nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201705.nc?Itime[0:1:744]")
fvcom_time_517<-ncvar_get(fvcom_time_517)
fvcom_time_517=data.frame((fvcom_time_517))
fvcom_time_517=distinct(fvcom_time_517)
colnames(fvcom_time_517) = c("modified_julian_date")
fvcom_time_517$modified_julian_date<-as.numeric(fvcom_time_517$modified_julian_date)+0.5

fvcom_time<-rbind(fvcom_time_416, fvcom_time_516, fvcom_time_317, fvcom_time_417, fvcom_time_517)
fvcom_time<- distinct(fvcom_time)
fvcom_time$modified_julian_date <- as.numeric(as.character(fvcom_time$modified_julian_date))
fvcom_time$id <- 0:(nrow(fvcom_time)-1)

#### Match time ####

time_id <- c()
for(i in 1:nrow(dates)){
  temp <- fvcom_time$id[which(round(fvcom_time$modified_julian_date,1)==round(dates$modified_julian_date[i],1))]
  if(length(temp)==0) time_id[i] <- NA
  else time_id[i] <- temp
}

#Make sure that you have the 2016 & 2017 latitude, longitude and depth files from fall loaded

#### Download temperature data ####

# April 2016
aprilfvcomtemp2016<-list()
for (i in 1:30){ 
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201604.nc.ascii?temp[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep="")) 
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  aprilfvcomtemp2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(aprilfvcomtemp2016[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(aprilfvcomtemp2016, file="aprilfvcomtemp2016.RData")

# May 2016
mayfvcomtemp2016<-list()
for (i in 1:31){ #1:30 because there are 30 days in the month of April
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201605.nc.ascii?temp[", time_id[i+30], ":1:", time_id[i+30], "][44:1:44][0:1:48450]", sep="")) 
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  mayfvcomtemp2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(mayfvcomtemp2016[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(mayfvcomtemp2016, file="mayfvcomtemp2016.RData")

# March 2017
marchfvcomtemp2017<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201703.nc.ascii?temp[", time_id[i+61], ":1:", time_id[i+61], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  marchfvcomtemp2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(marchfvcomtemp2017[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(marchfvcomtemp2017, file="marchfvcomtemp2017.RData")

# April 2017
aprilfvcomtemp2017<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201704.nc.ascii?temp[", time_id[i+91], ":1:", time_id[i+91], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  aprilfvcomtemp2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(aprilfvcomtemp2017[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(aprilfvcomtemp2017, file="aprilfvcomtemp2017.RData")

# May 2017
mayfvcomtemp2017<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201705.nc.ascii?temp[", time_id[i+122], ":1:", time_id[i+122], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  mayfvcomtemp2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(mayfvcomtemp2017[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(mayfvcomtemp2017, file="mayfvcomtemp2017.RData")

SP16temp<- append(aprilfvcomtemp2016, mayfvcomtemp2016, after=30)
SP17temp<- append(marchfvcomtemp2017, aprilfvcomtemp2017, after=31)# need to append twice since there are 3 months included
SP17temp= append(SP17temp, mayfvcomtemp2017, after = 61)

TRD_SP16 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:61){
  print(i)
  temp_data <- as.data.frame(SP16temp[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP16[[i]] <- raster::extract(rast, grid_data)
}
save(TRD_SP16, file="TRD_SP16.RData")


###Spring2017
TRD_SP17 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:92){
  print(i)
  temp_data <- as.data.frame(SP17temp[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, temperature, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  TRD_SP17[[i]] <- raster::extract(rast, grid_data)
}
save(TRD_SP17, file="TRD_SP17.RData")


###taking averages of each day during the season at each location

MeanTempsp16<- list(Reduce(`+`, TRD_SP16) / length(TRD_SP16)) 
MeanTempsp17<- list(Reduce(`+`, TRD_SP17) / length(TRD_SP17)) 

write.csv(MeanTempsp16, file="MeanTempsp16.csv")
write.csv(MeanTempsp17, file="MeanTempsp17.csv")

#### Download salinity data ####

# April 2016
aprilfvcomsal2016<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201604.nc.ascii?salinity[", time_id[i], ":1:", time_id[i], "][44:1:44][0:1:48450]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  aprilfvcomsal2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(aprilfvcomsal2016[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(aprilfvcomsal2016, file="aprilfvcomsal2016.RData")

# May 2016
mayfvcomsal2016<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201605.nc.ascii?salinity[", time_id[i+30], ":1:", time_id[i+30], "][44:1:44][0:1:48450]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  mayfvcomsal2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(mayfvcomsal2016[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(mayfvcomsal2016, file="mayfvcomsal2016.RData")


# March 2017
marchfvcomsal2017<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201703.nc.ascii?salinity[", time_id[i+61], ":1:", time_id[i+61], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  marchfvcomsal2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(marchfvcomsal2017[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(marchfvcomsal2017, file="marchfvcomsal2017.RData")

# April 2017
aprilfvcomsal2017<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201704.nc.ascii?salinity[", time_id[i+92], ":1:", time_id[i+92], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  aprilfvcomsal2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(aprilfvcomsal2017[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(aprilfvcomsal2017, file="aprilfvcomsal2017.RData")

# May 2017
mayfvcomsal2017<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201705.nc.ascii?salinity[", time_id[i+122], ":1:", time_id[i+122], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  mayfvcomsal2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(mayfvcomsal2017[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(mayfvcomsal2017, file="mayfvcomsal2017.RData")

SP16sal<-append(aprilfvcomsal2016, mayfvcomsal2016, after = 30)
SP17sal<-append(marchfvcomsal2017, aprilfvcomsal2017, after = 31)
SP17sal= append(SP17sal, mayfvcomsal2017, after = 61)


###Spring2016
SRD_SP16 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:61){
  print(i)
  temp_data <- as.data.frame(SP16sal[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP16[[i]] <- raster::extract(rast, grid_data)
}
save(SRD_SP16, file="SRD_SP16.RData")


###Spring2017
SRD_SP17 <- list()
coordinates(grid_data) <- ~lon + lat
for(i in 1:92){
  print(i)
  temp_data <- as.data.frame(SP17sal[[i]])
  rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)
  rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
  akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
  rast <- raster(akima.smooth)
  SRD_SP17[[i]] <- raster::extract(rast, grid_data)
}
save(SRD_SP17, file="SRD_SP17.RData")

MeanSalsp16<- list(Reduce(`+`, SRD_SP16) / length(SRD_SP16))
MeanSalsp17<- list(Reduce(`+`, SRD_SP17) / length(SRD_SP17)) 

write.csv(MeanSalsp16, file="MeanSalsp16.csv")
write.csv(MeanSalsp17, file="MeanSalsp17.csv")

ts2016= data.frame(FVdepth2016)
ts2016$BTemp=MeanTempsp16[,c(2)]
ts2016$Salinity=MeanSalsp16[,c(2)]
# add sediment column
write.csv(ts2016, file="ts2016.csv")

ts2017= data.frame(FVdepth2017)
ts2017$BTemp=MeanTempsp17[,c(2)]
ts2017$Salinity=MeanSalsp17[,c(2)]
write.csv(ts2017, file="ts2017.csv")
