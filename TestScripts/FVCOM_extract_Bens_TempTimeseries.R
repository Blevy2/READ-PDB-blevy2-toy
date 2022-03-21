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

Catch_Data=read.csv("C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\Plot_survey\\ADIOS_SV_164744_GBK_NONE_survey_dist_map_fixed.csv") #The survey catch file you are using to build your model


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
month <- substr(Catch_Data$TOWDATE,3,5) #3 letter month abbreviation in locations 3-5 of TOWDATE
month_match <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
Catch_Data$Month <- match(month,month_match) #changes character abbreviation into number 1-12

#merge 3 individual date columns into 1
Catch_Data$Date <- as.Date(with(Catch_Data,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")



#### Grid time ID#### 

#COMBINE FALL AND SPRING DATES INTO SINGLE OBJECT


##Fall
# # Define the dates in which the survey ran for each season each year. 
# # "by=" how often you want days... e.g by=2 means you will get data every 2  days 9/6/2000, 9/8/2000, 9/10/2000, etc... I recommend this if you are getting data for many months/years because I have had R crash because it was just too much for it to handle, getting daily data. 
# # len= defines the range of dates as 9/6 (there are 24 additional days in September) to 10/20 (add one to your last day. 20+1=21))
dates <- format(c(seq(as.Date("1/1/2005", "%m/%d/%Y"), by=1, len=4017)), format="%m/%d/%Y")
#4017 days from 1/1/2005 through 12/31/2015

colnames(dates) <- "date"


dates <- as.data.frame(dates)






#At this point check the "dates" file to ensure that the date ranges for each year are correct


dates$y <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[3]))
dates$m <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[1]))
dates$d <- unlist(lapply(dates$date, function(x) unlist(strsplit(as.character(x), "/", fixed = TRUE))[2]))
dates$modified_julian_date <- julian(as.numeric(dates$m), as.numeric(dates$d), as.numeric(dates$y),c(month = 11, day = 17, year = 1858)) + 0.5
#RECORD 2 DIGIT YEAR TO SET INDICES LATER
dates$y2 <- unlist(lapply(dates$y, function(x) paste0(unlist(strsplit(as.character(x),""))[3],unlist(strsplit(as.character(x),""))[4])))

#INDEX TO USE LATER
dates$index <- seq(length(dates$y))

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
#### Download FVCOM location data ####
lat <- read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?lat[0:1:48450]")
lon <- read.csv("http://www.smast.umassd.edu:8080/thredds/dodsC/fvcom/hindcasts/30yr_gom3.ascii?lon[0:1:48450]")
names(lat) <- "lat"
names(lon) <- "lon"

latitude <- lat$lat[5:nrow(lat)]
longitude <- lon$lon[5:nrow(lon)]
latitude <- as.numeric(as.character(latitude))
longitude <- as.numeric(as.character(longitude))


#DEFININIG STRATAS TO USE AS BOUNDING BOX FOR GRID
strata.dir <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\" # strata shape files in this directory

# get the shapefiles
strata.areas <- readOGR(paste(strata.dir,"Survey_strata", sep="")) #readShapePoly is deprecated; use rgdal::readOGR or sf::st_read 

#GEORGES BANK STRATA
#from FIGURE 1 in "history of bottom trawl" documnet from Chris, we see that GB has code that starts with 01 and ends with 0. The 2 numbers between are for the strata number
#from FIGURE 5 in "Stock Assessment of Georges Bank Yellowtail Flounder for 2017" by Christopher M. Legault, we see GB has strata numbers 13-22
#so thats 01130, 01140,...,01220
#after plotting it seems like it goes to 25 so I added 3 more strata

GB_strata_num <- c("01130","01140","01150","01160","01170","01180","01190","01200","01210","01220","01230","01240","01250","01290", "01300")
#pull out indices corresponding to GB strata
GB_strata_idx <- match(GB_strata_num,strata.areas@data[["STRATUMA"]])
#plot them
plot(strata.areas[GB_strata_idx,])
#define GB strata as own object
GB_strata <- strata.areas[GB_strata_idx,]

#can create single outter layer to clip with
GB_strata_singlePoly <- unionSpatialPolygons(GB_strata,GB_strata@data$SET_)


#GULF OF MAINE STRATA
#from stockeff documentation on white hake
#https://nova.nefsc.noaa.gov/ADIOS_dev2/pub/stockeff/index.php?product=survey_information&module=survey&species_itis=164732&stock_abbrev=UNIT&sex_type=NONE

# GOM_strata_num <- c(	"01210", "01220", "01230", "01240", "01250", "01260", "01270", "01280", "01290", "01300", "01360", "01370", "01380", "01390", "01400")
# #pull out indices 
# GOM_strata_idx <- match(GOM_strata_num,strata.areas@data[["STRATUMA"]])
# #plot them
# plot(strata.areas[GOM_strata_idx,])
# #define GB strata as own object
# GOM_strata <- strata.areas[GOM_strata_idx,]
# 
# 
# 
# #MID ATLANTIC. LOTS OF DIFFERENCES HERE. I USED BLACK SEA BASS WINTER SURVEY FOR THESE
# #https://nova.nefsc.noaa.gov/ADIOS_dev2/pub/stockeff/index.php?product=survey_information&module=survey&species_itis=167687&stock_abbrev=UNIT&sex_type=NONE
# MA_strata_num <- c("01010", "01020", "01030", "01040", "01050", "01060", "01070", "01100", "01110", "01140", "01610", "01620", "01630", "01640", "01650", "01660", 
#                    "01670", "01680", "01690", "01700", "01710", "01720", "01730", "01740", "01750", "01760", "03020", "03030", "03040", "03050", "03060", "03070", 
#                    "03080", "03090", "03100", "03110", "03120", "03130", "03140", "03150", "03160", "03170", "03180", "03190", "03200", "03210", "03220", "03230", 
#                    "03240", "03250", "03260", "03270", "03280", "03290", "03300", "03310", "03320", "03330", "03340", "03350", "03360", "03370", "03380", "03390", 
#                    "03400", "03410", "03420", "03430", "03440", "03450", "03460")
# #pull out indices 
# MA_strata_idx <- match(MA_strata_num,strata.areas@data[["STRATUMA"]])
# #remove NAs
# MA_strata_idx <- MA_strata_idx[!is.na(MA_strata_idx)]
# #plot them
# plot(strata.areas[MA_strata_idx,])
# #define GB strata as own object
# MA_strata <- strata.areas[MA_strata_idx,]


# method from robyn based on survey observations
# #### Grid location####
# # Can also define specific lat and lon coordinates that you want to map instead of based on survey
# start_x <- min(Catch_Data$Longitude)
# end_x <- max(Catch_Data$Longitude)
# start_y <-  min(Catch_Data$Latitude)
# end_y <- max(Catch_Data$Latitude)
# my_mesh=expand.grid(seq(start_x, end_x, by=0.01), seq(start_y, end_y, by=0.01)) #making a grid of every lat/lon combination to the 0.01 degree, can chose alternative resolution, but I wouldn't recommend any finer-scale
# coordinates(my_mesh) <- ~Var1 + Var2
# grid_data <-as.data.frame(my_mesh@coords)
# colnames(grid_data) <-c("lon", "lat")
# write.csv(grid_data, file="TestScripts/FVCOM/grid_data.csv")


#MAY WANT TO FIX START_X END_X ETC BASED ON BOUNDING BOX OF GIVEN STRATA WE WANT TO MODEL IN (EX GEORGES BANK, GULF OF MAIN)

str <- GB_strata#choose strata to use

#### Grid location#### 
# Can also define specific lat and lon coordinates that you want to map instead of based on survey
start_x <- bbox(str)[[1]]
end_x <- bbox(str)[[3]]
start_y <- bbox(str)[[2]]
end_y <- bbox(str)[[4]]
my_mesh=expand.grid(seq(start_x, end_x, by=0.01), seq(start_y, end_y, by=0.01)) #making a grid of every lat/lon combination to the 0.01 degree, can chose alternative resolution, but I wouldn't recommend any finer-scale
coordinates(my_mesh) <- ~Var1 + Var2
grid_data <-as.data.frame(my_mesh@coords)
colnames(grid_data) <-c("lon", "lat")
write.csv(grid_data, file="TestScripts/FVCOM_GB/grid_data.csv")


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
save(FVcom_depth.list, file="TestScripts/FVCOM_GB/FVcom_depth.list.RData")

grid_data=as.data.frame(grid_data)
FVdepth<-cbind(grid_data,FVcom_depth.list[[1]])
names(FVdepth)[3]<-"AvgDepth"
write.csv(FVdepth, file="TestScripts/FVCOM_GB/FVdepth.csv")
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
save(temperature_fvcom_data, file="TestScripts/FVCOM_GB/temperature_data.RData")

#load("TestScripts/FVCOM/temperature_data.RData")

#all years to collect
yrs <- unique(dates$y2)
sns <- c("FALL","SPRING")

#INSTEAD OF LISTING EACH YEAR & SEASON, LOOP THROUGH UNIQUE YEARS & SEASONS

TRD_all <- list()

#for(sn in sns){
  for(yr in yrs){
    
    # #define object as ex TRD_FA09 
    # assign(paste("TRD_",sn,yr,sep = "") ,list())
    
    temp_lst <- list()
    
    #pull out year and season you want samples for
    sub_set <- subset(dates,(y2==yr), select=date:index )
    
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
    
    TRD_all[[paste("TRD_",yr,sep = "")]] <- temp_lst
  }
  
#}
save(TRD_all, file=paste("TestScripts/FVCOM_GB/TRD_all.RData",sep = ""))


#load previously saved data

# load(paste("TestScripts/FVCOM/TRD_all.RData",sep = ""))







# #NOT CALCULATING ANY MEAN RIGHT NOW
# ### Average Temp Data For each year and season## 
# Mean_Temp_all <- list()
# for(sn in sns){
#   for(yr in yrs){
#     
#     assign(paste("MeanTemp",sn,yr,sep = ""), as.data.frame(list(Reduce(`+`, TRD_all[[paste("TRD_",sn,yr,sep = "")]]) / length(TRD_all[[paste("TRD_",sn,yr,sep = "")]]))))
#     
#     Mean_Temp_all[[paste(sn,yr,sep = "")]] <- as.data.frame(list(Reduce(`+`, TRD_all[[paste("TRD_",sn,yr,sep = "")]]) / length(TRD_all[[paste("TRD_",sn,yr,sep = "")]])))
#     
#     
#     write.csv(Mean_Temp_all[[paste(sn,yr,sep = "")]], file=paste("TestScripts/FVCOM/MeanTemp",sn,yr,".csv",sep = ""))
#   }
# }
# 
# save(Mean_Temp_all, file=paste("TestScripts/FVCOM/Mean_Temp_all.RData",sep = ""))

# #load previously saved data
# for(sn in sns){
#   for(yr in yrs){
#     assign(paste("MeanTemp",sn,yr,sep = ""),   read.csv(file=paste("TestScripts/FVCOM/MeanTemp",sn,yr,".csv",sep = "")))
#   
#   }
# }



#CREATE WEEKLY AVERAGE FROM DAILY VALUES

#first, put them into weekly lists
week_lst <- list()
week_lst_yr <-list()

for(yr in seq(length(TRD_all))){

#  print(yr)
 idx<-1
   for(day in seq(1,358,7)){
    week_lst[[idx]] <- TRD_all[[yr]][c(day:(day+6))]
     idx<- idx+1  
   }
  week_lst_yr[[yr]] <- week_lst
  
}

weekly_temp <- list()
weekly_temp_year <- list()
#then average each list
idx<-1
for(yr in yrs){
  for(wk in seq(52)){
  
      weekly_temp[[wk]] <- Reduce('+',week_lst_yr[[idx]][[wk]])/length(week_lst_yr[[idx]][[wk]])
    
  }
  
  weekly_temp_year[[yr]]<-weekly_temp
  idx<-idx+1#increasing year
  
}


#weekly_temp_year is a list with each year with weekly temp 


#load previously obtained depth data
#FVdepth <- read.csv(file="TestScripts/FVCOM_GB/FVdepth.csv")

#### Go through and link each temperature file with the depth data to get 
#lat/long for tempdata
ts_wk <- list()
ts_all <- list()

#for(sn in sns){
for(yr in yrs){

  print(yr)
  for(wk in seq(52)){
  #  print(day)
  temp <- data.frame(FVdepth)
  temp$BTemp = as.data.frame(weekly_temp_year[[yr]][[wk]])
  
  
  #assign(paste("ts",yr,day,sep = ""), temp)
  ts_wk[[wk]] <- temp
  
  
  #write.csv(temp,file=paste("TestScripts/FVCOM/Temp",yr,day,".csv",sep = ""))
  }
  ts_all[[yr]] <- ts_wk
}
#}
saveRDS(ts_all,file="TestScripts/FVCOM_GB/WeeklyTemp.RDS")

#load previously saved
#ts_all <- readRDS(file="TestScripts/FVCOM_GB/WeeklyTemp.RDS")



#turn each ts_all weekly file into a raster for temperature and a single depth one as well

#depth for georges bank          
temp<-rasterFromXYZ(ts_all[[1]][[1]][,c(1,2,3)])  #lat,lon,depth

# cropping to desired area here
#depth_GB <- raster::mask(temp ,GB_strata_singlePoly) 

#alter resolution. tested some options for GB so that about 10000 cells are nonzero
depth_GB <- raster::aggregate(temp,fact=2) 



#if dont want to mask to polygon area use
#crop(temp,GB_strata)

saveRDS(depth_GB,file="TestScripts/FVCOM_GB/depth_GB.RDS")



WeeklyTempRasters <-list()
WeeklyTempRasters_ByYR <-list()
for(yr in yrs){
for(wk in seq(52)){
  
  
  #change into raster
  temp <- rasterFromXYZ(ts_all[[yr]][[wk]][,c(1,2,4)]) #lat,lon,temp
  
  #alter resolution. tested some options for GB so that about 10000 cells are nonzero
  temp <- raster::aggregate(temp,fact=2) 
  
  # cropping to desired area here ex, GB_strata 
  WeeklyTempRasters[[wk]] <- raster::mask(temp ,GB_strata_singlePoly) 

  #if dont want to mask to polygon area use
  #crop(temp,GB_strata)
  
  }
  WeeklyTempRasters_ByYR[[yr]] <-WeeklyTempRasters
}

saveRDS(WeeklyTempRasters_ByYR,file="TestScripts/FVCOM_GB/YearlyTemp_GB.RDS")









