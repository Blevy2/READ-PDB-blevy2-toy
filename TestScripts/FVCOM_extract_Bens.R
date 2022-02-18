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
dates_fall <- format(c(#seq(as.Date("9/6/2000", "%m/%d/%Y"), by=1, len=24+21), #date range from stations in fall 2000
  #seq(as.Date("9/5/2001", "%m/%d/%Y"), by=1, len=25+23), #9/5 to 10/22
  #seq(as.Date("9/4/2002", "%m/%d/%Y"), by=1, len=26+26), #9/4 to 10/25 etc
  #seq(as.Date("9/7/2003", "%m/%d/%Y"), by=1, len=23+32),
  #seq(as.Date("9/10/2004", "%m/%d/%Y"), by=1, len=20+28),
  #seq(as.Date("9/7/2005", "%m/%d/%Y"), by=1, len=23+31+5),
  #seq(as.Date("9/6/2006", "%m/%d/%Y"), by=1, len=24+26),
  #seq(as.Date("9/5/2007", "%m/%d/%Y"), by=1, len=25+32),
  #seq(as.Date("9/3/2008", "%m/%d/%Y"), by=1, len=27+31+9),
  seq(as.Date("9/13/2009", "%m/%d/%Y"), by=1, len=17+31+19),
  seq(as.Date("9/9/2010", "%m/%d/%Y"), by=1, len=21+31+30+3),
  seq(as.Date("9/10/2011", "%m/%d/%Y"), by=1, len=20+31+15),
  seq(as.Date("9/7/2012", "%m/%d/%Y"), by=1, len=23+31+11),
  seq(as.Date("9/6/2013", "%m/%d/%Y"), by=1, len=24+31+20),
  seq(as.Date("9/10/2014", "%m/%d/%Y"), by=1, len=20+31+13),
  seq(as.Date("9/2/2015", "%m/%d/%Y"), by=1, len=28+31+6)), format="%m/%d/%Y")

dates_fall <- as.data.frame(dates_fall)
colnames(dates_fall) <- "date"

dates_fall$Season <- rep("FALL",length(dates_fall[,1])) #add season label


dates_spring <- format(c(seq(as.Date("2/28/2009", "%m/%d/%Y"), by=1, len=1+31+30+8),
                    seq(as.Date("2/28/2010", "%m/%d/%Y"), by=1, len=1+31+30+1),
                    seq(as.Date("3/3/2011", "%m/%d/%Y"), by=1, len=28+30+11),
                    seq(as.Date("2/29/2012", "%m/%d/%Y"), by=1, len=1+31+30+3),
                    seq(as.Date("3/15/2013", "%m/%d/%Y"), by=1, len=16+30+9),
                    seq(as.Date("3/31/2014", "%m/%d/%Y"), by=1, len=1+30+31),
                    seq(as.Date("3/14/2015", "%m/%d/%Y"), by=1, len=17+30+7)), format="%m/%d/%Y") 

dates_spring <- as.data.frame(dates_spring)
colnames(dates_spring) <- "date"

dates_spring$Season <- rep("SPRING",length(dates_spring[,1]))

dates <- as.data.frame(rbind(dates_fall,dates_spring))






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

GB_strata_num <- c("01130","01140","01150","01160","01170","01180","01190","01200","01210","01220","01230","01240","01250")
#pull out indices corresponding to GB strata
GB_strata_idx <- match(GB_strata_num,strata.areas@data[["STRATUMA"]])
#plot them
plot(strata.areas[GB_strata_idx,])
#define GB strata as own object
GB_strata <- strata.areas[GB_strata_idx,]


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
    
    TRD_all[[paste("TRD_",sn,yr,sep = "")]] <- temp_lst
  }
  
}
save(TRD_all, file=paste("TestScripts/FVCOM/TRD_all.RData",sep = ""))


#load previously saved data

# load(paste("TestScripts/FVCOM/TRD_all.RData",sep = ""))


### Average Temp Data For each year and season## 
Mean_Temp_all <- list()
for(sn in sns){
  for(yr in yrs){
    
    assign(paste("MeanTemp",sn,yr,sep = ""), as.data.frame(list(Reduce(`+`, TRD_all[[paste("TRD_",sn,yr,sep = "")]]) / length(TRD_all[[paste("TRD_",sn,yr,sep = "")]]))))
    
    Mean_Temp_all[[paste(sn,yr,sep = "")]] <- as.data.frame(list(Reduce(`+`, TRD_all[[paste("TRD_",sn,yr,sep = "")]]) / length(TRD_all[[paste("TRD_",sn,yr,sep = "")]])))
    
    
    write.csv(Mean_Temp_all[[paste(sn,yr,sep = "")]], file=paste("TestScripts/FVCOM/MeanTemp",sn,yr,".csv",sep = ""))
  }
}

save(Mean_Temp_all, file=paste("TestScripts/FVCOM/Mean_Temp_all.RData",sep = ""))

# #load previously saved data
# for(sn in sns){
#   for(yr in yrs){
#     assign(paste("MeanTemp",sn,yr,sep = ""),   read.csv(file=paste("TestScripts/FVCOM/MeanTemp",sn,yr,".csv",sep = "")))
#   
#   }
# }
















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

#load("TestScripts/FVCOM/salinity_data.RData")



#INSTEAD OF LISTING EACH YEAR & SEASON, LOOP THROUGH UNIQUE YEARS & SEASONS

SRD_all <- list()

for(sn in sns){
  for(yr in yrs){
    
    
    temp_lst <- list()
    
    #pull out year and season you want samples for
    sub_set <- subset(dates,(y2==yr) & (Season == sn), select=date:index )
    
    ind <- sub_set$index #pulling indices 
    
    print("max ind is")
    print(max(ind))
    
    lst_idx <- 1
    
    for(i in ind){ #go down the list
      print(i)
      
      temp_data <- as.data.frame(salinity_fvcom_data[[i]])
      rast_col <- ceiling((range(temp_data$lon)[2]-range(temp_data$lon)[1])/0.01)# Make sure this matches your resolution
      rast_row <- ceiling((range(temp_data$lat)[2]-range(temp_data$lat)[1])/0.01)
      akima.smooth <- with(temp_data, interp(lon, lat, salinity, nx=rast_col, ny=rast_row))
      rast <- raster(akima.smooth)
      temp_lst[[lst_idx]] <- raster::extract(rast, grid_data)
      
      lst_idx <- lst_idx + 1
      
    }
    
    SRD_all[[paste("SRD_",sn,yr,sep = "")]] <- temp_lst
  }
  
}
save(SRD_all, file=paste("TestScripts/FVCOM/SRD_all.RData",sep = ""))


Mean_Sal_all <- list()
### Average Salinity Data For each year and season## 
for(sn in sns){
  for(yr in yrs){
    
    assign(paste("MeanSalinity",sn,yr,sep = ""), as.data.frame(list(Reduce(`+`, SRD_all[[paste("SRD_",sn,yr,sep = "")]]) / length(SRD_all[[paste("SRD_",sn,yr,sep = "")]]))))
    
    Mean_Sal_all[paste(sn,yr,sep = "")] <- as.data.frame(list(Reduce(`+`, SRD_all[[paste("SRD_",sn,yr,sep = "")]]) / length(SRD_all[[paste("SRD_",sn,yr,sep = "")]])))
    
    
    write.csv(Mean_Sal_all[[paste(sn,yr,sep = "")]], file=paste("TestScripts/FVCOM/MeanSalinity",sn,yr,".csv",sep = ""))
  }
}



# #load previously saved data
# for(sn in sns){
#   for(yr in yrs){
#     assign(paste("MeanSalinity",sn,yr,".csv",sep = ""),   read.csv(file=paste("TestScripts/FVCOM/MeanSalinity",sn,yr,".csv",sep = "")))
#   
#   }
# }





#### All FVCOM Files through 2015 as a loop###
ts_all <- list()
for(sn in sns){
  for(yr in yrs){
    temp <- data.frame(FVdepth)
    temp$BTemp = as.data.frame(Mean_Temp_all[paste(sn,yr,sep = "")])[,c(1)]
    temp$Salinity = as.data.frame(Mean_Sal_all[paste(sn,yr,sep = "")])[,c(1)]
    
    assign(paste("ts",sn,yr,sep = ""), temp)
    
    ts_all[[paste(sn,yr,sep = "")]] <- temp
    
    write.csv(temp,file=paste("TestScripts/FVCOM/ts",sn,yr,".csv",sep = ""))
  }
}
























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
save(FVcom_depth2016.list, file="TestScripts/FVCOM/FVcom_depth2016.list.RData")

FVdepth2016<-cbind(grid_data@coords,FVcom_depth2016.list[[1]])
names(FVdepth2016)[3]<-"AvgDepth"
save(FVdepth2016, file="TestScripts/FVCOM/FVdepth2016.RData")

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
save(FVcom_depth2017.list, file="TestScripts/FVCOM/FVcom_depth2017.list.RData")

FVdepth2017<-cbind(grid_data@coords,FVcom_depth2017.list[[1]])
names(FVdepth2017)[3]<-"AvgDepth"
save(FVdepth2017, file="TestScripts/FVCOM/FVdepth2017.RData")


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
save(sepfvcomtemp2016, file="TestScripts/FVCOM/sepfvcomtemp2016.RData")

# Oct 2016
octfvcomtemp2016<-list()
for (i in 1:31){ #1:30 because there are 30 days in the month of April
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201610.nc.ascii?temp[", time_id[i+30], ":1:", time_id[i+30], "][44:1:44][0:1:48450]", sep="")) # time_id[i+30] because there are 30 days of data before this
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  octfvcomtemp2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(octfvcomtemp2016[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(octfvcomtemp2016, file="TestScripts/FVCOM/octfvcomtemp2016.RData")


# Oct 2017
octfvcomtemp2017<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201710.nc.ascii?temp[", time_id[i+61], ":1:", time_id[i+61], "][44:1:44][0:1:136431]", sep="")) #61 days of data before this
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  octfvcomtemp2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(octfvcomtemp2017[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(octfvcomtemp2017, file="TestScripts/FVCOM/octfvcomtemp2017.RData")

# Nov 2017
novfvcomtemp2017<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201711.nc.ascii?temp[", time_id[i+92], ":1:", time_id[i+92], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  novfvcomtemp2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(novfvcomtemp2017[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(novfvcomtemp2017, file="TestScripts/FVCOM/novfvcomtemp2017.RData")

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
save(TRD_FL16, file="TestScripts/FVCOM/TRD_FL16.RData")

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
save(TRD_FL17, file="TestScripts/FVCOM/TRD_FL17.RData")

##Taking average temps ##
MeanTempfl16<- list(Reduce(`+`, TRD_FL16) / length(TRD_FL16))
MeanTempfl17<- list(Reduce(`+`, TRD_FL17) / length(TRD_FL17))

write.csv(MeanTempfl16, file="TestScripts/FVCOM/MeanTempfl16.csv")
write.csv(MeanTempfl17, file="TestScripts/FVCOM/MeanTempfl17.csv")


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
save(sepfvcomsal2016, file="TestScripts/FVCOM/sepfvcomsal2016.RData")

octfvcomsal2016<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201610.nc.ascii?salinity[", time_id[i+30], ":1:", time_id[i+30], "][44:1:44][0:1:48450]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  octfvcomsal2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(octfvcomsal2016[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(octfvcomsal2016, file="TestScripts/FVCOM/octfvcomsal2016.RData")

# Oct 2017
octfvcomsal2017<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201710.nc.ascii?salinity[", time_id[i+61], ":1:", time_id[i+61], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  octfvcomsal2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(octfvcomsal2017[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(octfvcomsal2017, file="TestScripts/FVCOM/octfvcomsal2017.RData")

# Nov 2017
novfvcomsal2017<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201711.nc.ascii?salinity[", time_id[i+92], ":1:", time_id[i+92], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  novfvcomsal2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(novfvcomsal2017[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(novfvcomsal2017, file="TestScripts/FVCOM/novfvcomsal2017.RData")

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
save(SRD_FL16, file="TestScripts/FVCOM/SRD_FL16.RData")

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
save(SRD_FL17, file="TestScripts/FVCOM/SRD_FL17.RData")

MeanSalfl16<- list(Reduce(`+`, SRD_FL16) / length(SRD_FL16))
MeanSalfl17<- list(Reduce(`+`, SRD_FL17) / length(SRD_FL17))

write.csv(MeanSalfl16, file="TestScripts/FVCOM/MeanSalfl16.csv")
write.csv(MeanSalfl17, file="TestScripts/FVCOM/MeanSalfl17.csv")

ts2016= data.frame(FVdepth2016)
ts2016$BTemp=MeanTempfl16[,c(2)]
ts2016$Salinity=MeanSalfl16[,c(2)]
#Add your sediment column
write.csv(ts2016, file="TestScripts/FVCOM/ts2016.csv")

ts2017= data.frame(FVdepth2017)
ts2017$BTemp=MeanTempfl17[,c(2)]
ts2017$Salinity=MeanSalfl17[,c(2)]
write.csv(ts2017, file="TestScripts/FVCOM/ts2017.csv")


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
save(aprilfvcomtemp2016, file="TestScripts/FVCOM/aprilfvcomtemp2016.RData")

# May 2016
mayfvcomtemp2016<-list()
for (i in 1:31){ #1:30 because there are 30 days in the month of April
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201605.nc.ascii?temp[", time_id[i+30], ":1:", time_id[i+30], "][44:1:44][0:1:48450]", sep="")) 
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  mayfvcomtemp2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(mayfvcomtemp2016[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(mayfvcomtemp2016, file="TestScripts/FVCOM/mayfvcomtemp2016.RData")

# March 2017
marchfvcomtemp2017<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201703.nc.ascii?temp[", time_id[i+61], ":1:", time_id[i+61], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  marchfvcomtemp2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(marchfvcomtemp2017[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(marchfvcomtemp2017, file="TestScripts/FVCOM/marchfvcomtemp2017.RData")

# April 2017
aprilfvcomtemp2017<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201704.nc.ascii?temp[", time_id[i+91], ":1:", time_id[i+91], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  aprilfvcomtemp2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(aprilfvcomtemp2017[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(aprilfvcomtemp2017, file="TestScripts/FVCOM/aprilfvcomtemp2017.RData")

# May 2017
mayfvcomtemp2017<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201705.nc.ascii?temp[", time_id[i+122], ":1:", time_id[i+122], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  mayfvcomtemp2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(mayfvcomtemp2017[[i]]) <- c("lon", "lat", "temperature")
  print(i)
}
save(mayfvcomtemp2017, file="TestScripts/FVCOM/mayfvcomtemp2017.RData")

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
save(TRD_SP16, file="TestScripts/FVCOM/TRD_SP16.RData")


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
save(TRD_SP17, file="TestScripts/FVCOM/TRD_SP17.RData")


###taking averages of each day during the season at each location

MeanTempsp16<- list(Reduce(`+`, TRD_SP16) / length(TRD_SP16)) 
MeanTempsp17<- list(Reduce(`+`, TRD_SP17) / length(TRD_SP17)) 

write.csv(MeanTempsp16, file="TestScripts/FVCOM/MeanTempsp16.csv")
write.csv(MeanTempsp17, file="TestScripts/FVCOM/MeanTempsp17.csv")

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
save(aprilfvcomsal2016, file="TestScripts/FVCOM/aprilfvcomsal2016.RData")

# May 2016
mayfvcomsal2016<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom3_201605.nc.ascii?salinity[", time_id[i+30], ":1:", time_id[i+30], "][44:1:44][0:1:48450]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  mayfvcomsal2016[[i]] <- cbind(longitude2016, latitude2016, temp_data)
  colnames(mayfvcomsal2016[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(mayfvcomsal2016, file="TestScripts/FVCOM/mayfvcomsal2016.RData")


# March 2017
marchfvcomsal2017<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201703.nc.ascii?salinity[", time_id[i+61], ":1:", time_id[i+61], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  marchfvcomsal2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(marchfvcomsal2017[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(marchfvcomsal2017, file="TestScripts/FVCOM/marchfvcomsal2017.RData")

# April 2017
aprilfvcomsal2017<-list()
for (i in 1:30){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201704.nc.ascii?salinity[", time_id[i+92], ":1:", time_id[i+92], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  aprilfvcomsal2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(aprilfvcomsal2017[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(aprilfvcomsal2017, file="TestScripts/FVCOM/aprilfvcomsal2017.RData")

# May 2017
mayfvcomsal2017<-list()
for (i in 1:31){
  temp_data <- read.csv(paste("http://www.smast.umassd.edu:8080/thredds/dodsC/models/fvcom/NECOFS/Archive/Seaplan_33_Hindcast_v1/gom5_201705.nc.ascii?salinity[", time_id[i+122], ":1:", time_id[i+122], "][44:1:44][0:1:136431]", sep=""))
  temp_data <- as.numeric(as.character(temp_data[(6: nrow(temp_data)),1]))
  mayfvcomsal2017[[i]] <- cbind(longitude2017, latitude2017, temp_data)
  colnames(mayfvcomsal2017[[i]]) <- c("lon", "lat", "salinity")
  print(i)
}
save(mayfvcomsal2017, file="TestScripts/FVCOM/mayfvcomsal2017.RData")

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
save(SRD_SP16, file="TestScripts/FVCOM/SRD_SP16.RData")


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
save(SRD_SP17, file="TestScripts/FVCOM/SRD_SP17.RData")

MeanSalsp16<- list(Reduce(`+`, SRD_SP16) / length(SRD_SP16))
MeanSalsp17<- list(Reduce(`+`, SRD_SP17) / length(SRD_SP17)) 

write.csv(MeanSalsp16, file="TestScripts/FVCOM/MeanSalsp16.csv")
write.csv(MeanSalsp17, file="TestScripts/FVCOM/MeanSalsp17.csv")

ts2016= data.frame(FVdepth2016)
ts2016$BTemp=MeanTempsp16[c(1)]
ts2016$Salinity=MeanSalsp16[c(1)]
# add sediment column
data.table::fwrite(ts2016, file="TestScripts/FVCOM/ts2016.csv")

ts2017= data.frame(FVdepth2017)
ts2017$BTemp=MeanTempsp17[1]
ts2017$Salinity=MeanSalsp17[1]
data.table::fwrite(ts2017, file="TestScripts/FVCOM/ts2017.csv")
