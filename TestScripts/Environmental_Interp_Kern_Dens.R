#Following from https://cran.r-project.org/web/packages/envi/vignettes/vignette.html






####################################
# looking for data to use
########################
# 
# test.dir <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\From_Alicia\\" # strata shape files in this directory
# library(rgdal)
# 
# #east coast outline
# test <- readOGR(paste(test.dir,"EastCoast_SmoothLines", sep="")) 
# 
# plot(test)
# 
# #continents
# test2 <- readOGR(paste(test.dir,"ne_10m_land", sep=""))
# plot(test2)
# 
# 
# test3 <- readOGR(paste(test.dir,"nw_10m_bathymetry_L_0", sep=""))
# plot(test3)




#noaa directory
noaa.dir <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\NOAA_Maps\\" 

#Bathymetry data (depth)
Bathy <- readOGR(paste(noaa.dir,"Bathy_Poly_Clip", sep="")) 
par(mfrow = c(1,1),mar = c(1, 1, 1, 1))
plot(Bathy)

#floortemp data
FloorTemp <- readOGR(paste(noaa.dir,"Seafloor_Temp_Poly_Clip", sep="")) 
par(mfrow = c(1,1),mar = c(1, 1, 1, 1))
plot(FloorTemp)

#Lithology data
Sediment <- readOGR(paste(noaa.dir,"Atlantic_seafloor_sediment", sep="")) 
par(mfrow = c(1,1),mar = c(1, 1, 1, 1))
plot(Sediment)




###################################
# following from online
###################################

# trying to use presence and absence data along with 2 covariates (catch weight and bottom temp)
loadedPackages <- c("envi", "raster", "RStoolbox", "spatstat.data", "spatstat.geom", "spatstat.core")
invisible(lapply(loadedPackages, library, character.only = TRUE))
set.seed(1234) # for reproducibility


View(spatstat.data::gorillas.extra)

#two covariate datasets (rasters I think)
slopeangle <- spatstat.data::gorillas.extra$slopeangle
waterdist <- spatstat.data::gorillas.extra$waterdist

plot(spatstat.data::gorillas.extra$waterdist)


#Center and scale the covariate data.

slopeangle$v <- scale(slopeangle)
waterdist$v <- scale(waterdist)


#Convert the covariate data to class RasterLayer.

slopeangle_raster <- raster::raster(slopeangle)
waterdist_raster <- raster::raster(waterdist)
plot(slopeangle_raster)





#Check out the point data gorillas
View(spatstat.data::gorillas)
plot(spatstat.data::gorillas) #contains group, season and date attributs
class(spatstat.data::gorillas) #ppp data



#Add appropriate marks to the gorillas data from spatstat.data package. 
#These points are considered our "presence" locations.

presence <- spatstat.geom::unmark(spatstat.data::gorillas)  #unmark removes existing attributes (aka marks)
spatstat.geom::marks(presence) <- data.frame("presence" = rep(1, presence$n), #adds attributes back (aka marks)
                                             "lon" = presence$x,  #first repeat the nuimber 1 n times, then add x and y coordinates for each
                                             "lat" = presence$y)
spatstat.geom::marks(presence)$slopeangle <- slopeangle[presence] #adds covariate values for presence locations
spatstat.geom::marks(presence)$waterdist <- waterdist[presence]




#Randomly draw points from the study area and add the appropriate marks. 
#These points are considered our "(pseudo-)absence" locations.
#I WILL HAVE REAL ABSENCE LOCATIONS
absence <- spatstat.core::rpoispp(0.00004, win = slopeangle)
spatstat.geom::marks(absence) <- data.frame("presence" = rep(0, absence$n),
                                            "lon" = absence$x,
                                            "lat" = absence$y)
spatstat.geom::marks(absence)$slopeangle <- slopeangle[absence]
spatstat.geom::marks(absence)$waterdist <- waterdist[absence]







#Combine the presence (n = 647) and absence (769) locations into one object of 
#class data.frame and reorder the features required for the lrren function in the envi package:
# 1.ID
# 2.X-coordinate
# 3.Y-coordinate
# 4.Presence (binary)
# 5.Covariate 1
# 6.Covariate 2

obs_locs <- spatstat.geom::superimpose(absence, presence, check = FALSE) #combine two datasets
spatstat.geom::marks(obs_locs)$presence <- as.factor(spatstat.geom::marks(obs_locs)$presence) #mark presence locations
spatstat.geom::plot.ppp(obs_locs,
                        which.marks = "presence",
                        main = "Gorilla nesting sites (red-colored)\nPseudo-absence locations (blue-colored)",
                        cols = c("#0000CD","#8B3A3A"),
                        pch = 1,
                        axes = TRUE,
                        ann = TRUE)
obs_locs <- spatstat.geom::marks(obs_locs) #extracts information so it is now a table rather than a list
obs_locs$id <- seq(1, nrow(obs_locs), 1)  #adds column for ID
obs_locs <- obs_locs[ , c(6, 2, 3, 1, 4, 5)] #reorders columns so they are in correct order (see order above)



#Extract the prediction locations within the study area from one of the covariates.

predict_locs <- data.frame(raster::rasterToPoints(slopeangle_raster))  #adds column called layer with slopeangle
predict_locs$layer2 <- raster::extract(waterdist_raster, predict_locs[, 1:2]) #adds column called layer2 with waterdist




#Run the lrren function within the envi package. 
#We use the default settings except we want to predict the ecological niche within 
#the study area (predict = TRUE), we conduct k-fold cross-validation model fit diagnostics 
#(cv = TRUE) by undersampling absence locations to balance the prevalence (0.5) within all 
#testing data sets (balance = TRUE).

start_time <- Sys.time() # record start time
test_lrren <- lrren(obs_locs = obs_locs,
                    predict_locs = predict_locs,
                    predict = TRUE,
                    cv = TRUE,
                    balance = TRUE)
end_time <- Sys.time() # record end time
lrren_time <- end_time - start_time # calculate duration of lrren() example



#We display the estimated ecological niche within a space of Covariate 1 by Covariate 
# 2 using the plot_obs function. We use the default two-tailed alpha-level (alpha = 0.05)
# and the default colors where the yellow color denotes areas with covariate data combinations
# where we have sparse observations. As expected, extreme values of the log relative risk
# surface are located near the edges of the surface, however these areas are highly variable
# and are not statistically significant based on an asymptotic normal assumption. The default 
# color key for the log relative risk surface hides the heterogeneity closer to the null value 
# (zero). Therefore, we limit the color key for the log relative risk surface to (-1, 1).

plot_obs(test_lrren,
         lower_lrr = -1,
         upper_lrr = 1)





###########################################################
# Trying to recreate above with my data
###########################################################

loadedPackages <- c("rgdal", "data.table", "maptools","envi", "raster", "RStoolbox", "spatstat.data", "spatstat.geom", "spatstat.core")
invisible(lapply(loadedPackages, library, character.only = TRUE))



#load stratas for clipping etc
strata.dir <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\" # strata shape files in this directory
library(rgdal)
# get the shapefiles
strata.areas <- readOGR(paste(strata.dir,"Survey_strata", sep="")) #readShapePoly is deprecated; use rgdal::readOGR or sf::st_read 

#define georges bank
GB_strata_num <- c("01130","01140","01150","01160","01170","01180","01190","01200","01210","01220","01230","01240","01250")
#pull out indices corresponding to GB strata
GB_strata_idx <- match(GB_strata_num,strata.areas@data[["STRATUMA"]])
#plot them
#plot(strata.areas[GB_strata_idx,])
#define GB strata as own object
GB_strata <- strata.areas[GB_strata_idx,]

#can create single outter layer to clip with
GB_strata_singlePoly <- unionSpatialPolygons(GB_strata,GB_strata@data$SET_)



#noaa directory
noaa.dir <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\NOAA_Maps\\" 

#Bathymetry data (depth)
Bathy <- readOGR(paste(noaa.dir,"Bathy_Poly_Clip", sep="")) 
par(mfrow = c(1,1),mar = c(1, 1, 1, 1))
plot(Bathy)

#floortemp data
FloorTemp <- readOGR(paste(noaa.dir,"Seafloor_Temp_Poly_Clip", sep="")) 
par(mfrow = c(1,1),mar = c(1, 1, 1, 1))
plot(FloorTemp)

#Lithology (sediment) data
Sediment_poly <- readOGR(paste(noaa.dir,"Atlantic_seafloor_sediment", sep="")) 
par(mfrow = c(1,1),mar = c(1, 1, 1, 1))
plot(Sediment_poly)





#ABOVE ARE POLYGONS. I CHANGED TO RASTERS IN ARCMAP AND LOAD THEM INSTEAD

  #BATHYMETRY   (OLD)
#bathy_ras <- raster('C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\NOAA_Maps\\Bathy_raster\\Seafloor_Bathymetry_Clip1_Pr21.tif')
#plot(bathy_ras)

  #SEDIMENT   (OLD)
#categorical data (will this work?)
#sediment_ras_categ <-  raster('C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\NOAA_Maps\\Atlantic_seafloor_sediment\\raster_categorical\\Sedim_ras_categ.tif')
#plot(sediment_ras_categ)

#numerical data   (OLD)
#sediment_ras_num <-  raster('C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\NOAA_Maps\\Atlantic_seafloor_sediment\\raster_number\\sediment_ras_num.tif')
#plot(sediment_ras_num)

#SEDIMENT THICKNESS   (OLD)
#sediment_thick_ras <-  raster('C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\NOAA_Maps\\GlobalSedimentThickness\\Final_ras\\sed_thick_ras.tif')
#plot(sediment_thick_ras)

#Median sediment size (from Robyns USGS link)
#extrapolated from points using natural neighbor interpolation method
median_sed_thick_NN <-  raster('C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\NOAA_Maps\\Median_Sediment_Size_(ecstdb2014)\\med_sdsze_NaturalNeighbor\\Med_SdSze_NN')
plot(median_sed_thick_NN)
#extrapolated from points using IDW interpolation method
median_sed_thick_IDW <-  raster('C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\NOAA_Maps\\Median_Sediment_Size_(ecstdb2014)\\med_sdsze_IDW_Method\\Med_SdSze_IDW')
plot(median_sed_thick_IDW)

#DEPTH FROM FVCOM DATA
depth_GB_ras <- readRDS(file="TestScripts/FVCOM_GB/depth_GB.RDS")
plot(depth_GB_ras)






#make resolutions match (dont need this?)
#extent(median_sed_thick_IDW) <- extent(depth_GB_ras)
#test <- projectRaster(median_sed_thick_IDW,raster(GB_strata))
#res(median_sed_thick_IDW) <- res(depth_GB_ras)
#depth_GB_ras@crs@projargs <- median_sed_thick_IDW@crs@projargs




#clip data to desired area using either
#1) crop (for full extent. will create rectangle)
#2) mask will cut to polygon

#bathy_ras <-crop(bathy_ras,GB_strata)
#sediment_ras_num <- crop(sediment_ras_num,GB_strata)
#sedmient_ras_categ <- crop(sediment_ras_categ,GB_strata)
#sediment_thick_ras <- crop(sediment_thick_ras,GB_strata)
median_sed_thick_NN <- raster::mask(median_sed_thick_NN,GB_strata_singlePoly)
median_sed_thick_IDW <- raster::mask(median_sed_thick_IDW,GB_strata_singlePoly)
depth_GB_ras <- raster::mask(depth_GB_ras,GB_strata_singlePoly)

#MAYBE DONT MASK TO AVOID ISSUES OF POINTS FALLING OUTSIDE RASTERS


# plot(bathy_ras)
# plot(GB_strata,add=T)
# 
# plot(sediment_ras_num)
# plot(GB_strata,add=T)
# 
# plot(sediment_thick_ras)
# plot(GB_strata,add=T)

plot(median_sed_thick_NN)
plot(GB_strata,add=T)

plot(median_sed_thick_IDW)
plot(GB_strata,add=T)

plot(depth_GB_ras)
plot(GB_strata,add=T)

#create image files to use later
#bathy_im<- as.im(bathy_ras)
#sediment_im <- as.im(sediment_ras_num) #CHANGE FROM CATEGORICAL TO NUMERICAL HERE
#sediment_thick_im <- as.im(sediment_thick_ras)
median_sed_thick_IDW_im <- as.im(median_sed_thick_IDW)
median_sed_thick_NN_im <- as.im(median_sed_thick_NN)
depth_GB_im <- as.im(depth_GB_ras)


#scale data
#bathy_im$v <- scale(bathy_im)
#sediment_im$v <- scale(sediment_im)
#sediment_thick_im$v <- scale(sediment_thick_im)
median_sed_thick_IDW_im$v <- scale(median_sed_thick_IDW_im)
median_sed_thick_NN_im$v <- scale(median_sed_thick_NN_im)
depth_GB_im$v <- scale(depth_GB_im)

#old (wrong?) way
#bathy_im_sc<- scale(as.im(bathy_ras))  #scaling here
#sediment_im_sc <- scale(as.im(sediment_ras_num)) #CHANGE FROM CATEGORICAL TO NUMERICAL HERE

#CHANGE BACK TO RASTER AFTER SCALING
#bathy_ras <- raster::raster(bathy_im)
#sediment_ras_num <- raster::raster(sediment_im)
#sediment_thick_ras <- raster::raster(sediment_thick_im)
median_sed_thick_IDW_ras <- raster::raster(median_sed_thick_IDW_im)
median_sed_thick_NN_ras <- raster::raster(median_sed_thick_NN_im)
depth_GB_ras <- raster::raster(depth_GB_im)




#blank data for some reason. Having trouble converting in arcmap
#seafloortemp_ras <- raster('C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\NOAA_Maps\\SeafloorTemp_raster\\Seafloor_Temperature_Clip1.tif')
#plot(seafloortemp_ras)


# #following https://gis.stackexchange.com/questions/265064/rasterize-polygons-with-r
# #to convert to rasters
# library(sf)
# r1 <- raster(as(Bathy, "Spatial"), ncols = 100, nrows =500 )
# bathy_ras <- rasterize(Bathy, r1,field=Bathy@data[["gridcode"]], getCover = TRUE, progress = "text")
# 
# #trying to create my own breaks for above but not working well
# Qbreaks <- classInt::classIntervals(var=as.matrix(bathy_ras), style = "fisher") 
# #remove zeros from breaks
# Qbreaks2 <- Qbreaks[["brks"]][!Qbreaks[["brks"]] %in% 0]
# Qbreaks2 <- append(0,Qbreaks2)#put single 0 back to start
# 
# 
# plot(bathy_ras,breaks=Qbreaks2)
# 
# 
# r2 <- raster(as(FloorTemp, "SpatialPolygonsDataFrame"), ncols = 100, nrows =500 )
# flrtemp_ras <- rasterize(as(FloorTemp,"SpatialPolygonsDataFrame"), r2, getCover = TRUE, progress = "text")
# plot(flrtemp_ras)
# 
# r3 <- raster(Sediment, ncols = 100, nrows =500 )
# sediment_ras <- rasterize(Sediment, r3, getCover = TRUE, progress = "text")
# plot(sediment_ras)






#read in point data and convert to ppp
gis.name <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\Plot_survey\\ADIOS_SV_172909_GBK_NONE_survey_dist_map_fixed.csv"

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




#setting up weighting following along from https://stackoverflow.com/questions/21273525/weight-equivalent-for-geom-density2d
#this replaces each prescence point with CatchWt number of points
library(data.table)
#weighted_data_spring <- data.table(spring.gis)[,list(Longitude=rep(Longitude,ceiling(CatchWt)),Latitude=rep(Latitude,ceiling(CatchWt)),CatchWt=rep(ceiling(CatchWt),ceiling(CatchWt)))]
weighted_data_all <- data.table(gis)[,list(Longitude=rep(Longitude,ceiling(CatchWt)),Latitude=rep(Latitude,ceiling(CatchWt)),CatchWt=rep(ceiling(CatchWt),ceiling(CatchWt)))]






# #removing points that fall outside one of our covariates
# HAVENT QUITE FIGURED THIS OUT YET
# # following from https://rspatial.org/raster/rosu/Chapter5.html
# p <- weighted_data_all[,1:2]
# win <- aggregate(GB_strata)
# owin <- as.owin(sp::as_Spatial(win))
# pp <- ppp(p[,1],p[,2],marks=, owin(c(-69.98, -65.68) ,c(40.09,42.45) ))
# 
# 
# library(rgeos)
# sp <- SpatialPoints(p,proj4string = CRS(proj4string((GB_strata))))
# i <- gIntersects(sp,GB_strata_singlePoly,byid=TRUE)
# which(!i)
# 
# 
# for(x in length(presence)){
#   for(y in length(presence$y)){
#     
#    if(median_sed_thick_IDW_im[x,y]){
#      print("TRUE")
#    } 
#     
#   }
# }








#convert data into planar point pattern (ppp). no weight option so use weighted/repeated points
#spring_points <- ppp(weighted_data_spring$Longitude,weighted_data_spring$Latitude,owin(c(-69.98, -65.68) ,c(40.09,42.45) ))

#weighted points (2,181 presence and only 704 absence locations)
all_points <- ppp(weighted_data_all$Longitude,weighted_data_all$Latitude,owin(c(-69.98, -65.68) ,c(40.09,42.45) ))

#unweighted points (516 presence and 704 absence)
temp <- data.table(gis)[CatchWt>0]
all_points <- ppp(temp$Longitude,temp$Latitude,owin(c(-69.98, -65.68) ,c(40.09,42.45) ))







#Add appropriate marks to the data from spatstat.data package. 
#These points are considered our "presence" locations.

presence <- spatstat.geom::unmark(all_points)  #unmark removes existing attributes (aka marks)
spatstat.geom::marks(presence) <- data.frame("presence" = rep(1, presence$n), #adds attributes back (aka marks)
                                             "lon" = presence$x,  #first repeat the nuimber 1 n times, then add x and y coordinates for each
                                             "lat" = presence$y)


#spatstat.geom::marks(presence)$bathy <-  bathy_im[presence] #adds covariate values for presence locations
#spatstat.geom::marks(presence)$sediment <- sediment_im[presence]
#spatstat.geom::marks(presence)$sediment_thick <- sediment_thick_im[presence]
spatstat.geom::marks(presence)$median_sed <- median_sed_thick_NN_im[presence]
spatstat.geom::marks(presence)$median_sed <- median_sed_thick_IDW_im[presence]
spatstat.geom::marks(presence)$depth <-  depth_GB_im[presence] #adds covariate values for presence locations






#DEFINE ABSENCE LOCATIONS

abs <-  data.table(gis)[(CatchWt==0)]  #pull out absence points

#remove duplicate locations because it leads to problems later
# NO IT DOESNT, THERE WERE POINTS OUTSIDE THE RASTER REGION
#abs <- abs[!duplicated(abs[,c("LONGITUDE","LATITUDE")]),]

absence <-  ppp(abs$Longitude,abs$Latitude,owin(c(-69.98, -65.68) ,c(40.09,42.45)))


spatstat.geom::marks(absence) <- data.frame("presence" = rep(0, absence$n),
                                            "lon" = absence$x,
                                            "lat" = absence$y)
#spatstat.geom::marks(absence)$bathy <-  bathy_im[absence] #some points are outside the region
#spatstat.geom::marks(absence)$ sediment <- sediment_im[absence]
#spatstat.geom::marks(absence)$ sediment_thick <- sediment_thick_im[absence]

spatstat.geom::marks(absence)$median_sed <- median_sed_thick_NN_im[absence]
spatstat.geom::marks(absence)$median_sed <- median_sed_thick_IDW_im[absence]
spatstat.geom::marks(absence)$depth <-  depth_GB_im[absence] #adds covariate values for presence locations







#Combine the presence and absence locations into one object of 
#class data.frame and reorder the features required for the lrren function in the envi package:
# 1.ID
# 2.X-coordinate
# 3.Y-coordinate
# 4.Presence (binary)
# 5.Covariate 1
# 6.Covariate 2

obs_locs <- spatstat.geom::superimpose(absence, presence, check = FALSE) #combine two datasets
spatstat.geom::marks(obs_locs)$presence <- as.factor(spatstat.geom::marks(obs_locs)$presence) #mark presence locations
spatstat.geom::plot.ppp(obs_locs,
                        which.marks = "presence",
                        main = "Fish Catch Sites (red-colored)\n Absence locations (blue-colored)",
                        cols = c("#0000CD","#8B3A3A"),
                        pch = 1,
                        axes = TRUE,
                        ann = TRUE)
obs_locs <- spatstat.geom::marks(obs_locs) #extracts information so it is now a table rather than a list
obs_locs$id <- seq(1, nrow(obs_locs), 1)  #adds column for ID
obs_locs <- obs_locs[ , c(6, 2, 3, 1, 4, 5)] #reorders columns so they are in correct order (see order above)





#Extract the prediction locations within the study area from one of the covariates.

predict_locs <- data.frame(raster::rasterToPoints(depth_GB_ras))  #adds column called layer with depth

#predict_locs <- data.frame(raster::rasterToPoints(bathy_ras))  #adds column called layer with bathymetry
#predict_locs$layer2 <- raster::extract(sediment_ras_num, predict_locs[, 1:2]) #adds column called layer2 with sediment number
#predict_locs$layer2 <- raster::extract(sediment_thick_ras, predict_locs[, 1:2]) #adds column called layer2 with sediment number
predict_locs$layer2 <- raster::extract(median_sed_thick_NN_ras, predict_locs[, 1:2]) #adds column called layer2 with sediment number
predict_locs$layer2 <- raster::extract(median_sed_thick_IDW_ras, predict_locs[, 1:2]) #adds column called layer2 with sediment number




#Run the lrren function within the envi package. 
#We use the default settings except we want to predict the ecological niche within 
#the study area (predict = TRUE), we conduct k-fold cross-validation model fit diagnostics 
#(cv = TRUE) by undersampling absence locations to balance the prevalence (0.5) within all 
#testing data sets (balance = TRUE).

start_time <- Sys.time() # record start time
fish_lrren <- lrren(obs_locs = obs_locs,
                    predict_locs = predict_locs,
                    predict = TRUE,
                    cv = TRUE,
                   # adapt=T,
                    #balance = TRUE)
                    #conserve = TRUE #Logical. If TRUE (the default), the ecological niche will be estimated within a concave hull around the locations in obs_locs. If FALSE, the ecological niche will be estimated within a concave hull around the locations in predict_locs.
)
end_time <- Sys.time() # record end time
lrren_time <- end_time - start_time # calculate duration of lrren() example



#We display the estimated ecological niche within a space of Covariate 1 by Covariate 
# 2 using the plot_obs function. We use the default two-tailed alpha-level (alpha = 0.05)
# and the default colors where the yellow color denotes areas with covariate data combinations
# where we have sparse observations. As expected, extreme values of the log relative risk
# surface are located near the edges of the surface, however these areas are highly variable
# and are not statistically significant based on an asymptotic normal assumption. The default 
# color key for the log relative risk surface hides the heterogeneity closer to the null value 
# (zero). Therefore, we limit the color key for the log relative risk surface to (-1, 1).

plot_obs(fish_lrren,
         lower_lrr = -1,
         upper_lrr = 1)





plot_predict(fish_lrren, cref0 = "EPSG:32632", cref1 = "EPSG:4326",
             lower_lrr = -1,
             upper_lrr = 1)








#Trying my own lrren function that uses adapt=T option
source("TestScripts/lrren_BENS.R")

fish_lrren <- lrren_Bens(obs_locs = obs_locs,
                    predict_locs = predict_locs,
                    predict = TRUE,
                    cv = FALSE, #if true get error foreach::foreach comb not defined
                    adapt=T,
                    #balance = TRUE)
                    #conserve = TRUE #Logical. If TRUE (the default), the ecological niche will be estimated within a concave hull around the locations in obs_locs. If FALSE, the ecological niche will be estimated within a concave hull around the locations in predict_locs.
)




#to source a plot_predict where I save output
#1: load file
source("TestScripts/plot_predict_BENS.R") #my edited version that saves output

#2: allow the function to call other hidden functions from envi
environment(plot_predict_BENS) <- asNamespace('envi')



p<-plot_predict_BENS(fish_lrren, cref0 = "EPSG:32632", cref1 = "EPSG:4326",
             lower_lrr = -1, #used to be -1 to 1
             upper_lrr = 1)

#choose one. both from plot_predict_BENS
p$out<-predict_risk_raster
p$out<-rrp$v
#rescale output from -1 to 1 to 0 to 1

#first exponentiate log values
vec <- exp(p$out@data@values)
range(vec,na.rm=T)

#maybe converts to correct interval?
#check https://www.statisticshowto.com/log-odds/
#vec <- vec/(1+vec)

#then convert to 0 to 1 range
vec_01 <-(vec - min(vec,na.rm=T)) / (max(vec,na.rm=T) - min(vec,na.rm=T)) 
range(vec_01,na.rm=T)


p$out$v@data@values <- vec_01

#MUCH OF THIS CODE IS FROM ENVI::DIV_PLOT
#rescale legend labels and colors

#scale to display
rbr <- max(vec_01,na.rm=T) - min(vec_01,na.rm=T)
rbt <- rbr / 4
rbs <- seq( min(vec_01,na.rm=T),  max(vec_01,na.rm=T), rbt)

#numbers to show
rbl <- round(rbs, digits = 1)


midpoint<-0
lowerhalf <- length(out[out < midpoint & !is.na(out)]) # values below 0
upperhalf <- length(out[out > midpoint & !is.na(out)]) # values above 0

# Color ramp parameters
## Colors
cols = c("#8B3A3A", "#CCCCCC", "#0000CD")
### vector of colors for values below midpoint
rc1 <- grDevices::colorRampPalette(colors = c(cols[3], cols[2]), space = "Lab")(lowerhalf)
### vector of colors for values above midpoint
rc2 <- grDevices::colorRampPalette(colors = c(cols[2], cols[1]), space = "Lab")(upperhalf)
### compile colors
rampcols <- c(rc1, rc2)



graphics::par(pty = "s")

#fix extent
p$out$v@extent <- GB_strata_ras@extent

fields::image.plot(p$out$v, breaks = p$out$breaks, col = p$out$cols, 
                   axes = TRUE, main = "log relative risk", xlab = "longitude", 
                   ylab = "latitude", legend.mar = 3.1, axis.args = list(at = rbs, 
                                                                         las = 0, labels = rbl, cex.axis = 0.67))




final_ras <- raster::mask(p$out$v,GB_strata_singlePoly)

final_matrix <- as.matrix(final_ras)

#turn matrix for plotting
rotate <- function(x) t(apply(x, 2, rev))
final_matrix_turned <- rotate(final_matrix)
par(mar=c(1,1,1,1))
fields::image.plot(final_matrix_turned)
