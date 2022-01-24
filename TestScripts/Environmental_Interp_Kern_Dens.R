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

  #BATHYMETRY
bathy_ras <- raster('C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\NOAA_Maps\\Bathy_raster\\Seafloor_Bathymetry_Clip1_Pr21.tif')
plot(bathy_ras)

  #SEDIMENT
#categorical data (will this work?)
sediment_ras_categ <-  raster('C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\NOAA_Maps\\Atlantic_seafloor_sediment\\raster_categorical\\Sedim_ras_categ.tif')
plot(sediment_ras_categ)

#numerical data
sediment_ras_num <-  raster('C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\NOAA_Maps\\Atlantic_seafloor_sediment\\raster_number\\sediment_ras_num.tif')
plot(sediment_ras_num)

#SEDIMENT THICKNESS
sediment_thick_ras <-  raster('C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\NOAA_Maps\\GlobalSedimentThickness\\Final_ras\\sed_thick_ras.tif')
plot(sediment_thick_ras)


#clip data to desired area
bathy_ras <-crop(bathy_ras,GB_strata)
sediment_ras_num <- crop(sediment_ras_num,GB_strata)
sedmient_ras_categ <- crop(sediment_ras_categ,GB_strata)
sediment_thick_ras <- crop(sediment_thick_ras,GB_strata)

plot(bathy_ras)
plot(GB_strata,add=T)

plot(sediment_ras_num)
plot(GB_strata,add=T)

plot(sediment_thick_ras)
plot(GB_strata,add=T)

#create image files to use later
bathy_im<- as.im(bathy_ras)
sediment_im <- as.im(sediment_ras_num) #CHANGE FROM CATEGORICAL TO NUMERICAL HERE
sediment_thick_im <- as.im(sediment_thick_ras)

#scale data
bathy_im$v <- scale(bathy_im)
sediment_im$v <- scale(sediment_im)
sediment_thick_im$v <- scale(sediment_thick_im)


#old (wrong?) way
#bathy_im_sc<- scale(as.im(bathy_ras))  #scaling here
#sediment_im_sc <- scale(as.im(sediment_ras_num)) #CHANGE FROM CATEGORICAL TO NUMERICAL HERE

#CHANGE BACK TO RASTER AFTER SCALING
bathy_ras <- raster::raster(bathy_im)
sediment_ras_num <- raster::raster(sediment_im)
sediment_thick_ras <- raster::raster(sediment_thick_im)






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

spring.gis <- gis[gis$SEASON=='SPRING',]
fall.gis   <- gis[gis$SEASON=='FALL',]


#ONLY TAKE MORE RECENT ONES?
spring.gis <- spring.gis[spring.gis$Year>2005,]



#setting up weighting following along from https://stackoverflow.com/questions/21273525/weight-equivalent-for-geom-density2d
library(data.table)
weighted_data_spring <- data.table(spring.gis)[,list(Longitude=rep(Longitude,ceiling(CatchWt)),Latitude=rep(Latitude,ceiling(CatchWt)),CatchWt=rep(ceiling(CatchWt),ceiling(CatchWt)))]

#convert data into planar point pattern (ppp). no weight option so use weighted/repeated points
spring_points <- ppp(weighted_data_spring$Longitude,weighted_data_spring$Latitude,owin(c(-69.98, -65.68) ,c(40.09,42.45) ))



#Add appropriate marks to the data from spatstat.data package. 
#These points are considered our "presence" locations.

presence <- spatstat.geom::unmark(spring_points)  #unmark removes existing attributes (aka marks)
spatstat.geom::marks(presence) <- data.frame("presence" = rep(1, presence$n), #adds attributes back (aka marks)
                                             "lon" = presence$x,  #first repeat the nuimber 1 n times, then add x and y coordinates for each
                                             "lat" = presence$y)


spatstat.geom::marks(presence)$bathy <-  bathy_im[presence] #adds covariate values for presence locations
#spatstat.geom::marks(presence)$sediment <- sediment_im[presence]
spatstat.geom::marks(presence)$sediment_thick <- sediment_thick_im[presence]





#DEFINE ABSENCE LOCATIONS

abs <-  data.table(spring.gis)[(CatchWt==0)]  #pull out absence points

#remove duplicate locations because it leads to problems later
# NO IT DOESNT, THERE WERE POINTS OUTSIDE THE RASTER REGION
#abs <- abs[!duplicated(abs[,c("LONGITUDE","LATITUDE")]),]

absence <-  ppp(abs$Longitude,abs$Latitude,owin(c(-69.98, -65.68) ,c(40.09,42.45)))


spatstat.geom::marks(absence) <- data.frame("presence" = rep(0, absence$n),
                                            "lon" = absence$x,
                                            "lat" = absence$y)
spatstat.geom::marks(absence)$bathy <-  bathy_im[absence] #some points are outside the region
#spatstat.geom::marks(absence)$ sediment <- sediment_im[absence]
spatstat.geom::marks(absence)$ sediment_thick <- sediment_thick_im[absence]






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

predict_locs <- data.frame(raster::rasterToPoints(bathy_ras))  #adds column called layer with bathymetry
#predict_locs$layer2 <- raster::extract(sediment_ras_num, predict_locs[, 1:2]) #adds column called layer2 with sediment number
predict_locs$layer2 <- raster::extract(sediment_thick_ras, predict_locs[, 1:2]) #adds column called layer2 with sediment number




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
                    #balance = TRUE)
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

