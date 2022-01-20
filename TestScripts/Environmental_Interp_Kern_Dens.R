#Following from https://cran.r-project.org/web/packages/envi/vignettes/vignette.html






####################################
# looking for data to use
########################

test.dir <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\From_Alicia\\" # strata shape files in this directory
library(rgdal)

#east coast outline
test <- readOGR(paste(test.dir,"EastCoast_SmoothLines", sep="")) 

plot(test)

#continents
test2 <- readOGR(paste(test.dir,"ne_10m_land", sep=""))

plot(test2)


test3 <- readOGR(paste(test.dir,"nw_10m_bathymetry_L_0", sep=""))

plot(test3)




#NOAA directory
noaa.dir <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\From_NOAA_Drive\\" # strata shape files in this directory

#doesnt work
noaa1 <- readOGR(paste(noaa.dir,"b100_gom", sep="")) 

noaa2 <- readOGR(paste(noaa.dir,"world_bathy", sep="")) 


gis.dir <- noaa.dir <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\" # strata shape files in this directory
# 
# 
 test <- readOGR(paste(gis.dir,"GEBCO_19_Jan_2022", sep="")) 


library(raster)
bathy <- raster("C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\Bathy_Geotiff_noaa_ncei\\exportImage.tiff")
plot(bathy, add=T)




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

#SEDIMENT MAP
sed.dir <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\" # strata shape files in this directory
library(rgdal)

sediment <- readOGR(paste(sed.dir,"Atlantic_seafloor_sediment", sep="")) 
sediment <- raster(sediment)
#plot(sediment)
#View(sediment)


#BATHYMETRY MAP
library(raster)
#THIS ONE IS FROM GEBCO AND IS INCOMPLETE?
#bathy <- raster("C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\Bathy_Geotiff_noaa_ncei\\exportImage.tiff")
bathy.dir <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\From_Chris\\eastcoast_bathymetry\\"   # directory containing bathymetric isopleths
bathy <- sf::st_read(paste(bathy.dir,"eastcoastbathy_smoothed_NAD83", sep="")) #readShapeLines is deprecated; use rgdal::readOGR or sf::st_read 
plot(bathy)



#Center and scale the covariate data.

#sediment$v <- scale(sediment)   #cant do this with categorical data
#bathy$v <- scale(bathy)        #raster is too large (all of east coast)


#Convert the covariate data to class RasterLayer.

sediment_raster <- raster::raster(sediment)
bathy_raster <- raster::raster(bathy)
plot(bathy_raster)
plot(sediment_raster)
