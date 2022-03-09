#will create the "hab" object for mixfish sim using previously created habiats for each species


loadedPackages <- c("rgdal", "data.table", "maptools","envi", "raster", "RStoolbox", "spatstat.data", "spatstat.geom", "spatstat.core")
invisible(lapply(loadedPackages, library, character.only = TRUE))



#load habitat matrices previously created and store values in hab

#haddock
Had_mat <- readRDS(file="TestScripts/Habitat_plots/Haddock/Haddock_Weighted_AdaptFalse_MATRIX.RDS")
fields::image.plot(Had_mat)

#cod
Cod_mat <- readRDS(file="TestScripts/Habitat_plots/Cod/Cod_Weighted_AdaptFalse_MATRIX.RDS")
fields::image.plot(Cod_mat)

#yellowtail
Yell_mat <- readRDS(file="TestScripts/Habitat_plots/YellowtailFlounder/YellowtailFlounder_Weighted_AdaptFalse_MATRIX.RDS")
fields::image.plot(Yell_mat)

hab<- list()
hab[["hab"]][["spp1"]] <- Had_mat / sum(Had_mat,na.rm = T) #normalize like MFS does
hab[["hab"]][["spp2"]] <- Cod_mat / sum(Cod_mat, na.rm=T)
hab[["hab"]][["spp3"]] <- Yell_mat / sum(Yell_mat, na.rm=T)




#CREATE HADDOCK STRATA
strata.dir <- "C:\\Users\\benjamin.levy\\Desktop\\NOAA\\GIS_Stuff\\" # strata shape files in this directory
# get the shapefiles
strata.areas <- readOGR(paste(strata.dir,"Survey_strata", sep="")) #readShapePoly is deprecated; use rgdal::readOGR or sf::st_read 
#define georges bank
GB_Had_strata_num <- c("01130","01140","01150","01160","01170","01180","01190","01200","01210","01220","01230","01240","01250", "01290", "01300")
#pull out indices corresponding to GB strata
GB_strata_idx <- match(GB_Had_strata_num,strata.areas@data[["STRATUMA"]])
#plot them
#plot(strata.areas[GB_strata_idx,])
#define GB strata as own object
GB_had_strata <- strata.areas[GB_strata_idx,]
plot(GB_had_strata)

#CREATE COD STRATA
#define georges bank
GB_Cod_strata_num <- c("01130","01140","01150","01160","01170","01180","01190","01200","01210","01220","01230","01240","01250")
#pull out indices corresponding to GB strata
GB_strata_idx <- match(GB_Cod_strata_num,strata.areas@data[["STRATUMA"]])
#plot them
#plot(strata.areas[GB_strata_idx,])
#define GB strata as own object
GB_cod_strata <- strata.areas[GB_strata_idx,]
plot(GB_cod_strata)

#CREATE YELLOWTAIL STRATA
#define georges bank
GB_Yel_strata_num <- c("01130","01140","01150","01160","01170","01180","01190","01200","01210")
#pull out indices corresponding to GB strata
GB_strata_idx <- match(GB_Yel_strata_num,strata.areas@data[["STRATUMA"]])
#plot them
#plot(strata.areas[GB_strata_idx,])
#define GB strata as own object
GB_yell_strata <- strata.areas[GB_strata_idx,]
plot(GB_yell_strata)



#ADD "STRATA" list to hab which is used to determine how many total strata there are in Bens_init_survey
hab[["strata"]] <- GB_had_strata@data$FINSTR_ID






#load previously created rasters

#haddock
Had_ras <- readRDS(file="TestScripts/Habitat_plots/Haddock/Haddock_Weighted_AdaptFalse_RASTER.RDS")
plot(Had_ras)
plot(GB_had_strata,add=T)

#cod
Cod_ras <- readRDS(file="TestScripts/Habitat_plots/Cod/Cod_Weighted_AdaptFalse_RASTER.RDS")
plot(Cod_ras)
plot(GB_cod_strata,add=T)

#yellowtail
Yell_ras <- readRDS(file="TestScripts/Habitat_plots/YellowtailFlounder/YellowtailFlounder_Weighted_AdaptFalse_RASTER.RDS")
plot(Yell_ras)
plot(GB_yell_strata,add=T)




###########################################################################
#create matrix with strata number inside each strata (same as hab$stratas)


#HADDOCK
all_had_strat_num <- list()

for(i in seq(length(GB_had_strata))){
  
  GB_strata_idx <- match(GB_had_strata@data[["STRATUMA"]][i],strata.areas@data[["STRATUMA"]])
  
  specific_strata <- strata.areas[GB_strata_idx,]
  strat_num <- specific_strata$STR2
  #first make everything outside strata 0
  x1<- mask(Had_ras,specific_strata,updatevalue=0)
  #then make everything inside given strata the strata number
  x2<- mask(x1,specific_strata,inverse=TRUE,updatevalue=strat_num)
  
  all_had_strat_num[[i]]<-x2
  
}

had_stratas <- Reduce('+',all_had_strat_num)
had_stratas<- as.matrix(had_stratas)
fields::image.plot(had_stratas)

#COD
all_cod_strat_num <- list()

for(i in seq(length(GB_cod_strata))){

GB_strata_idx <- match(GB_cod_strata@data[["STRATUMA"]][i],strata.areas@data[["STRATUMA"]])

specific_strata <- strata.areas[GB_strata_idx,]
strat_num <- specific_strata$STR2
#first make everything outside strata 0
x1<- mask(Cod_ras,specific_strata,updatevalue=0)
#then make everything inside given strata the strata number
x2<- mask(x1,specific_strata,inverse=TRUE,updatevalue=strat_num)

all_cod_strat_num[[i]]<-x2

}

cod_stratas <- Reduce('+',all_cod_strat_num)
cod_stratas<- as.matrix(cod_stratas)
fields::image.plot(cod_stratas)


#YELLOWTAIL FLOUNDER
all_yell_strat_num <- list()

for(i in seq(length(GB_yell_strata))){
  
  GB_strata_idx <- match(GB_yell_strata@data[["STRATUMA"]][i],strata.areas@data[["STRATUMA"]])
  
  specific_strata <- strata.areas[GB_strata_idx,]
  strat_num <- specific_strata$STR2
  #first make everything outside strata 0
  x1<- mask(Yell_ras,specific_strata,updatevalue=0)
  #then make everything inside given strata the strata number
  x2<- mask(x1,specific_strata,inverse=TRUE,updatevalue=strat_num)
  
  all_yell_strat_num[[i]]<-x2
  
}

yell_stratas <- Reduce('+',all_yell_strat_num)
yell_stratas<- as.matrix(yell_stratas)
fields::image.plot(yell_stratas)


#store values just created
#hab[["stratas"]] <- list(had_stratas,cod_stratas,yell_stratas)
hab[["stratas"]] <- had_stratas #just use haddock because it contains others

###########################################################################















###########################################################################
# NEED TO DEFINE SPAWNING GROUNDS
# IN THE MEANTIME USE REGULAR HABITAT AS SPAWNING TO GET THINGS GOING

hab[["spwn_hab"]] <- hab$hab  #CHANGE THIS



###########################################################################
# NEED TO DEFINE SPAWNING WEEKS
# IN THE MEANTIME USE RANDOM ONES

hab[["spwn_hab"]] <- hab$hab  #CHANGE THIS












#INTEGRATE WITH TEMP GRADIENT

#constant temp gradient
moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_ConstTemp_HaddockStrata")

#add temp tolerances order: had, cod, yellow
moveCov[["spp_tol"]] <- list() #just in case
moveCov[["spp_tol"]] <- list("spp1" = list("mu" = 8.5, "va" = 3.08), 
                             "spp2" = list("mu" = 8.75, "va" = 3.33), 
                             "spp3" = list("mu" = 5.5, "va" = 2.5) )







#1) PLOTTING JUST TEMPERATURE OVER TIME

#plot increasing temp gradient over time similar to how 
#plot_spatiotemp_hab_justtemp works, but without species-specific 
#influences


yearscut <- 2

#function to rotate image before plotting because image.plot rotates it
rotate <- function(x) t(apply(x, 2, rev))

pdf(file=paste0('testfolder/Monthly_temp_plots','.pdf'))

#figure out max/min temp to set color limits below
zmax <- max(unlist(lapply(moveCov$cov.matrix,FUN=max, na.rm=T)))
zmin <- min(unlist(lapply(moveCov$cov.matrix,FUN=min, na.rm=T)))


for(k in seq(12)){
  
  par(mfrow = c(5,4),mar = c(1, 1, 1, 1))
  
  
  for(i in seq(52*yearscut+1,length(moveCov$cov.matrix),52)){
    
    month_shift <- 4*(k-1)
    
    
    temp_rotate <- rotate(moveCov$cov.matrix[[i+month_shift]])
    
    fields::image.plot(temp_rotate, zlim = c(zmin,zmax))
    
    #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
    #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
    text(0.5, 0.98, labels = paste('Week', (i+month_shift)%%52,'Year', ceiling((i+month_shift)/52)), cex = 1)
    
    
  }
  
  
}


dev.off()







################################################################################

#2A) PLOTTING SPECIES-SPECIFIC TEMPERATURE OVER TIME CONSTANT TEMP
#ie, applying each species temp preferences to temp gradient




#constant temp gradient
moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_ConstTemp_HaddockStrata")

#add temp tolerances order: had, cod, yellow
moveCov[["spp_tol"]] <- list() #just in case
moveCov[["spp_tol"]] <- list("spp1" = list("mu" = 8.5, "va" = 3.08), 
                             "spp2" = list("mu" = 8.75, "va" = 3.33), 
                             "spp3" = list("mu" = 5.5, "va" = 2.5) )




library(MixFishSim)

yearscut <- 2

spp_names <- c("Haddock","Cod","Yellowtail Flounder")


#function to rotate image before plotting because image.plot rotates it
rotate <- function(x) t(apply(x, 2, rev))




#deifne spawning weeks (made up for now)
spwn_wk = list("spp1" = 15:18, "spp2" = 15:18, "spp3" = 15:18) #CHANGE THINS





pdf(file=paste0('testfolder/Monthly_species_temp_plots_ConstTemp','.pdf'))


maxtemp1 <-vector()
mintemp1<-vector()
maxtemp <- vector()
mintemp <- vector()


#obtain zlim bounds from maxtemp
zmax <- c(.23,.22,.26)
zmin <- c(0,0,0)

#PLOT EACH SPECIES in groups
for(s in seq_len(length(hab[["hab"]]))) {
  
  
  
  
  par(mfrow = c(13,4), mar = c(1, 1, 1, 1))
  
  
  for(i in seq(1,52)){
    #par( mar = c(1, 1, 1, 1))
    
    move_cov_wk <- moveCov[["cov.matrix"]][[i]]
    
    move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                              nr = nrow(move_cov_wk), 
                              sapply(move_cov_wk, norm_fun, 
                                     mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                     va = moveCov[["spp_tol"]][[s]][["va"]]))
    #col = grey(seq(1,0,l = 51)), 
    if(!i %in% spwn_wk[[s]]) {
      temp_rotate <- rotate( move_cov_wk_spp)
      fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 2, axes = F,zlim = c(zmin[s],zmax[s]))
    }
    # col = grey(seq(1,0,l = 51)),
    if(i %in% spwn_wk[[s]]) {
      temp_rotate <- rotate( move_cov_wk_spp)
      fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 1, axes = F, zlim = c(zmin[s],zmax[s]) )
    }
    #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
    #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
    text(0.5, 0.98, labels = paste('Week', (i)%%52, spp_names[s]), cex = 1)
    
    
    
  }
  
}



#PLOT EACH ON OWN PAGE
for(s in seq_len(length(hab[["hab"]]))) {
  
  
    # par(mfrow = c(10,6), mar = c(1, 1, 1, 1))
    
    
    for(i in seq(1,52)){
      par( mfrow = c(1,1), mar = c(1, 1, 1, 1))
  
      move_cov_wk <- moveCov[["cov.matrix"]][[i]]
      
      move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                                nr = nrow(move_cov_wk), 
                                sapply(move_cov_wk, norm_fun, 
                                       mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                       va = moveCov[["spp_tol"]][[s]][["va"]]))
      #col = grey(seq(1,0,l = 51)), 
      if(!i %in% spwn_wk[[s]]) {
        temp_rotate <- rotate(move_cov_wk_spp)
        fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 2, axes = F, zlim = c(zmin[s],zmax[s]))
      }
      # col = grey(seq(1,0,l = 51)),
      if(i %in% spwn_wk[[s]]) {
        temp_rotate <- rotate( move_cov_wk_spp)
        fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 1, axes = F, zlim = c(zmin[s],zmax[s]) )
      }
      #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
      #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
      text(0.5, 0.98, labels = paste('Week', (i)%%52, spp_names[s]), cex = 1)
      
      maxtemp1[i]<- max(temp_rotate,na.rm=T)
      mintemp1[i]<- min(temp_rotate,na.rm=T)
      
    }
  
  maxtemp[s] <- max(maxtemp1)
  mintemp[s] <- min(mintemp1)
    
}




dev.off()





################################################################################
#2B) PLOTTING SPECIES-SPECIFIC TEMPERATURE OVER TIME VARRYING TEMP
#ie, applying each species temp preferences to temp gradient


#load increasing temp gradient
#constant temp gradient
moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_IncrTemp_HaddockStrata")

#add temp tolerances order: had, cod, yellow
moveCov[["spp_tol"]] <- list() #just in case
moveCov[["spp_tol"]] <- list("spp1" = list("mu" = 8.5, "va" = 3.08), 
                             "spp2" = list("mu" = 8.75, "va" = 3.33), 
                             "spp3" = list("mu" = 5.5, "va" = 2.5) )





yearscut <- 2

#function to rotate image before plotting because image.plot rotates it
rotate <- function(x) t(apply(x, 2, rev))

#trying same zlim as above, max values may need to be extended
zmax <- c(.23,.22,.26)
zmin <- c(0,0,0)

pdf(file=paste0('testfolder/Monthly_species_temp_plots_IncrTemp','.pdf'))



#deifne spawning weeks (made up for now)
spwn_wk = list("spp1" = 15:18, "spp2" = 15:18, "spp3" = 15:18)






for(s in seq_len(length(hab[["hab"]]))) {
  
  
  for(k in seq(12)){
    
    
    par(mfrow = c(5,4), mar = c(1, 1, 1, 1))
    
    
    for(i in seq(52*yearscut+1,length(moveCov$cov.matrix),52)){
      
      month_shift <- 4*(k-1)
      
      move_cov_wk <- moveCov[["cov.matrix"]][[i+month_shift]]
      
      move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                                nr = nrow(move_cov_wk), 
                                sapply(move_cov_wk, norm_fun, 
                                       mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                       va = moveCov[["spp_tol"]][[s]][["va"]]))
      #col = grey(seq(1,0,l = 51)), 
      if(!i %in% spwn_wk[[s]]) {
        temp_rotate <- rotate( move_cov_wk_spp)
        fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 2, axes = F, zlim = c(zmin[s],zmax[s]))
      }
      # col = grey(seq(1,0,l = 51)),
      if(i %in% spwn_wk[[s]]) {
        temp_rotate <- rotate( move_cov_wk_spp)
        fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 1, axes = F, zlim = c(zmin[s],zmax[s]) )
      }
      #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
      #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
      text(0.5, 0.98, labels = paste(spp_names[s],'Week', (i+month_shift)%%52,'Year', ceiling((i+month_shift)/52)), cex = 1)
      
      
      
    }
    
    
  }
}


dev.off()









######################################################################################


#3A) PLOTTING SPECIES-SPECIFIC HABITAT OVER TIME WITH CONSTANT TEMP GRADIENT
#ie, applying each species temp preferences to temp gradient and combining with habitat preference



#load increasing temp gradient
#constant temp gradient
moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_ConstTemp_HaddockStrata")

#add temp tolerances order: had, cod, yellow
moveCov[["spp_tol"]] <- list() #just in case
moveCov[["spp_tol"]] <- list("spp1" = list("mu" = 8.5, "va" = 3.08), 
                             "spp2" = list("mu" = 8.75, "va" = 3.33), 
                             "spp3" = list("mu" = 5.5, "va" = 2.5) )





yearscut <- 2

#function to rotate image before plotting because image.plot rotates it
rotate <- function(x) t(apply(x, 2, rev))

#trying same zlim as above, max values may need to be extended
#zmax <- c(.23,.22,.26)
#zmin <- c(0,0,0)

pdf(file=paste0('testfolder/Monthly_species_temp_plots_HabTemp_ConstTemp','.pdf'))



#deifne spawning weeks (made up for now)
spwn_wk = list("spp1" = 15:18, "spp2" = 15:18, "spp3" = 15:18)


maxtemp1 <-vector()
mintemp1<-vector()
maxtemp <- vector()
mintemp <- vector()


#PLOT EACH SPECIES in groups

for(s in seq_len(length(hab[["hab"]]))) {
  
  
  
  par(mfrow = c(13,4), mar = c(1, 1, 1, 1))
  
  
  for(i in seq(1,52)){
    
    
    
    move_cov_wk <- moveCov[["cov.matrix"]][[i]]
    
    move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                              nr = nrow(move_cov_wk), 
                              sapply(move_cov_wk, norm_fun, 
                                     mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                     va = moveCov[["spp_tol"]][[s]][["va"]]))
    #col = grey(seq(1,0,l = 51)), 
    if(!i %in% spwn_wk[[s]]) {
      temp_rotate <- rotate(hab[["hab"]][[paste0('spp',s)]] * move_cov_wk_spp)
      fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 2, axes = F )
    }
    # col = grey(seq(1,0,l = 51)),
    if(i %in% spwn_wk[[s]]) {
      temp_rotate <- rotate(hab[["spwn_hab"]][[paste0('spp',s)]] * move_cov_wk_spp)
      fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 1, axes = F )
    }
    #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
    #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
    text(0.5, 0.98, labels = paste(spp_names[s],'Week', (i)%%52), cex = 1)
    
    
    
  }
  
  
}



#PLOT EACH ON OWN PAGE
for(s in seq_len(length(hab[["hab"]]))) {
  
  
  # par(mfrow = c(10,6), mar = c(1, 1, 1, 1))
  
  
  for(i in seq(1,52)){
    par( mfrow = c(1,1), mar = c(1, 1, 1, 1))
    
    move_cov_wk <- moveCov[["cov.matrix"]][[i]]
    
    move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                              nr = nrow(move_cov_wk), 
                              sapply(move_cov_wk, norm_fun, 
                                     mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                     va = moveCov[["spp_tol"]][[s]][["va"]]))
    #col = grey(seq(1,0,l = 51)), 
    if(!i %in% spwn_wk[[s]]) {
      temp_rotate <- rotate(hab[["hab"]][[paste0('spp',s)]] *move_cov_wk_spp)
      fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 2, axes = F)
    }
    # col = grey(seq(1,0,l = 51)),
    if(i %in% spwn_wk[[s]]) {
      temp_rotate <- rotate(hab[["hab"]][[paste0('spp',s)]] * move_cov_wk_spp)
      fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 1, axes = F )
    }
    #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
    #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
    text(0.5, 0.98, labels = paste('Week', (i)%%52, spp_names[s]), cex = 1)
    
    maxtemp1[i]<- max(temp_rotate,na.rm=T)
    mintemp1[i]<- min(temp_rotate,na.rm=T)
    
  }
  
  maxtemp[s] <- max(maxtemp1)
  mintemp[s] <- min(mintemp1)
  
}




dev.off()





















######################################################################################


#3B) PLOTTING SPECIES-SPECIFIC HABITAT OVER TIME WITH INCREASING TEMP GRADIENT
#ie, applying each species temp preferences to temp gradient and combining with habitat preference



#load increasing temp gradient
#constant temp gradient
moveCov <- readRDS(file="20 year moveCov matrices/GeorgesBank/GB_22yr_IncrTemp_HaddockStrata")

#add temp tolerances order: had, cod, yellow
moveCov[["spp_tol"]] <- list() #just in case
moveCov[["spp_tol"]] <- list("spp1" = list("mu" = 8.5, "va" = 3.08), 
                             "spp2" = list("mu" = 8.75, "va" = 3.33), 
                             "spp3" = list("mu" = 5.5, "va" = 2.5) )





yearscut <- 2

#function to rotate image before plotting because image.plot rotates it
rotate <- function(x) t(apply(x, 2, rev))

#trying same zlim as above, max values may need to be extended
#zmax <- c(.23,.22,.26)
#zmin <- c(0,0,0)

pdf(file=paste0('testfolder/Monthly_species_temp_plots_HabTemp_IncrTemp','.pdf'))



#deifne spawning weeks (made up for now)
spwn_wk = list("spp1" = 15:18, "spp2" = 15:18, "spp3" = 15:18)






for(s in seq_len(length(hab[["hab"]]))) {
  
  
  for(k in seq(12)){
    
    
    par(mfrow = c(5,4), mar = c(1, 1, 1, 1))
    
    
    for(i in seq(52*yearscut+1,length(moveCov$cov.matrix),52)){
      
      month_shift <- 4*(k-1)
      
      move_cov_wk <- moveCov[["cov.matrix"]][[i+month_shift]]
      
      move_cov_wk_spp <- matrix(nc = ncol(move_cov_wk),
                                nr = nrow(move_cov_wk), 
                                sapply(move_cov_wk, norm_fun, 
                                       mu = moveCov[["spp_tol"]][[s]][["mu"]], 
                                       va = moveCov[["spp_tol"]][[s]][["va"]]))
      #col = grey(seq(1,0,l = 51)), 
      if(!i %in% spwn_wk[[s]]) {
        temp_rotate <- rotate(hab[["hab"]][[paste0('spp',s)]] * move_cov_wk_spp)
        fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 2, axes = F )
      }
      # col = grey(seq(1,0,l = 51)),
      if(i %in% spwn_wk[[s]]) {
        temp_rotate <- rotate(hab[["spwn_hab"]][[paste0('spp',s)]] * move_cov_wk_spp)
        fields::image.plot(temp_rotate, cex.axis = 1.5, cex.main = 1, axes = F )
      }
      #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
      #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
      text(0.5, 0.98, labels = paste(spp_names[s],'Week', (i+month_shift)%%52,'Year', ceiling((i+month_shift)/52)), cex = 1)
      
      
      
    }
    
    
  }
}


dev.off()




















