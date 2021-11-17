#' @title Plot population summary

#' @description \code{plot_pop_summary} plots the four population dynamic
#' metrics: catches, biomass, fishing mortality and recruitment.

#' It can either operate at a daily timestep or an annual timestep

#' @param results is an output from the function \link{run_sim}.
#' @param timestep is a character string determining whether the plot is
#' 'daily' or 'annual'
#' @param save is a logical whether to save the plot
#' @param save.location is a location (defaults to current directory)

#' @return is a ggplot of all the species and metrics as a faceted plot

#' examples
#' plot_pop_summary(results = res, timestep = 'daily', save = TRUE, location = '.') 
#' ## Not run

#' @export

Bens_plot_pop_spatiotemp <- function(results = res, timestep = 'daily', save = FALSE, save.location = '.',sim=sim) {

  
  
  
  #creating a plot similar to plot_spatiotemp_hab
  
  nrows <- nrow(res$pop_bios[[1]]$spp1) 
  ncols <- ncol(res$pop_bios[[1]]$spp1)
  
  
  for(s in seq_len(length(results[["pop_summary"]]))) {  #number of species
    
    nt <- length(results[["pop_bios"]])  #number of weeks to plot
    if(save == TRUE & !is.null(save.location)) {
      png(filename = paste0(save.location,'/','Population_spatiotemp_spp_',s,'.png'), width = 800, height = 800)
    }
    par(mfrow = c(ceiling(sqrt(nt)), ceiling(nt/ceiling(sqrt(nt)))), mar = c(1, 1, 1, 1))
    
    for(i in seq_len(nt-1)) {   #PLOTTED 1 LESS THAN END BECAUSE COMING UP NULL
      
      pop_wk <- results[["pop_bios"]][[i]][s]   
      
#original color option col = grey(seq(.5,0,l = 51))
#Other options that ive tried
# col = hcl.colors(12, "YlOrRd")  makes very red picture
      image(results[["pop_bios"]][[i]][[paste0('spp',s)]], cex.axis = 1.5, cex.main = 2,  col = grey(seq(1,0,l = 51)) , axes = F)
      
      
      
      axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
      axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
      text(0.5, 0.98, labels = paste('week', i), cex = 1)

      
      
    }
    dev.off()
  }
	
	
  
  #trying different types of plots
  library(lattice)
  
  for(s in seq_len(length(results[["pop_summary"]]))) {  #number of species
    
    nt <- length(results[["pop_bios"]])  #number of weeks to plot

    par(mfrow = c(ceiling(sqrt(nt)), ceiling(nt/ceiling(sqrt(nt)))), mar = c(1, 1, 1, 1))
    
    for(i in seq_len(nt-1)) {   #PLOTTED 1 LESS THAN END BECAUSE COMING UP NULL
      

      
      Pop<-matrix(unlist(results[["pop_bios"]][[i]][[paste0('spp',s)]]),ncol = ncols, nrow= nrows)
      
     # levelplot(Pop,col.regions = terrain.colors(20)) #number in terrain.colors tells how many colors to use
      
     # levelplot(Pop,col.regions = heat.colors(15)) #number in terrain.colors tells how many colors to use
      if(save == TRUE & !is.null(save.location)) {
     png(filename = paste0(save.location,'/','Population_spatiotemp_spp_',s,i,'.png'), width = 800, height = 800)
   }  
      library(RColorBrewer)
      coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
   levelplot(Pop, col.regions = coul) # try cm.colors() or terrain.colors()
   
 #  P1<-levelplot(Pop, col.regions = coul) # try cm.colors() or terrain.colors()
   
 
     
   #uncomment to plot to plot window
   #show(P1)
      
      #attempting to put them all on same plot
     # print(p1,split=c(1,1,ceiling(sqrt(nt)), ceiling(nt/ceiling(sqrt(nt)))),more=TRUE)
     dev.off() 
      
    }
   
  }
	
	
	
}

