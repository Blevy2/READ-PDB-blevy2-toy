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

Bens_plot_pop_spatiotemp <- function(results = res, timestep = 'daily', save = FALSE , plot_monthly = FALSE, save.location = NULL, plot_weekly = FALSE) {
  
  
  

  
  #creating a plot similar to plot_spatiotemp_hab
  
  nrows <- nrow(res$pop_bios[[1]]$spp1) 
  ncols <- ncol(res$pop_bios[[1]]$spp1)
  
  
  if(plot_weekly == TRUE){
  
  for(s in seq_len(length(sim$idx[["n.spp"]]))) {  #number of species
    
    nt <- length(results[["pop_bios"]])  #number of weeks to plot
    if(!is.null(save.location)) {
      png(filename = paste0(save.location,'/','Population_spatiotemp_spp_',s,'.png'), width = 800, height = 800)
    }
    par(mfrow = c(ceiling(sqrt(nt)), ceiling(nt/ceiling(sqrt(nt)))), mar = c(1, 1, 1, 1))
    
    for(i in seq_len(nt-1)) {   #PLOTTED 1 LESS THAN END BECAUSE COMING UP NULL
      
      pop_wk <- results[["pop_bios"]][[i]][s]   
      
      
      
      image(results[["pop_bios"]][[i]][[paste0('spp',s)]], cex.axis = 1.5, cex.main = 2, col = grey(seq(1,0,l = 51)), axes = F)
      
      
      
      axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
      axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
      text(0.5, 0.98, labels = paste('week', i), cex = 1)
      
    }
    dev.off()
  }
    
  }
  
  
  
  
  
  
  
  if(plot_monthly == TRUE){
    
    if(!is.null(save.location)) {

      pdf(file=paste0(save.location,'/Monthly_sd_plots','.pdf'))
      #png(filename = paste0(plot.file,'/Monthly_covariate_plots/','monthly_habitat_spatiotemp_spp_',s,'month_',k,'.png'), width = 800, height = 800)
    }
    
    
    
    for(s in seq_len(length(hab[["hab"]]))) {
      
      #set color range for spatial plots
      colrange <- range(res[["pop_bios_sd"]][[s]],na.rm=TRUE)
      
      
      
      for(k in seq(12)){
        
        nt <- length(moveCov[["cov.matrix"]])

        par(mfrow = c(5,4),mar = c(1, 1, 1, 1))
        
        
        for(i in seq(1,nt,52)){
          
          month_shift <- 4*(k-1)
          
          pop_sd_wk <- res[["pop_bios_sd"]][[s]][[i+month_shift]]
          
          
          fields::image.plot(pop_sd_wk,  zlim = colrange )
          
          #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
          #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
          text(0.5, 0.98, labels = paste('week', i), cex = 1)
          
          
        }
        
        
      }
    }
    
    dev.off()
    
    
    if(!is.null(save.location)) {
      
      pdf(file=paste0(save.location,'/Monthly_pop_plots','.pdf'))
      #png(filename = paste0(plot.file,'/Monthly_covariate_plots/','monthly_habitat_spatiotemp_spp_',s,'month_',k,'.png'), width = 800, height = 800)
    }
    
    
    
    for(s in seq_len(length(hab[["hab"]]))) {
      
      #set color range for spatial plots
      colrange <- range(res[["pop_bios"]][[s]],na.rm=TRUE)
      
      
      
      for(k in seq(12)){
        
        nt <- length(moveCov[["cov.matrix"]])
        
        par(mfrow = c(5,4),mar = c(1, 1, 1, 1))
        
        
        for(i in seq(1,nt,52)){
          
          month_shift <- 4*(k-1)
          
          pop_sd_wk <- res[["pop_bios"]][[i+month_shift]][[s]]
          
          
          fields::image.plot(pop_sd_wk,  zlim = colrange )
          
          #	  axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
          #	  axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
          text(0.5, 0.98, labels = paste('week', i), cex = 1)
          
          
        }
        
        
      }
    }
    
    dev.off()
    
    
    
  }

  
  
  
  
  
}
