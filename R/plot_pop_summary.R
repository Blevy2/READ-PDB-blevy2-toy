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

plot_pop_summary <- function(results = res, timestep = 'daily', save = FALSE, save.location = '.') {

	n_spp <- length(results[["pop_summary"]]) 
		res_df <- lapply(seq_len(n_spp), function(x) {
	 		res_spp <- lapply(names(results[["pop_summary"]][[x]]), function(x1) {
			      x1_res <- tidyr::gather(as.data.frame(t(results[["pop_summary"]][[x]][[x1]])), key = "year", factor_key = T)
		      	      if(x1 !="Rec.mat") {	res_out <- data.frame("pop" = rep(paste("spp",x, sep = "_"), length.out = nrow(x1_res)), 
						              "metric" = rep(sapply(strsplit(x1,".",fixed = T),"[",1), length.out = nrow(x1_res)), 
							      "year" = x1_res$year, 
							      "day" = rep(1:362, length.out = nrow(x1_res)),
							      "julien_day" = seq_len(nrow(x1_res)),
							      "data" = x1_res$value) }
			      if(x1 == "Rec.mat") { res_out <- data.frame("pop" = rep(paste("spp",x, sep = "_"), length.out = nrow(x1_res)), 
						              "metric" = rep(sapply(strsplit(x1,".",fixed = T),"[",1), length.out = nrow(x1_res)), 
							      "year" = seq_len(nrow(x1_res)), 
							      "day" = rep(1, length.out = nrow(x1_res)),
							      "julien_day" = rep(1, length.out = nrow(x1_res)),
							      "data" = x1_res$value) 
			      }
			      return(res_out)
			      })
			return(do.call(rbind, res_spp))
	   })
	results_df <- do.call(rbind, res_df)

	if(timestep == "daily") {
	require(ggplot2)
	print(ggplot(results_df, aes(x = julien_day, y = data, group = 2)) + geom_point() + facet_grid(pop ~ metric, scale = "free"))
	}
	
	if(timestep == "annual") {
	require(ggplot2); require(dplyr)
		
	results_df_an1 <- results_df %>% filter(metric == "Bio", day == 1) %>% 
	group_by(pop, metric, year) %>% summarise(data = sum(data))
	results_df_an2 <- results_df %>% filter(metric != "Bio") %>% 
	group_by(pop, metric, year) %>% summarise(data = sum(data, na.rm = T))

	results_df_annual <- rbind(results_df_an1, results_df_an2) 

	print(ggplot(results_df_annual, aes(x = year, y = data, group = 2)) + geom_point() + geom_line() + 
	facet_grid(pop ~ metric, scale = "free") + expand_limits(y = 0))
	}

	if(save == TRUE) {
	ggsave(file = file.path(paste(save.location, '/Population_Summary', timestep, '.png', sep = "")))
	}
	
	
	
	
	
	
	
	
	
	#creating a plot similar to plot_spatiotemp_hab
	
	nrows <- nrow(res$pop_bios[[1]]$spp1) 
	ncols <- ncol(res$pop_bios[[1]]$spp1)
	
	
	for(s in seq_len(length(results[["pop_summary"]]))) {  #number of species
	  
	  nt <- length(results[["pop_bios"]])  #number of weeks to plot
	  if(!is.null(plot.file)) {
	    png(filename = paste0(plot.file,'/','Population_spatiotemp_spp_',s,'.png'), width = 800, height = 800)
	  }
	  par(mfrow = c(ceiling(sqrt(nt)), ceiling(nt/ceiling(sqrt(nt)))), mar = c(1, 1, 1, 1))
	  
	  for(i in seq_len(nt)) {
	    
	    pop_wk <- results[["pop_bios"]][[i]][s]   
	    
	   
	    
	   image(results[["pop_bios"]][[i]][[paste0('spp',s)]], cex.axis = 1.5, cex.main = 2, col = grey(seq(1,0,l = 51)), axes = F)
	    
	    

	    axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
	    axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
	    text(0.5, 0.98, labels = paste('week', i), cex = 1)
	    
	  }
	  dev.off()
	}
}

