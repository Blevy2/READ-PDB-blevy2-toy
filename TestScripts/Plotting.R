#Plotting things


library(tidyverse)
library(readxl)
library(here)




############################################################################
## Plotting survey by strata
############################################################################
#Below assumes surv_random in in environment, created by BENS_init_survey

#change into a data frame to be used by ggplot
new_random_survey <-do.call(rbind.data.frame,surv_random) #surv_random comes out of BENS_init_survey


gg_base <- ggplot(data = new_random_survey, aes(x = as.numeric(y), y = as.numeric(x),col = as.factor(strata)))

#plot each strata a different color
gg_base + geom_point()

#THE ABOVE PLOT SEEMS TO PLOT THINGS STRANGE/OUT OF ORDER



#trying a simple plot with plot() command
plot(new_random_survey$y,
     new_random_survey$x,                       # Draw Base R plot
     pch = 16,
     col = new_random_survey$strata)

legend(1, 95, legend=c("Strata 1","Strata 2","Strata 3","Strata 4"),
       col=c("green", "blue", "black", "red"), lty=1:2, cex=0.8)




#plot some matrices

ggplot(dat) +
geom_tile(aes(fill=factor(value),alpha=0.8)) + 
    geom_polygon(data=dat, aes(x=res[["fleets_catches"]][[2]][[1]][[v]][1041:2080, "x"], y=res[["fleets_catches"]][[2]][[1]][[v]][1041:2080, "y"], group=group), 
                      fill=NA,color="grey50", size=1)+
coord_equal()





############################################################################
## Plotting spatial population plots
############################################################################



#from https://www.r-graph-gallery.com/27-levelplot-with-lattice.html

#install.packages("lattice")
#install.packages("RColorBrewer")
library(lattice)


#convert list into matrix
Pop<-matrix(unlist(res$pop_bios[[1]][[1]]),ncol = 100,nrow=100)

levelplot(Pop,col.regions = terrain.colors(20)) #number in terrain.colors tells how many colors to use

levelplot(Pop,col.regions = heat.colors(15)) #number in terrain.colors tells how many colors to use

library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
levelplot(Pop, col.regions = coul) # try cm.colors() or terrain.colors()









#assumes results are in res
data<-as.numeric(res[["pop_bios"]][[44]][[1]]) #change to numeric





############################################################################
#plotting vessel numbers on top of value map
############################################################################
library(ggplot2)

timespan <- 100:200


# Only last year
image(1:100, 1:100, 0.02 * 100 * Pop[["Start_pop"]][[1]] + 
        0.01 * 200 * Pop[["Start_pop"]][[2]])

for(i in timespan){

points(res[["fleets_catches"]][[2]][[1]][[v]][i, "x"], 
       res[["fleets_catches"]][[2]][[1]][[v]][i, "y"], pch = "x",cex=sqrt(i) )
}













############################################################################
#to plot just the temp preferences over time
############################################################################
source("R/BENS_plot_spatiotemp_hab_justtemp.R")

BENS_plot_spatiotemp_hab_justtemp(hab = hab, moveCov = moveCov, 
spwn_wk = list("spp1" = 16:18, "spp2" = 16:19,"spp3" = 16:18, "spp4" = 18:20), 
plot.file =  "testfolder")