#Plotting things


library(tidyverse)
library(readxl)
library(here)

#change into a data frame to be used by ggplot
new_random_survey <-do.call(rbind.data.frame,surv_random) #surv_random comes out of BENS_init_survey


gg_base <- ggplot(data = new_random_survey, aes(x = y, y = rev(x),col = as.factor(strata)))

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
