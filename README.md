# READ-PDB-blevy2-toy

# This repository will be for practicing 

#test


#Descriptions of new scripts

init_movCov_Bens  (original, 2 and 3) each create temperature covariate matrices that extend longer than 1 year


init_movCov_Bens edits init_movCov in 2 ways
	1) changes trend from decreasing temperature over the course of a year to increasing temp
	2) each year it extends the temp further
This one is not ideal because it starts at the same temp each year and extends further


init_movCov_Bens2 edits init_movCov by starting at a higher temp point each year but following the same trend
problem with this trend is that it just increases temp over the course of a year and then jumps back to a lower value


init_movCov_Bens3 is an attempt at an oscillating relationship rather than just increasing over the course of a year and then jumping back to a lower value

init_movCov_Bens4 is same as init_moveCov except only creates first matrix. Then this matrix is adjusted, sine is applied, second half stretched, and finally copied 52 times


init_sim_Bens...
 1) creates different week.breaks (called week.breaks.all) that will run from week 1 to week n where n is total week in simulation
week.breaks.all will be used to call the temperature covariate matrix for each week in the simulation (rather than looping back each year)
2) creates a day.seq and day.breaks that allows us to intialize a fleet where no fishing is taking place (ghost fleet fishes 1 day per week)

Bens_plot_spatiotemp_hab takes plot_spatiotemp_hab and allows only certain weeks to be plotted, as specified by plot_wk

Bens_plot_spatiotemp_hab_justtemp takes plot_spatiotemp_hab and plots only the temperature gradient (no habitat preferences). Also allows only certain weeks to be plotted, as specified by plot_wk



