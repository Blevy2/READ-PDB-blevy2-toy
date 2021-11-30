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



init_sim_Bens creates different week.breaks (called week.breaks.all) that will run from week 1 to week n where n is total week in simulation
week.breaks.all will be used to call the temperature covariate matrix for each week in the simulation (rather than looping back each year)