# toy.R
# simple test program to get MixFishSim running

# useful tools for package development and installation
# only need to run once
install.packages("devtools")

# get the MixFishSim package from GitHub repository
# only need to run once (may ask to install other packages first, do so)
devtools::install_github("pdolder/MixFishSim")

# now begin the code that runs each time

# tell R you want to use MixFishSim package
library(MixFishSim)

# look at the help functions using ?
?MixFishSim::init_pop

# follow examples in Simple_MixFishSim_Example.pdf

# set random number seed so results are repeatable
set.seed(123)

# Base parameters
sim <- init_sim(nrows = 10, ncols = 10, n_years = 10, n_tows_day = 4, n_days_wk_fished = 5, n_fleets = 2, n_vessels = 20, n_species = 2, move_freq = 2)

class(sim)

sim$idx

names(sim$brk.idx)

