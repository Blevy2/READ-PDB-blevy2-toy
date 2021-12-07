#this starts the profiling
library(profvis)

profvis({


library(MixFishSim)


#for profiling
library(lineprof)


#NEW VERSION that has week breaks for entire simulation
source("R/init_sim_Bens_nofish.R")
sim <- init_sim_Bens_nofish(nrows = 100, ncols = 100, n_years = 2, n_tows_day = 1,
                            n_days_wk_fished = 1, n_fleets = 1, n_vessels = 1, n_species = 2,
                            move_freq = 1)


source("R/BENS_create_hab.R")

source("R/BENS_plot_habitat.R")

#values settled on from anisotropy and habtest scripts

spp.ctrl = list(
  "spp.1" = list('nu' = 1/0.05, 
                 'var' = 1,
                 'scale' = 5, 
                 'Aniso' = matrix(nc = 2,  c(1.5, -3, 3, 4) )),
  "spp.2" = list('nu' = 1/0.015, 
                 'var'  = 1,
                 'scale' = 25, 
                 'Aniso' = matrix(nc = 2,c(1, -2, 1, 2))),
  plot.dist = FALSE, 
  plot.file = "testfolder"
)


hab <- BENS_create_hab(sim_init = sim, 
                       spp.ctrl = spp.ctrl,
                       
                       spawn_areas = list(
                         "spp1" = list(
                           'area1' = c(30,45,55,65),   #of the form  c(x1, x2, y1, y2) THESE ARE BOUNDARIES OF MATRIX VALUES
                           'area2' = c(70,90,50,60)   #need to revisit closure areas
                         ),
                         "spp2" = list(
                           'area1' = c(30,45,55,65),
                           'area2' = c(70,90,50,60)
                         ),
                         spwn_mult = 10, 
                         plot.dist = FALSE, 
                         plot.file = "testfolder" ),
                       
                       
                       #created this new part defining strata
                       
                       strata = list(                              #Form: upper left, upper right, lower left, lower right
                         "strata1" = c(1,sim$idx[["nrows"]]/2,1,sim$idx[["ncols"]]/2),   #of the form  c(x1, x2, y1, y2) THESE ARE BOUNDARIES OF STRATA VALUES
                         
                         "strata2" = c(1,sim$idx[["nrows"]]/2,sim$idx[["ncols"]]/2+1,sim$idx[["ncols"]]),
                         
                         "strata3" = c(sim$idx[["nrows"]]/2 + 1,sim$idx[["nrows"]], 1 ,sim$idx[["ncols"]]/2),
                         
                         "strata4" = c(sim$idx[["nrows"]]/2 + 1,sim$idx[["nrows"]],sim$idx[["ncols"]]/2 +1 , sim$idx[["ncols"]])
                       )
)




Pop <- init_pop(sim_init = sim, Bio = c("spp1" = 1e5, "spp2" = 2e5), #these values from paper 
                hab = hab[["hab"]], start_cell = c(50,50), 
                lambda = c("spp1" = 0.1, "spp2" = 0.1), #same lambda for all?
                init_move_steps = 20,
                rec_params = list("spp1" = c("model" = "BH", "a" = 6, "b" = 4, "cv" = 0.7),
                                  "spp2" = c("model" = "BH", "a" = 27, "b" = 4,"cv" = 0.6)), #these values from paper 
                rec_wk = list("spp1" = 12:15, "spp2" = 12:15),
                spwn_wk = list("spp1" = 15:18, "spp2" = 15:18),
                M = c("spp1" = 0.2, "spp2" = 0.1), #these values from paper 
                K = c("spp1" = 0.3, "spp2" = 0.3) #all the same for now
)





moveCov <- init_moveCov(sim_init = sim, steps = 52,
                        spp_tol = list("spp1" = list("mu" = 12, "va" = 8),
                                       "spp2" = list("mu" = 15, "va" = 9) #given as 7 in code but 9 in paper
                                       #			       "spp3" = list("mu" = 17, "va" = 7),
                                       #			       "spp4" = list("mu" = 14, "va" = 10)
                        )
)




#no fishing
fleets <- init_fleet(sim_init = sim, VPT = list("spp1" = 10, "spp2" = 10), #VPT = value per ton
                     Qs = list("fleet 1" = c("spp1" = 0.1, "spp2" = 0.1)   #Q = catchability
                     ),
                     fuelC = list("fleet1" = 3),
                     step_params = list("fleet 1" = c("rate" = 3, "B1" = 1, "B2" = 2, "B3" = 3)
                     ),				
                     past_knowledge = FALSE,  #dont use past knowledge
                     past_year_month = TRUE,
                     past_trip = TRUE,
                     threshold = 0.7
)



source("R/BENS_init_survey.R")



#CURRENTLY NEED TO MAKE SURE THAT N_STATIONS*#YEARS / #STRATA IS A WHOLE NUMBER OTHERWISE DAY, TOW, YEAR WONT LINEUP WITH NUMBER OF STATIONS
#ALSO NEED N_STATION TO BE DIVISIBLE BY STATIONS_PER_DAY


surv_random <- BENS_init_survey(sim_init = sim,design = 'random_station', n_stations = 50, start_day = 1, stations_per_day = 7, Qs = c("spp1" = 0.1, "spp2"= 0.2), strata_coords = hab$strata, strata_num = hab$stratas )


source("R/run_sim.R")

source("R/go_fish.R") 





#to source a new go_fish where I edited to skips most things:
#1: load file
source("R/go_fish_Bens.R") #my edited version that skips most things
#2: allow the function to call other hidden functions from mixfishsim 
environment(go_fish_Bens) <- asNamespace('MixFishSim')
#3: replace go_fish with go_fish_Bens in the MixFishSim package
assignInNamespace("go_fish", go_fish_Bens, ns = "MixFishSim")


res <- run_sim(sim_init = sim,
               pop_init = Pop,
               move_cov = moveCov,
               fleets_init = fleets,
               hab_init = hab,
               save_pop_bio = TRUE,
               survey = surv_random,
               closure = NULL,
               InParallel = TRUE)  #does it runin parallel? Doesnt seem like it

})


