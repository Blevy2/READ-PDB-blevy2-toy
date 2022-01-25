
temp2 <- list(moveCov[["cov.matrix"]][[52]])

for(i in seq(51,1)){
  
  
  temp<-list(moveCov[["cov.matrix"]][[i]])

  covmatrix <- list(moveCov[["cov.matrix"]][[i]])
  
  temp2 <- append(covmatrix,temp2)
  
  
}
 

#added two years to the front end that are copies of the first year
append(temp2,moveCov[["cov.matrix"]]) -> new
append(temp2,new) -> new

# View(new)
 
 moveCov[["cov.matrix"]]<-new

 
 #remove extra stuff taking up memory
 remove(temp)
 remove(temp2)
 remove(new)
 remove(Move_Prob)
 remove(Move_Prob_spwn)
 remove(MoveProb)
 remove(MoveProb_spwn)
 
 remove(Good_moveCov)
 remove(all_moveCov)
 remove(B_moveCov)
 remove(Pop)
 remove(surv_random)
 remove(fleets)
 
 
 
 
 ##old and wrong (only did 2 weeks)
 
 # temp<-list(moveCov[["cov.matrix"]][[1]],moveCov[["cov.matrix"]][[1]])
 # append(temp,moveCov[["cov.matrix"]]) -> new
 # View(new)
 # moveCov[["cov.matrix"]]<-new
 # 
 # remove(temp)