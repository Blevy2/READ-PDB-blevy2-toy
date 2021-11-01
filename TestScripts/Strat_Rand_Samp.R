# TRY THIS FIRST THIN IN THE MORNING
# 
# Sample seems the best bet for you. To get 1000 random positions you can do something like:
#   
#   rows = sample(1:nrow(dataset), 1000, replace = TRUE)
#   columns = sample(1:ncol(dataset), 1000, replace = TRUE)
#   I think this gives what you want, but ofcourse I could be mistaken.
#   
#   Extracting the items from the matrix can be done like:
#     
#     random_sample = mapply(function(row, col) 
#       return(dataset[row,col]), 
#       row = rows, col = columns)





#trying this but keeps crashing
#works well when doesnt crash

np <- 1000 # number of elements desired
M1 <- t(combn(1:np, 2))
sam <- sample(1:nrow(M1), np, replace = FALSE)
M2 <- M1[sam,]
anyDuplicated(M2) # returns FALSE






#testing anyDuplicated. True to get some repeated rows and let it find them

x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) 
y <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

example.mat 	       <- matrix(NA, nrow = 10, ncol = 2)
colnames(example.mat)      <- c( "x","y")

example.mat[,'x']          <- x  #different than fixed station section
example.mat[,'y']          <- y  #different than fixed station section

t<-anyDuplicated(example.mat)


if (t==0){stop("There is a repeated survey row")}




#Below method only works for square areas
size <- 100
samples <- 1000
vals <- sample.int(size ^ 2, samples)
All_Samples <- cbind(vals %/% size + 1, vals %% size)

anyDuplicated(All_Samples)


#larger example from website
size <- 1e5
samples <- 100
vals <- sample.int(size ^ 2, samples)
All_Samples <- cbind(vals %/% size + 1, vals %% size)

anyDuplicated(All_Samples)


#the above would only work for square region.
#trying to adapt for rectangular

size_x <- 10   #x dimensions to select from. 
samples <- 25  #total number of stations (need n_stations for each year)
vals_x <- sample.int(size_x, samples,replace=TRUE)

#setting up y values
size_y <- 11  #y dimensions to select from. 
samples <- 25  #total number of stations (need n_stations for each year)
vals_y <- sample.int(size_y, samples, replace=TRUE)


All_Samples <- cbind(vals_x %% size_x + 1, vals_y %% size_y)

if(anyDuplicated(All_Samples)!=0){"There are duplicated sampling stations"}

anyDuplicated(All_Samples)

#explanation from webpage:

#Basically, we use integers to represent every possible combination of values. 
#In our example, we sample from all the numbers up to 1e5 ^ 2, since we have 1e5 ^ 2 possible combinations of 1e5 numbers. 
#Each of those 1e10 integers represents one of the combinations. 
#We then decompose that integer into the two component values by taking the modulo, as the first number, and the integer division as the second.



