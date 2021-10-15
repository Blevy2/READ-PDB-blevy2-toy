


#create 10x10 matrix with random values between 0 and 1


#sample from it n times 
#add noise to each sample
# compare true sample mean with noise sample mean

#create matrix with random values
r <- 1000
c <- 1000
m1 <- matrix(runif(r*c), r, c)

#sample the matrix n times without replacement
set.seed(1)

n <- 500
samp <- sample(m1,size=n,replace=FALSE)

#calculate means
true_mean <- mean(m1) #should be about .5
samp_mean <- mean(samp) 

#create some noise same length as samp
#then add to samp
#rnorm(n, mean = 0, sd = 1)
noise <- rnorm(length(samp),0,.15)

samp_noise <- samp + noise
mean_samp_noise <- mean(samp_noise)





# create a loop that increases the number of samples and evaluates how close they are to the true value
x <- c(1:1000)

idx <- 1

#preallocate matrices for storage
samp_noise <- matrix(NA,length(x),max(x))
samp_mean <- matrix(0,1,length(x))
mean_samp_noise <- matrix(0,1,length(x))

true_mean <- mean(m1) #should be about .5

for (n in x){
  samp <- sample(m1,size=n,replace=FALSE)

  #calculate means

  samp_mean[idx] <- mean(samp) 
  
  #create some noise same length as samp
  #then add to samp
  #rnorm(n, mean = 0, sd = 1)
  noise <- rnorm(length(samp),0,.3)
  

    samp_noise[idx,1:n] <- samp+noise
  
  
  mean_samp_noise[idx] <- mean(samp_noise[idx,1:n])
  
  idx <- idx+1
}

library(tidyverse)
library(readxl)
library(here)

plot(x,samp_mean)
  

plot(x,mean_samp_noise)


#the output is being classified as a value so must reclassify as data to plot
#mean_samp_noise <- as.data.frame(mean_samp_noise)

#ggplot needs to have columns/rows labeled 

#ggplot(data = mean_samp_noise, aes(x = c(10,50,100,200,400,500,1000),y= c(10,1000)))+
#  geom_line()
