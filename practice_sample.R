

#create 10x10 matrix with random values between 0 and 1


#sample from it n times 
#add noise to each sample
# compare true sample mean with noise sample mean

#create matrix with random values
r <- 10000
c <- 10000
m1 <- matrix(runif(r*c), r, c)

#sample the matrix n times without replacement
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
