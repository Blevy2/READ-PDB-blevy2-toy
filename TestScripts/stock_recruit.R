# stock_recruit.R
# examine how Beverton-Holt stock recruitment curve changes for different values of alpha, beta, and stdev

# Bev-Holt SR curve: R = (alpha * B / (beta + B)) * exp(rnorm*cv)
# R = Recruitment (age 1 fish) in year t
# B = biomass of adult fish in year t-1
# alpha and beta are parameters of the curve
# cv determines the variability around the curve

library(tidyverse)

# values from Dolder et al. (2020) Table 4
a1 <- 6
b1 <- 4
cv1 <- 0.7
a2 <- 27
b2 <- 4
cv2 <- 0.6

# lots of B values to plot lines (TRAC Ref Doc Figure 5)
B <- seq(0, 50, 0.1)
predR1 <- a1 * B / (b1 + B)
predR2 <- a2 * B / (b2 + B)

df <- tibble(plotB = c(B, B),
             plotR = c(predR1, predR2),
             Species = rep(c("1", "2"), each = length(B)))

p1 <- ggplot(df, aes(x=plotB, y=plotR, color=Species)) +
  geom_line() +
  theme_bw()
print(p1)

# what do sampled points look like?
nsamp <- 20
B1 <- sample(50, nsamp)
B2 <- sample(50, nsamp)
Recra1 <- log(a1 * B1 / (b1 + B1)) - (cv1^2)/2 # lognormal bias adjustment
Recrb1 <- log(a2 * B2 / (b2 + B2)) - (cv2^2)/2
Recra2 <- rlnorm(nsamp, mean = Recra1, sdlog = cv1)
Recrb2 <- rlnorm(nsamp, mean = Recrb1, sdlog = cv2)
df2 <- tibble(obsB = c(B1, B2),
              obsR = c(Recra2, Recrb2),
              Species = rep(c("1", "2"), each = nsamp))
p2 <- p1 +
  geom_point(data=df2, aes(x=obsB, y=obsR, color=Species))
print(p2)

# note the lack of units on the axes - unclear how they are defined, could be direct numbers of fish and kg, could be thousands of fish and metric tons, I don't know if the code defines units anywhere

# what would be nice to know is what the unexploited population looks like with no recruitment variability about the SR curve (this is often used to define the end of the SR curve when plotting)
# in non-spatial, age-structured models, can calculate the adult spawners per recruit (SPR) and then see where a line with slope 1/SPR intersects with the SR curve to see the expected equilibrium
# not so easy to do in this spatial model with daily and weekly timesteps and spawning only occurring in some cells and weeks
# perhaps Paul et al. did some testing to derive the values used in the paper (not sure if they have code to look at this already)

