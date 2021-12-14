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

# an example showing how SPR and SR can be used to find equilibrium
# a simple population with two ages, recruits and adults
# adults weight 5 times what a recruit weighs
# natural mortality is 0.4
# start with per recruit calculations
# 1 recruit produces exp(-0.4)=0.67032 adults
# multiply adult numbers by weight to derive spawning biomass (S)
# exp(-0.4) * 5 = 3.3516
# so spawners per recruit under F=0 (phi0) = 3.3516 kg

# use the Beverton-Holt stock recruitment function
# R = alpha * S / (beta + S)

# the intersection of BH SR and line from the origin with slope 1/phi0
# can be found by noting that the latter line is R = S/phi0 => S = R*phi0
# replace S with R*phi0 in the SR curve
# R = alpha * R * phi0 / (beta + R * phi0)
# and simplfiy
# beta + R * phi0 = alpha * phi0
# R = alpha - (beta / phi0)
# this value of R is the unfished recruitment R0
# then S0 can be found from R0 * phi0

# what does this look like?
# as an example use
M <- 0.4
wadult <- 5
phi0 <- exp(-M) * wadult
alpha <- 7
beta <- 3
R0 <- alpha - (beta / phi0)
S0 <- R0 * phi0
Svals <- seq(0, S0*1.10, length.out = 1000)
Rvals <- alpha * Svals / (beta + Svals)
philine <- Svals / phi0
df <- tibble(Svals=Svals,
             Rvals=Rvals,
             philine=philine)
p1 <- ggplot(df, aes(x=Svals, y=Rvals)) +
  geom_line() +
  geom_line(aes(x=Svals, y=philine), color="blue") +
  ggtitle(paste0("(S0, R0) = (", round(S0,2), ", ", round(R0,2),")")) +
  theme_bw()
print(p1)

# does the population actually go to this equilibrium value?
# can check by starting the population away from equilibrium
# and following it for a number of years
nyears <- 5
Spop <- rep(NA, nyears) # placeholder for population biomass
Rpop <- rep(NA, nyears) # placeholder for resulting recruitment
Spop[1] <- 5 # some starting value not equal to S0
Rpop[1] <- alpha * Spop[1] / (beta + Spop[1])
for (i in 2:nyears){
  Spop[i] <- Rpop[i-1] * exp(-M) * wadult
  Rpop[i] <- alpha * Spop[i] / (beta + Spop[i])
}
df2 <- tibble(Spop = Spop,
              Rpop = Rpop,
              year = seq(1, nyears))
p2 <- p1 +
  geom_point(data=df2, aes(x=Spop, y=Rpop, color=year))
print(p2)
# note: the convergence to equilbirium is fast because this example
# uses only two ages, a more realistic age-strutured model with
# say 20 ages and delayed maturity would take longer to reach equilibrium


# next the steepness (h) version of the Beverton-Holt equation
# steepness is how sharply the curve bends
#   0.2 is a staight line from the origin to (S0, R0)
#   1.0 is a line that immediately goes from the origin to R0 at a small S
#   values around 0.6-0.8 are common
# the definition of steepness is the proportion of R0 when S is 1/5 of S0
# h = (R|S=S0/5) / R0 = ((alpha * (S0/5)) / (beta + (S0/5))) / R0
# for the example above
h = ((alpha * (S0/5)) / (beta + (S0/5))) / R0
h # 0.661602

# the Beverton-Holt equation can then be written using h, R0, and phi0
# R = 4 * h * R0 * S / (phi0 * R0 * (1-h) + (5*h - 1) * S)
Rvals2 <- 4 * h * R0 * Svals / (phi0 * R0 * (1-h) + (5*h - 1) * Svals)
max(Rvals - Rvals) # shows Rvals2 and Rvals are the same

# many have adopted the steepness form of the BH SR curve because
# the parameters R0 and phi are less correlated than alpha and beta
# (note that phi0 can be calculated from known values and is not a
# parameter in the fitting of the curve)
# the standard alpha and beta parameters can be found from the fitted
# h and R0 (and known phi0) using some simple algebra
# alpha = (4 * h * R0) / (5 * h - 1)
# beta = (phi0 * R0 * (1 - h)) / (5 * h - 1)
alphacalc <- (4 * h * R0) / (5 * h - 1)
betacalc <- (phi0 * R0 * (1 - h)) / (5 * h - 1)
c(alpha, alphacalc) 
c(beta, betacalc)
# occasionally get minor (e-12) diffs due to computer precision

