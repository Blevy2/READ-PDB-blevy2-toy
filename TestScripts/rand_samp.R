# rand_samp.R
# can small sample sizes and many zeros create large CIs?
# note: no measurement error, simply examining variability due to sampling

library(tidyverse)

# population and sample sizes
ntot <- 2500 # 50 by 50 grid, one quarter of general setup
nsamp <- 10

# how to fill population values
propzero <- 0.3
meanlog <- 20
sdlog <- 0.6

# fill population
set.seed(141593) # make repeatable
popzero <- rbinom(ntot, 1, (1-propzero))
poplog <- round(rlnorm(ntot, meanlog = log(meanlog), sdlog = sdlog), 2)
pop <- popzero * poplog
sumpop <- sum(pop) # true population

# sample the population once
onesamp <- sample(pop, nsamp, replace = FALSE)
meansamp <- mean(onesamp)
sdsamp <- sd(onesamp)
cisamp <- c(meansamp - 1.96 * sdsamp, meansamp + 1.96 * sdsamp)
expandedsamp <- meansamp * ntot 
expandedci <- cisamp * ntot 
checkci <- (sumpop >= expandedci[1] & sumpop <= expandedci[2])

# do a bunch of times
nreps <- 1000
mysamp <- rep(NA, nreps)
mysd <- rep(NA, nreps)
for (i in 1:nreps){
  thissamp <- sample(pop, nsamp, replace = FALSE)
  mysamp[i] <- mean(thissamp)
  sdsamp[i] <- sd(thissamp)
}

df <- tibble(sampid = 1:nreps,
             mysamp = mysamp,
             esamp = mysamp * ntot,
             sdsamp = sdsamp,
             ecilow = ntot * (mysamp - 1.96 * sdsamp),
             ecihigh = ntot * (mysamp + 1.96 * sdsamp)
             ) %>%
  mutate(checkci = ifelse(((sumpop >= ecilow) & (sumpop <= ecihigh)), 1, 0))

# how variable are the expanded sample estimates?
ggplot(df, aes(x = 1, y = esamp)) +
  geom_violin() +
  geom_abline(intercept = sumpop, color="blue") +
  theme_bw()  

summary(df$esamp)

# look at all estimates and CIs compared to true pop (blue line)
propwithinci <- 100 * sum(df$checkci) / nreps

ggplot(df, aes(x=sampid, y=esamp)) +
  geom_point() +
  geom_ribbon(aes(ymin=ecilow, ymax=ecihigh), fill="red", alpha=0.3) +
  geom_abline(intercept = sumpop, color="blue") +
  ggtitle(paste0(propwithinci, "% of samples had true sumpop within CI")) +
  theme_bw()+
  ylim(-2.5e5,2.5e5)

# the two plots show the mean estimate is basically unbiases, even with small sample size and lots of zeros, but CIs are large

# possible extensions of this work:
#  try differnt nsamp levels to see how CIs shrink with increased n
#  add measurement error to see how CIs increase
#  add three more strata and compute stratified mean and CI
