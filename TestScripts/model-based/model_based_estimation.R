# model_based_estimation.R
# simple example of using model based estimation for survey data

library(gamm4)
library(dplyr)

# GB yellowtail data from ADIOS (aka StockEff)
# create some variables for use in modeling
mydir <- "TestScripts\\model-based"
myfile <- "ADIOS_SV_172909_GBK_NONE_survey_dist_map_fixed.csv"
dat <- read.csv(file.path(mydir, myfile)) %>%
  mutate(ispos = ifelse(!is.na(CATCH_WT_CAL) & CATCH_WT_CAL > 0, 1, 0),
         catchwt = ifelse(is.na(CATCH_WT_CAL), 0, CATCH_WT_CAL),
         logcatchwt = ifelse(is.na(CATCH_WT_CAL), 0, log(CATCH_WT_CAL)))
names(dat)
head(dat)

# look at just Fall survey in recent years to start
fall <- dat %>%
  filter(SEASON == "FALL", YEAR >= 2000) %>%
  mutate(year = factor(YEAR))
fallpos <- fall %>%
  filter(ispos == 1)

# model as two parts: presence/absence and log catch of positive tows
# have to transform data back to normal scale
# note: binomial family uses logit link and poisson family uses log link
# using just a simple model with year fixed effects and depth as a smooth
# note: gamm4 models take a few minutes to run

# model that estimates presence/absence using logit function
p2 <- gamm4(ispos ~ year + s(DEPTH), family = binomial, data = fall)
p2
plot(p2$gam)
plot(p2$gam$coefficients)
p2$gam$coefficients
logitmeans <- as.vector(p2$gam$coefficients[1] + p2$gam$coefficients[2:20])
propmeans <- exp(logitmeans) / (exp(logitmeans) + 1) # transform to normal scale
plot(2001:2019, propmeans)

# model that estimates log(catch) of positive tows
p3 <- gamm4(catchwt ~ year + s(DEPTH), family = poisson, data = fallpos)
p3
plot(p3$gam)
plot(p3$gam$coefficients)
p3$gam$coefficients
logmeans <- as.vector(p3$gam$coefficients[1] + p3$gam$coefficients[2:20])
posmeans <- exp(logmeans) # transform to normal scale
plot(2001:2019, posmeans)

# multiply the two parts together to get the annual mean values
estmeans <- propmeans * posmeans
plot(2001:2019, estmeans)

# can try estimating models with latitude, longitude or bottom temperature
# the variance is much harder to track through the process
# probably best to use VAST (https://github.com/James-Thorson-NOAA/VAST) for the model based approach because the variance calculations and spatial smoothing are already figured out
