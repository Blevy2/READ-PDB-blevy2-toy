# anisotropy.R
# examples showing how changes in ANISO result in changes in random samples

library(RandomFields)

# setting up 100x100 grid
x <- seq(0, 1, len=100)

# two different Aniso matrices
# first one has strong NW-SE similaries
A1 <- matrix(nc=2, c(1.5, 3, -3, 4))
# second one has weak SW-NE similaries
A2 <- matrix(nc=2, c(1, -1.5, 1.5, 2))

# create models for the two cases
model1 <- RMmatern(nu=1, Aniso=A1)
model2 <- RMmatern(nu=1, Aniso=A2)

# simulate data from each model
z1 <- RFsimulate(model=model1, x, x)
z2 <- RFsimulate(model=model2, x, x)

# start a pdf file to save all the plots in one place
pdf(file="testfolder/anisotropy.pdf")

# compare the strength and directions of the two models
plot(model1, dim=2, xlim=c(-1,1))
plot(model2, dim=2, xlim=c(-1,1))

# see how different the random field is from each model
plot(z1)
plot(z2)

# close pdf
dev.off()

