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





#from publication/FullTest.R/moveCovTest
# 
# spp.ctrl = list(
#   "spp.1" = list('nu' = 1/0.015, #inconsistent btwn paper and moveCovTest
#                  'var' = 1,
#                  'scale' = 10, #given as 10 in FullTest.R
#                  'Aniso' = matrix(nc = 2, c(1.5, 3, -3, 4))),
#   "spp.2" = list('nu' = 1/0.05, #inconsistent btwn paper and moveCovTest
#                  'var'  = 2,
#                  'scale' = 20, #given as 20 in FullTest.R
#                  'Aniso' = matrix(nc = 2, c(1, 2, -1, 2))),
#   "spp.3" = list('nu' = 1/0.01, #inconsistent btwn paper and moveCovTest
#                  'var' = 1,
#                  'scale' = 5, #given as 5 in moveCovTest.R
#                  'Aniso' = matrix(nc = 2, c(2.5, 1, -1, 2))),
#   "spp.4" = list('nu' = 1/.005, #inconsistent btwn paper and moveCovTest
#                  'var' = 1,
#                  'scale' = 30, #given as 30 in moveCovTest.R
#                  'Aniso' = matrix(nc = 2, c(0.1, 2, -1, 0.2))),



#Bens edits

#some tests with SW-NE anisotropy

A3 <- matrix(nc=2, c(1, -.5, .5, 1))
A4 <- matrix(nc=2, c(1, -1, 1, 1))
A5 <- matrix(nc=2, c(1, -4, 1, 1))
A6 <- matrix(nc=2, c(1, -4, 4, .5))

#from publication/FullTest.R/moveCovTest
A7<- matrix(nc = 2, c(1.5, 3, -3, 4))
A8<- matrix(nc = 2, c(1, 2, -1, 2))
A9<- matrix(nc = 2, c(2.5, 1, -1, 2))
A10<- matrix(nc = 2, c(0.1, 2, -1, 0.2))
  
#EDITED from publication/FullTest.R/moveCovTest
A11<- matrix(nc = 2, c(1.5, -3, 3, 4))
A12<- matrix(nc = 2, c(1, -2, 1, 2))  #changed to (+,-,+,+)
A13<- matrix(nc = 2, c(2.5, -1, 1, 2))
A14<- matrix(nc = 2, c(0.1, -2, 1, 0.2))


#create models
model3 <- RMmatern(nu=1, Aniso =A3)
model4 <- RMmatern(nu=1, Aniso =A4)
model5 <- RMmatern(nu=1, Aniso =A5)
model6 <- RMmatern(nu=1, Aniso =A6)

#from publication/FullTest.R/moveCovTest
model7 <- RMmatern(nu=1/0.015, Aniso = A7)
model8 <- RMmatern(nu=1/0.05, Aniso = A8)
model9 <- RMmatern(nu=1/0.01, Aniso = A9)
model10 <- RMmatern(nu=1/.005, Aniso = A10)

#EDITED from publication/FullTest.R/moveCovTest
model11 <- RMmatern(nu=1/0.015, Aniso = A11)
model12 <- RMmatern(nu=1/0.05, Aniso = A12)
model13 <- RMmatern(nu=1/0.01, Aniso = A13)
model14 <- RMmatern(nu=1/.005, Aniso = A14)

#simulate data from model
z3<- RFsimulate(model=model3,x,x)
z4<- RFsimulate(model=model4,x,x)
z5<- RFsimulate(model=model5,x,x)
z6<- RFsimulate(model=model6,x,x)

z7<- RFsimulate(model=model7,x,x)
z8<- RFsimulate(model=model8,x,x)
z9<- RFsimulate(model=model9,x,x)
z10<- RFsimulate(model=model10,x,x)

z11<- RFsimulate(model=model11,x,x)
z12<- RFsimulate(model=model12,x,x)
z13<- RFsimulate(model=model13,x,x)
z14<- RFsimulate(model=model14,x,x)


# start a pdf file to save all the plots in one place
pdf(file="testfolder/anisotropy2.pdf")

# compare the strength and directions of the two models
plot(model3, dim=2, xlim=c(-1,1))
text(0.1, 0.88, labels = paste("nu = ", 1), cex = 1)
text(0.1, 0.78, labels = paste("Aniso = ", A3[1],A3[2],A3[3],A3[4]))

plot(model4, dim=2, xlim=c(-1,1))
text(0.1, 0.88, labels = paste("nu = ", 1), cex = 1)
text(0.1, 0.78, labels = paste("Aniso = ", A4[1],A4[2],A4[3],A4[4]))

plot(model5, dim=2, xlim=c(-1,1))
text(0.1, 0.88, labels = paste("nu = ", 1), cex = 1)
text(0.1, 0.78, labels = paste("Aniso = ", A5[1],A5[2],A5[3],A5[4]))

plot(model6, dim=2, xlim=c(-1,1))
text(0.1, 0.88, labels = paste("nu = ", 1), cex = 1)
text(0.1, 0.78, labels = paste("Aniso = ", A6[1],A6[2],A6[3],A6[4]))

plot(model7, dim=2, xlim=c(-1,1))
text(0.1, 0.88, labels = paste("nu = ", 1), cex = 1)
text(0.1, 0.78, labels = paste("Aniso = ", A7[1],A7[2],A7[3],A7[4]))

plot(model8, dim=2, xlim=c(-1,1))
text(0.1, 0.88, labels = paste("nu = ", 1), cex = 1)
text(0.1, 0.78, labels = paste("Aniso = ", A8[1],A8[2],A8[3],A8[4]))

plot(model9, dim=2, xlim=c(-1,1))
text(0.1, 0.88, labels = paste("nu = ", 1), cex = 1)
text(0.1, 0.78, labels = paste("Aniso = ", A9[1],A9[2],A9[3],A9[4]))

plot(model10, dim=2, xlim=c(-1,1))
text(0.1, 0.88, labels = paste("nu = ", 1), cex = 1)
text(0.1, 0.78, labels = paste("Aniso = ", A10[1],A10[2],A10[3],A10[4]))


plot(model11, dim=2, xlim=c(-1,1))
text(0.1, 0.88, labels = paste("nu = ", 1), cex = 1)
text(0.1, 0.78, labels = paste("Aniso = ", A11[1],A11[2],A11[3],A11[4]))

plot(model12, dim=2, xlim=c(-1,1))
text(0.1, 0.88, labels = paste("nu = ", 1), cex = 1)
text(0.1, 0.78, labels = paste("Aniso = ", A12[1],A12[2],A12[3],A12[4]))

plot(model13, dim=2, xlim=c(-1,1))
text(0.1, 0.88, labels = paste("nu = ", 1), cex = 1)
text(0.1, 0.78, labels = paste("Aniso = ", A13[1],A13[2],A13[3],A13[4]))

plot(model14, dim=2, xlim=c(-1,1))
text(0.1, 0.88, labels = paste("nu = ", 1), cex = 1)
text(0.1, 0.78, labels = paste("Aniso = ", A14[1],A14[2],A14[3],A14[4]))

# close pdf
dev.off()

