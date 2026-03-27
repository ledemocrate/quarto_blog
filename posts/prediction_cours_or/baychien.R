library(BMS)

data(datafls)
#estimating a standard MC3 chain with 1000 burn-ins and 2000 iterations and uniform model priors
bma1 = bms(datafls,burn=1000, iter=2000, mprior="uniform")

coef(bma1)
summary(bma1)

topmodels.bma(bma1)[,1:3]
image(bma1)
##standard coefficients based on exact likelihoods of the 100 best models:
coef(bma1,exact=TRUE, std.coefs=TRUE) 

density(bma1,reg="LifeExp") # plot posterior density for covariate "English"


