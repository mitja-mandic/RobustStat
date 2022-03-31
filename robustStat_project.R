library(robustbase)
library(sn)
library(MASS)
library(mrfDepth)
contamination = 0.1
p = 2
n = 200

alpha.2 <- c(10,4)
alpha.5 <- c(10,10,4,4,4)
alpha.10 <- c(10,10,10,10,10,4,4,4,4,4)

omega <- diag(p)
#Function from package sn to generate skewed normal distribution. Added column of 1 to distinguish from 
#added noise later.
generated <- rmsn(n=180, xi=rep(0,length(alpha.2)), omega, alpha.2,  tau=0, dp=NULL)
generated <- cbind(generated,rep(1,180))

#Multivariate normal outliers (10% of 200)
outliers <- mvrnorm(20, mu = rep(-1,length(alpha.2)), Sigma = omega/20)
outliers <- cbind(outliers, rep(0,20))

#added the mixed skewed normal with outliers and shuffled them (not sure if shuffling is necessary though,
#just seemed nicer)
simulated <- rbind(outliers, generated)
simulated <- simulated[sample(1:nrow(simulated)),]

#Calculated adjusted outlyingness and necessary values for boxplot cutoff
adjOutl <- adjOutlyingness(simulated[,1:2])
AO.values <- adjOutl$adjout
AO.q3 <- quantile(AO.values, 0.75)
AO.mc <- mc(AO.values)
AO.IQR <- IQR(AO.values)
AO.cutoff <- AO.q3 + 1.5*exp(3*AO.mc)*AO.IQR

#Searched for an implemented SD funciton - tried this, but got a lot larger values than from AO and not
#sure why. From package mrfDepth
stahelDonoho <- outlyingness(simulated[,1:2])
SD.out.values <- stahelDonoho$outlyingnessX
SD.q3 <- quantile(SD.out.values,0.75)
SD.mc <- mc(SD.out.values)
SD.IQR <- IQR(SD.out.values)
SD.cutoff <- SD.q3 + 1.5*exp(3*SD.mc)*SD.IQR

