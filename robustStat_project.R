library(robustbase)
library(sn)
library(MASS)
library(mrfDepth)
contamination <- 0.1
p <- 5
n <- 200
iterations <- 50
seed <- 30

alpha.2 <- c(10, 4)
alpha.5 <- c(10, 10, 4, 4, 4)
alpha.10 <- c(10, 10, 10, 10, 10, 4, 4, 4, 4, 4)
distancesOutliers <- seq(0.1,2.0,0.1)
omega <- diag(p)

# percentage of correctly and incorrectly classified examples
correct.SD <- rep(0, length(distancesOutliers))
correct.AO <- rep(0, length(distancesOutliers))
incorrect.SD <- rep(0, length(distancesOutliers))
incorrect.AO <- rep(0, length(distancesOutliers))

for (j in 1:iterations) {
  set.seed(seed + j)
  
  # Function from package sn to generate skewed normal distribution. Added column of 1 to distinguish from
  # added noise later. When changing p, don't forget to change alpha!!
  nRegular <- n * (1 - contamination)
  generated <- rmsn(n = nRegular, xi = rep(0, length(alpha.5)), omega, alpha.5)
  generated <- cbind(generated, rep(0, nRegular))
  
  nOut <- n * contamination
  # Multivariate normal outliers (10% of m)
  # put center at zero and later add distancesOutliers
  outliers.notShifted <- mvrnorm(nOut, mu = rep(0, length(alpha.5)), Sigma = omega / 20)
  
  for (i in 1:length(distancesOutliers)) {
    outliers <- outliers.notShifted - distancesOutliers[i]
    outliers <- cbind(outliers, rep(1, nOut))
    
    # added the mixed skewed normal with outliers and shuffled them (not sure if shuffling is necessary though,
    # just seemed nicer)
    
    simulated <- rbind(outliers, generated)
    simulated <- simulated[sample(1:nrow(simulated)), ]
    
    # Calculated adjusted outlyingness and necessary values for boxplot cutoff
    # IQR constant 0.7478975
    
    AO.values <- adjOutlyingness(simulated[, 1:p], only.outlyingness = TRUE)
    AO.q3 <- quantile(AO.values, 0.75)
    AO.mc <- mc(AO.values)
    AO.IQR <- IQR(AO.values)
    AO.cutoff <- AO.q3 + 1.5 * exp(3 * AO.mc) * AO.IQR
    
    # Searched for an implemented SD funciton - tried this, but got a lot larger values than from AO and not
    # sure why. From package mrfDepth
    
    stahelDonoho <- outlyingness(simulated[, 1:p], z = NULL)
    SD.out.values <- stahelDonoho$outlyingnessX
    SD.q3 <- quantile(SD.out.values, 0.75)
    SD.mc <- mc(SD.out.values)
    SD.IQR <- IQR(SD.out.values)
    SD.cutoff <- SD.q3 + 1.5 * exp(3 * SD.mc) * SD.IQR
    
    outlyingnessTable <- as.data.frame(cbind(AO.values, SD.out.values, simulated[, ncol(simulated)]))
    colnames(outlyingnessTable) <- c("AO", "SD", "outlier")
    
    outlyingnessTable["classified by AO"] <- AO.values > AO.cutoff
    outlyingnessTable["classified by SD"] <- SD.out.values > SD.cutoff
    
    # calculate distance for plots
    outlyingnessTable["distance"] <- sqrt(rowSums(simulated^2))
    
    # correctly detected
    outliersTable <- outlyingnessTable[outlyingnessTable$outlier == 1, ]
    correct.SD[i] <- correct.SD[i] + nrow(outliersTable[outliersTable$`classified by SD` == TRUE, ])/nrow(outliersTable) * 100
    correct.AO[i] <- correct.AO[i] + nrow(outliersTable[outliersTable$`classified by AO` == TRUE, ])/nrow(outliersTable) * 100
    
    # false positives
    nonOutliersTalble <- outlyingnessTable[outlyingnessTable$outlier == 0, ]
    incorrect.SD[i] <- incorrect.SD[i] + nrow(nonOutliersTalble[nonOutliersTalble$`classified by SD` == TRUE, ])/nrow(nonOutliersTalble) * 100
    incorrect.AO[i] <- incorrect.AO[i] + nrow(nonOutliersTalble[nonOutliersTalble$`classified by AO` == TRUE, ])/nrow(nonOutliersTalble) * 100
  } 
}
correct.SD <- correct.SD/iterations
correct.AO <- correct.AO/iterations
incorrect.SD <- incorrect.SD/iterations
incorrect.AO <- incorrect.AO/iterations

# generate correct plot
plot(distancesOutliers, correct.AO, col="red", type="l", xlim=c(0, 2), ylim=c(0, max(c(max(correct.AO), max(correct.SD))))
     , xlab = "Distance", ylab = "Percentage of outliers detected")
lines(distancesOutliers, correct.SD, col="green")
legend(x = "topleft",          # Position
       legend = c("AO", "SD"),  # Legend texts
       lty = c(1, 1),           # Line types
       col = c(2, 3),           # Line colors
       lwd = 2)                 # Line width
# generate incorrect plot
plot(distancesOutliers, incorrect.AO, col="red", type="l", xlim=c(0, 2), ylim=c(0, max(c(max(incorrect.AO), max(incorrect.SD))))
     , xlab = "Distance", ylab = "Percentage of regular points classified as outliers")
lines(distancesOutliers, incorrect.SD, col="green")
legend(x = "topleft",          # Position
       legend = c("AO", "SD"),  # Legend texts
       lty = c(1, 1),           # Line types
       col = c(2, 3),           # Line colors
       lwd = 2)                 # Line width
