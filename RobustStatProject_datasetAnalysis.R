rm(list=ls())
setwd("C:\\Users\\lukab\\Documents\\Fakultet\\KU Leuven\\RS\\Project")
set.seed(10001)

taxi_data=read.delim("taxi_data.txt")
taxi_data=taxi_data[taxi_data$VendorID==1,]
taxi_alt_data=taxi_data[taxi_data$trip_distance>0 & taxi_data$total_amount>0,] #without points on the axes

summary(taxi_data)
summary(taxi_alt_data)

#boxplots taxi
library(robustbase)

taxi_boxplot=boxplot(taxi_data$trip_distance, horizontal=TRUE, main="Standard boxplot", xlab="Trip distance", width=1,
                     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
length(taxi_boxplot$out)/length(taxi_data$total_amount)     #share of outliers - no outliers below the lower whisker

#boxplot(taxi_data$trip_distance, horizontal=TRUE, ylim=c(0, 40))

taxi_adjbox=adjbox(taxi_data$trip_distance, horizontal=TRUE, main="Adjusted boxplot", xlab="Trip distance", width=1,
                   cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
length(taxi_adjbox$out)/length(taxi_data$trip_distance)     #share of outliers - no outliers below the lower whisker

#adjbox(taxi_data$trip_distance, horizontal=TRUE, ylim=c(0, 40))


taxi_boxplot=boxplot(taxi_data$total_amount, horizontal=TRUE, main="Standard boxplot", xlab="Total amount", width=1,
                     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
length(taxi_boxplot$out)/length(taxi_data$total_amount)     #share of outliers - no outliers below the lower whisker

#boxplot(taxi_data$total_amount, horizontal=TRUE, ylim=c(0,100))

taxi_adjbox=adjbox(taxi_data$total_amount, horizontal=TRUE, main="Adjusted boxplot", xlab="Total amount", width=1,
                   cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
length(taxi_adjbox$out)/length(taxi_data$total_amount)     #share of outliers
length(taxi_adjbox$out[taxi_adjbox$out>taxi_adjbox$stats[5]])/length(taxi_data$total_amount)
#share of outliers above the upper whisker out of all data points

#adjbox(taxi_data$total_amount, horizontal=TRUE, ylim=c(0,100))


#boxplots taxi_alt

taxi_boxplot=boxplot(taxi_alt_data$trip_distance, horizontal=TRUE,main="Standard boxplot", xlab="Trip distance", width=1,
                     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
length(taxi_boxplot$out)/length(taxi_alt_data$total_amount)     #share of outliers - no outliers below the lower whisker

#boxplot(taxi_alt_data$trip_distance, horizontal=TRUE, ylim=c(0, 40))

taxi_adjbox=adjbox(taxi_alt_data$trip_distance, horizontal=TRUE, main="Adjusted boxplot", xlab="Trip distance", width=1,
                   cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
length(taxi_adjbox$out)/length(taxi_alt_data$trip_distance)     #share of outliers
length(taxi_adjbox$out[taxi_adjbox$out>taxi_adjbox$stats[5]])/length(taxi_alt_data$trip_distance)
#share of outliers above the upper whisker out of all data points

#adjbox(taxi_alt_data$trip_distance, horizontal=TRUE, ylim=c(0, 40))

taxi_boxplot=boxplot(taxi_alt_data$total_amount, horizontal=TRUE, main="Standard boxplot", xlab="Total amount", width=1,
                     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
length(taxi_boxplot$out)/length(taxi_alt_data$total_amount)     #share of outliers - no outliers below the lower whisker

#boxplot(taxi_alt_data$total_amount, horizontal=TRUE, ylim=c(0,100))

taxi_adjbox=adjbox(taxi_alt_data$total_amount, horizontal=TRUE, main="Adjusted boxplot", xlab="Total amount", width=1,
                   cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
length(taxi_adjbox$out)/length(taxi_alt_data$total_amount)     #share of outliers
length(taxi_adjbox$out[taxi_adjbox$out>taxi_adjbox$stats[5]])/length(taxi_alt_data$total_amount)
#share of outliers above the upper whisker out of all data points

#adjbox(taxi_alt_data$total_amount, horizontal=TRUE, ylim=c(0,100))


#kernel density estimation

plot(density(taxi_data$trip_distance, bw="nrd0", kernel="epanechnikov"), xlim=c(0, 20), xlab="Trip distance",
     main="Trip distance", cex.main=1.25, cex.lab=1.25, cex.axis=1.25, lwd=2)
plot(density(taxi_data$total_amount, bw="nrd0", kernel="epanechnikov"), xlim=c(0, 80), xlab="Total amount",
     main="Total amount", cex.main=1.25, cex.lab=1.25, cex.axis=1.25, lwd=2)
plot(density(taxi_alt_data$trip_distance, bw="nrd0", kernel="epanechnikov"), xlim=c(0, 20))
plot(density(taxi_alt_data$total_amount, bw="nrd0", kernel="epanechnikov"), xlim=c(0, 100))

##variants with boundary kernel estimation
library(evmix)

seq1=seq(0, 20, length.out=1000)
seq2=seq(0, 100, length.out=1000)
plot(seq1, dbckden(seq1, taxi_data$trip_distance, 
                   bw=bw.nrd0(taxi_data$trip_distance), kernel="epanechnikov", bcmethod="simple"), type="l", ylim=c(0,1.5),
     xlim=c(0, 20), xlab="Trip distance", ylab="Density", main="Trip distance", cex.main=1.5, cex.lab=1.5, cex.axis=1.5, 
     lwd=2)
plot(seq2, dbckden(seq2, taxi_data$total_amount, 
                   bw=bw.nrd0(taxi_data$total_amount), kernel="epanechnikov", bcmethod="simple"), type="l",
     xlim=c(0, 80), xlab="Total amount", ylab="Density", main="Total amount", cex.main=1.5, cex.lab=1.5, cex.axis=1.5, lwd=2)
plot(seq1, dbckden(seq1, taxi_alt_data$trip_distance, 
                   bw=bw.nrd0(taxi_alt_data$trip_distance), kernel="epanechnikov", bcmethod="simple"), type="l",
     xlim=c(0, 20), xlab="Trip distance", ylab="Density", main="Trip distance", cex.main=1.5, cex.lab=1.5, cex.axis=1.5, 
     lwd=2)
plot(seq2, dbckden(seq2, taxi_alt_data$total_amount, 
                   bw=bw.nrd0(taxi_alt_data$total_amount), kernel="epanechnikov", bcmethod="simple"), type="l",
     xlim=c(0, 80), xlab="Total amount", ylab="Density", main="Total amount", cex.main=1.5, cex.lab=1.5, cex.axis=1.5, lwd=2)


###plot against the histogram for illustration, as some of the previous plots look a bit weird or uninformative
hist(taxi_data$trip_distance, breaks=seq(0, 150, 0.25), xlim=c(0, 20), probability = TRUE)
lines(seq1, dbckden(seq1, taxi_data$trip_distance, 
                    bw=bw.nrd0(taxi_data$trip_distance), kernel="epanechnikov", bcmethod="simple"))



#skewness
library(moments)

c(trip_distance=skewness(taxi_data$trip_distance), total_amount=skewness(taxi_data$total_amount))
c(trip_distance=mc(taxi_data$trip_distance), total_amount=mc(taxi_data$total_amount))

c(trip_distance=skewness(taxi_alt_data$trip_distance), total_amount=skewness(taxi_alt_data$total_amount))
c(trip_distance=mc(taxi_alt_data$trip_distance), total_amount=mc(taxi_alt_data$total_amount))


#data plots
library(ggplot2)

plot(taxi_data$trip_distance, taxi_data$total_amount, xlab="Trip distance", ylab="Total amount", main="Taxi dataset",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
plot(taxi_data$trip_distance, taxi_data$total_amount, xlim=c(0, 30), ylim=c(0, 60))
ggplot(taxi_data, aes(trip_distance, total_amount))+
  stat_density_2d(aes(fill=..level..), geom="polygon")+
  theme(text=element_text(size=14), axis.text=element_text(size=14))

plot(taxi_alt_data$trip_distance, taxi_alt_data$total_amount)
plot(taxi_alt_data$trip_distance, taxi_alt_data$total_amount, xlim=c(0, 30), ylim=c(0, 60))
ggplot(taxi_alt_data, aes(trip_distance, total_amount))+
  stat_density_2d(aes(fill=..level..), geom="polygon")+
  theme(text=element_text(size=14), axis.text=element_text(size=14))


#AO, SDO
library(rrcov)
library(mrfDepth)
library(car)

#from now on, we have a single code for analysis of different datasets
#possible options for data_x and data_y:
#1) taxi_data$trip_distance, taxi_data$total_amount
#2) taxi_alt_data$trip_distance, taxi_alt_data$total_amount

data_x=taxi_data$trip_distance #change when necessary
data_y=taxi_data$total_amount  #change when necessary
data=data.frame(x=data_x, y=data_y)

data_sd_1=outlyingness(data)
data_ao=adjOutlyingness(data)
plot(data_sd_1$outlyingnessX, data_ao$adjout, xlab="SDO", ylab="AO", cex.lab=1.5, cex.axis=1.5,
     xlim=c(0, 25), ylim=c(0, 15))
abline(v=data_sd_1$cutoff)
abline(h=data_ao$cutoff)
points(data_sd_1$outlyingnessX[data_sd_1$outlyingnessX<=data_sd_1$cutoff & data_ao$adjout>data_ao$cutoff], 
       data_ao$adjout[data_sd_1$outlyingnessX<=data_sd_1$cutoff & data_ao$adjout>data_ao$cutoff], col=2)
points(data_sd_1$outlyingnessX[data_sd_1$outlyingnessX>data_sd_1$cutoff & data_ao$adjout<=data_ao$cutoff], 
       data_ao$adjout[data_sd_1$outlyingnessX>data_sd_1$cutoff & data_ao$adjout<=data_ao$cutoff], col="springgreen3")
points(data_sd_1$outlyingnessX[data_sd_1$outlyingnessX>data_sd_1$cutoff & data_ao$adjout>data_ao$cutoff], 
       data_ao$adjout[data_sd_1$outlyingnessX>data_sd_1$cutoff & data_ao$adjout>data_ao$cutoff], col=4)

plot(data, xlab="Trip distance", ylab="Total amount", main="Taxi dataset",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
points(data[data_sd_1$outlyingnessX<=data_sd_1$cutoff & data_ao$adjout>data_ao$cutoff,], col=2)
points(data[data_sd_1$outlyingnessX>data_sd_1$cutoff & data_ao$adjout<=data_ao$cutoff,], col="springgreen3")
points(data[data_sd_1$outlyingnessX>data_sd_1$cutoff & data_ao$adjout>data_ao$cutoff,], col=4)

#comparison between estimators

data_sd=CovSde(data, nsamp=100)
data_mcd=CovMcd(data)
data_s=CovSest(data)
data_m=CovMest(data)
data_mm=CovMMest(data)
data_ogk=CovOgk(data)

data_cutoff=sqrt(qchisq(0.975,ncol(data)))
ogk_cutoff=sqrt(qchisq(0.9,ncol(data)))/sqrt(qchisq(0.5,ncol(data)))*median(data_ogk$raw.mah)

plot(data, xlab="Trip distance", ylab="Total amount", main="Taxi dataset",
     cex.lab=1.5, cex.axis=1.5, cex.legend=1.5, cex.main=1.5)
#points(data[data_ao$adjout>data_cutoff,], col=2)
points(data[data_ao$adjout>data_ao$cutoff,], col=4)

ellipse(center=colMeans(data), shape=cov(data), radius=data_cutoff, col=1, center.pch=NA_integer_)
ellipse(center=data_sd$center, shape=data_sd$cov, radius=data_cutoff, col=2, center.pch=NA_integer_)
ellipse(center=data_mcd$center, shape=data_mcd$cov, radius=data_cutoff, col="green", center.pch=NA_integer_)
ellipse(center=data_s$center, shape=data_s$cov, radius=data_cutoff, col=5, center.pch=NA_integer_)
ellipse(center=data_m$center, shape=data_m$cov, radius=data_cutoff, col=6, center.pch=NA_integer_)
ellipse(center=data_mm$center, shape=data_mm$cov, radius=data_cutoff, col="orange", center.pch=NA_integer_)
ellipse(center=data_ogk$center, shape=data_ogk$cov, radius=ogk_cutoff, col=7, center.pch=NA_integer_)
legend("topright",c("Classical", "SD", "MCD", "S", "M", "MM", "OGK"), 
       col = c(1, 2, "green", 5, 6, "orange", 7), lwd=2)


#bagplots
mrfDepth::bagplot(compBagplot(data, type="hdepth"), 
                  databag=FALSE, colorbag="blue", colorloop="lightblue", colorchull="darkblue", plot.fence=TRUE)
mrfDepth::bagplot(compBagplot(data, type="sprojdepth"), 
                  databag=FALSE, colorbag="blue", colorloop="lightblue", colorchull="darkblue", plot.fence=TRUE)