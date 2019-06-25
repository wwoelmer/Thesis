#PAR data clean-up
#Author: Mary Lofton
#Date: 14JUL17

#Beer's law
#A = Ebc
setwd("~/PAR/2016") 

#Get data
#This should be a csv with the following format:
#Column 1: Date (I wrote this script using dates in numeric form, FYI!)
#Column 2: Depth (in meters)
#Column 3: PAR (in umol photons m^-2 s^-1)
light <-read.csv("./FCR_PAR_2016.csv")
colnames(light) <- c("Date","Depth","YSI_PAR")
#Limit to only observations with data (in case data is not entered for some days/depths)
library(stats)
light <- na.omit(light)

#Get rid of row with zero (can't take log later)
##Go through each row and determine if a value is zero
row_sub = apply(light, 1, function(row) all(row !=0 ))
##Subset as usual
light <- light[row_sub,]

#Truncate to epilimnion - only do this if your specific task requires it
#Most of the time, you will skip this step
epi<-light[(light$Depth <= 5.5),]
light = epi


#For-loop to plot PAR values by day
#Just to make sure everything looks relatively normal
dates <- unique(light$Date)
d <- c(0:5)
par(mfrow=c(2,2))

for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(light, light$Date == j)
  plot(q$YSI_PAR,q$Depth,
       ylim = rev(range(d)),
       main = j,
       xlab = "Light",
       ylab = "Depth")
                  
  
} 

#For-loop to plot ln of PAR
#Again, just to lay eyes on it

for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(light, light$Date == j)
  plot(log(q$YSI_PAR),q$Depth,
       ylim = rev(range(d)),
       main = j,
       xlab = "Light",
       ylab = "Depth",
       abline(lm(q$Depth ~ log(q$YSI_PAR))))
  
  
} 


#Get slope for Kd
final <- matrix(data=NA, ncol=2, nrow=24)


for (i in 1:length(dates)){
  j=dates[i]
  q <- subset(light, light$Date == j)
  mod <- lm(q$Depth ~ log(q$YSI_PAR))
  slope <- mod$coefficients[2]
  temp <- c(j,slope)
  final[i,] <- temp
  
}

final <- data.frame(final)

#Yay! :)
write.csv(final, "FCR_Kd_2016_22JUL18.csv")

#Plot FCR light attenuation
#Looks like I manually converted the csv of Kd values to a txt file 
#with datetime format of yyyy-mm-dd hh:mm:ss cause I don't have code for it here...?

library(rLakeAnalyzer)
library(tidyverse)
data <- read_csv("./FCR_Kd_2016.csv")
data1 <- read_csv("./FCR_Kd_2016_22JUL18.csv")
plot(data$datetime, data$kd, type = "b", main = "FCR Kd 2016")
points(data1$datetime, data1$kd, type = "b", pch = 3)
#The next three lines create vertical lines on particular days to denote special events of 
#interest, such as EM experiments
abline(v=as.numeric(data$datetime[5]), lwd=1, col='blue')
abline(v=as.numeric(data$datetime[11]), lwd=1, col='blue')
# abline(v=as.numeric(data$datetime[17]), lwd=1, col='blue')


#Plot BVR light attenuation

data2 <- read_csv("./BVR_Kd_2016.csv")
plot(data2$datetime[3:22], data2$kd[3:22], type = "b", main = "BVR Kd 2016")
abline(v=as.numeric(data2$datetime[8]), lwd=1, col='blue')
abline(v=as.numeric(data2$datetime[12]), lwd=1, col='blue')
abline(v=as.numeric(data2$datetime[17]), lwd=1, col='blue')


#Compare the two
plot(data2$datetime[3:16], data2$kd[3:16], type = "l", main = "",
     xlab = "",
     ylab = expression("k  " ~ (m^{-1})),
     ylim = c(0.75,2.55),
     lty = 3,
     lwd = 2,
     xaxt = 'n')
axis.POSIXct(1,at = c(data2$datetime[4],data2$datetime[7],data2$datetime[11],data2$datetime[12],
                      data2$datetime[15]),
             labels = c("12 May", "29 May", "16 Jun", "28 Jun", "14 Jul"))
points(data$datetime[1:16], data$kd[1:16], type = "l", lty = 1, lwd = 2)
abline(v=as.numeric(data2$datetime[8]), lwd=2, col='darkgray')
abline(v=as.numeric(data2$datetime[12]), lwd=2, col='darkgray')
#abline(v=as.numeric(data2$datetime[17]), lwd=1, col='blue')
legend("topright", legend = c("FCR", "BVR"), 
       lty=c(1,3), lwd=c(2,2), bty = "n")



  
 
