### create figures of changes in chla at the surface (1m) ~2weeks pre/post turnover for FCR historically (2013-2017)
# use DO sonde data



library(tidyverse)
library(ggplot2)
setwd("C:/Users/wwoel/Dropbox/FCR_TimeSeries")

#use the DO sonde data to find the exact time of turnover (2016-2017 only)
data <- read.csv("FCR_DOsonde_2012to2017.csv") #ctd data for FCR at site 50, processed for layers
data <- data %>% select("Date", "DO.Temperature..C.",  "DO1m.Temperature..C." )
colnames(data) <- c("Date", "Temp_8m", "Temp_1m")

# now determine when turnover is each year
turn <- ifelse(abs(data$Temp_1m - data$Temp_8m)<= 1, 1, 0)

data[4] <- turn
colnames(data)[4] <- "turnover"
data <- data[!is.na(data$Temp_1m),]

# for 2013-2015, look at CTD data to find turnover
ctd <- read.csv("C:/Users/wwoel/Dropbox/FCR_TimeSeries/CTD/FCR_CTD_50_binned.csv")
ctd <- ctd %>% select("Date", "Depth_m", "Temp_C")
ctd_wide <- spread(ctd, Depth_m, Temp_C)
ctd_wide <- ctd_wide %>% select("Date", "0.8", "8")
colnames(ctd_wide) <- c("Date", "Temp_0.8", "Temp_8")
turn <- ifelse(abs(ctd_wide$Temp_0.8 - ctd_wide$Temp_8)<= 1, 1, 0)
ctd_wide[4] <- turn

# turnover = <1 degree difference between 1m and 8m
# Turnover dates:
# 2013-10-30 (between Oct 23 and Oct 30)
# 2014-10-23 (between Oct 15 and Oct 23)
# 2015-10-05 (between Oct 2 and Oct 5)
# 2016-10-07 11:00:00 (according to CTD oct 11, we would have over estimated if using CTD only!)
# 2017-10-25 05:15:00
## before turnover, there is a long period of time where the temperature difference hovers around <1 degree different, sometime
## going within that range for a short amount of time and then back to a larger than 1 degree difference. I chose a time where
## the difference of <1 degree was sustained. 


###############################################################################################################################################
# make figures of chla 2 weeks before and after turnover each year  #########################################################################
############################################################################################################################################
ctd <- read.csv("C:/Users/wwoel/Dropbox/FCR_TimeSeries/CTD/FCR_CTD_50_binned.csv")
ctd$Date <- as.Date(ctd$Date)
# 2013 (10-30) 10-15 to 11-14
thirteen <- ctd[ctd$Date > "2013-10-15" & ctd$Date < "2013-11-20" & ctd$Depth_m==0.8,]
plot(thirteen$Date, thirteen$Chla_ugL, ylab = "Chlorophyll a (ug/L)", xlab = "Date", ylim = c(0,14))
title("Chlorophyll a Concentrations \nbefore and after turnover, 2013")
abline(v = as.Date("2013-10-30"))

# 2014 (10-23) 10-09 to 11-07
fourteen <- ctd[ctd$Date > "2014-10-01" & ctd$Date < "2014-12-21" & ctd$Depth_m==0.8,]
plot(fourteen$Date, fourteen$Chla_ugL, ylab = "Chlorophyll a (ug/L)", xlab = "Date", ylim = c(0,14))
title("Chlorophyll a Concentrations \nbefore and after turnover, 2014")
abline(v = as.Date("2014-10-23"))


# 2015 (10-05) 09-21 to 10-19
fifteen <- ctd[ctd$Date > "2015-09-20" & ctd$Date < "2015-10-21" & ctd$Depth_m==0.8,]
plot(fifteen$Date, fifteen$Chla_ugL, ylab = "Chlorophyll a (ug/L)", xlab = "Date", ylim = c(0,14))
title("Chlorophyll a Concentrations \nbefore and after turnover, 2015")
abline(v = as.Date("2015-10-05"))

# 2016 (10-07) 09-23 to 10-21
sixteen <- ctd[ctd$Date > "2016-09-23" & ctd$Date < "2016-10-21" & ctd$Depth_m==0.8,]
plot(sixteen$Date, sixteen$Chla_ugL, ylab = "Chlorophyll a (ug/L)", xlab = "Date", ylim = c(0,14))
title("Chlorophyll a Concentrations \nbefore and after turnover, 2016")
abline(v = as.Date("2016-10-07"))

#2017 (10-25) 10-11 to 11-08
#### no chlorophyll data in CTD
master <- read.csv("FCR_Master_2013_2017.csv")
master$Date <- as.Date(master$Date)
seventeen <- master[master$Date > "2017-10-11" & master$Date < "2017-11-08" & master$Depth==0.8,]
plot(seventeen$Date, seventeen$Total_chlorophyll, ylab = "Chlorophyll a (ug/L)", xlab = "Date")
title("Chlorophyll a Concentrations \nbefore and after turnover, 2017")
abline(v = as.Date("2017-10-25"))

plot(seventeen$Date, seventeen$Chla_ugL, ylab = "Chlorophyll a (ug/L)", xlab = "Date")


###########################################################################################################################################
######### plot temperature and light ~2 weeks before and after turnover ############################################################################
master <- read.csv("FCR_Master_2013_2017.csv")
master$Date <- as.Date(master$Date)

ctd <- read.csv("C:/Users/wwoel/Dropbox/FCR_TimeSeries/CTD/FCR_CTD_50_binned.csv")
ctd$Date <- as.Date(ctd$Date)

############################## 2013 (10-30) 10-15 to 11-14 #################################################################################
thirteen <- ctd[ctd$Date > "2013-10-15" & ctd$Date < "2013-11-20" & ctd$Depth_m==0.8,]
par(mar = c(5,5,3,5))
plot(thirteen$Date, thirteen$Temp_C, ylab = "Temperature (C), Chlorophyll (ug/L)", xlab = "Date", type = 'l',
     ylim = c(0,25), xlim = c(as.Date("2013-10-15"), as.Date("2013-11-07")))
points(thirteen$Date, thirteen$Chla_ugL, col = "green", type ='l')
par(new = TRUE)
plot(thirteen$Date, thirteen$Turb_NTU, col = "brown", type = 'l', xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", ylim = c(1,6), xlim = c(as.Date("2013-10-15"), as.Date("2013-11-07")))
axis(side = 4)
mtext("Turbidity (NTU)", side = 4, line = 2)
title("Chlorophyll a Concentrations \nbefore and after turnover, 2013")
abline(v = as.Date("2013-10-30"))
legend('topright', c('Temp', 'Chl', 'Turb'), col = c('black', 'green', 'brown'), bty = 'n', lty = c(1,1), cex = 0.75)



###################################### 2014 (10-23) 10-09 to 11-07  ###########################################################
fourteen <- ctd[ctd$Date > "2014-10-01" & ctd$Date < "2014-12-21" & ctd$Depth_m==0.8,]
par(mar = c(5,5,3,5))
plot(fourteen$Date, fourteen$Temp_C, ylab = "Temperature (C), Chlorophyll (ug/L)", xlab = "Date", 
     type = 'l', ylim = c(0,25), xlim = c(as.Date("2014-10-01"), as.Date("2014-12-10")))
points(fourteen$Date, fourteen$Chla_ugL, col = "green", type = 'l')
par(new = TRUE)
plot(fourteen$Date, fourteen$Turb_NTU, col = "brown", type = 'l', xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", ylim = c(1,6), xlim = c(as.Date("2014-10-01"), as.Date("2014-12-10")))
axis(side = 4)
mtext("Turbidity (NTU)", side = 4, line = 2)
title("Chlorophyll a Concentrations \nbefore and after turnover, 2014")
abline(v = as.Date("2014-10-23"))
legend('topright', c('Temp', 'Chl', 'Turb'), col = c('black', 'green', 'brown'), bty = 'n', lty = c(1,1), cex = 0.75)



########################### 2015 (10-05) 09-21 to 10-19  ###################################################################
fifteen <- ctd[ctd$Date > "2015-09-20" & ctd$Date < "2015-10-21" & ctd$Depth_m==0.8,]
par(mar = c(5,5,3,5))
plot(fifteen$Date, fifteen$Temp_C, ylab = "Temperature (C), Chlorophyll (ug/L)", xlab = "Date", 
     xlim = c(as.Date("2015-09-20"), as.Date("2015-10-21")),
     ylim = c(0,25), type = 'l')
points(fifteen$Date, fifteen$Chla_ugL, col = "green", type = 'l')
par(new = TRUE)
plot(fifteen$Date, fifteen$Turb_NTU, col = "brown", type = 'l', xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", ylim = c(1,6), xlim = c(as.Date("2015-09-20"), as.Date("2015-10-21")))
axis(side = 4)
mtext("Turbidity (NTU)", side = 4, line = 2)
title("Chlorophyll a Concentrations \nbefore and after turnover, 2015")
abline(v = as.Date("2015-10-05"))
legend('topright', c('Temp', 'Chl', 'Turb'), col = c('black', 'green', 'brown'), bty = 'n', lty = c(1,1), cex = 0.75)



######################## 2016 (10-07) 09-23 to 10-21  ###########################################################################
sixteen <- ctd[ctd$Date > "2016-09-23" & ctd$Date < "2016-10-21" & ctd$Depth_m==0.8,]
par(mar = c(5,5,3,5), xpd = FALSE)
plot(sixteen$Date, sixteen$Temp_C, ylab = "Temperature (C), Chlorophyll (ug/L)", xlab = "Date", ylim = c(0,25), 
     xlim =c(as.Date("2016-09-26"), as.Date("2016-10-17")) , type = 'l')
points(sixteen$Date, sixteen$Chla_ugL, col = "green", type = 'l')
par(new = TRUE)
plot(sixteen$Date, sixteen$Turb_NTU, col = "brown", type = 'l', xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", ylim = c(1,6), xlim =c(as.Date("2016-09-26"), as.Date("2016-10-17")))
axis(side = 4)
mtext("Turbidity (NTU)", side = 4, line = 2)
title("Chlorophyll a Concentrations \nbefore and after turnover, 2016")
abline(v = as.Date("2016-10-07"))
legend('right', c('Temp', 'Chl', 'Turb'), col = c('black', 'green', 'brown'), bty = 'n', lty = c(1,1), cex = 0.7)




#2017 (10-25) 10-11 to 11-08
#### no chlorophyll data in CTD
master <- read.csv("FCR_Master_2013_2017.csv")
master$Date <- as.Date(master$Date)
seventeen <- master[master$Date > "2017-10-11" & master$Date < "2017-11-08" & master$Depth==0.8,]
par(mar = c(5,5,5,5))
#plot(seventeen$Date, seventeen$Temp_C, ylab = "Temperature (C), Chlorophyll (ug/L)", xlab = "Date", ylim = c(0,20), type = 'l')
plot(seventeen$Date, seventeen$Total_chlorophyll, col = "green", type = 'l')
par(new = TRUE)
plot(seventeen$Date, seventeen$Turb_NTU, col = "brown", type = 'l', xaxt = "n", yaxt = "n",
     ylab = "", xlab = "")
axis(side = 4)
mtext("Turbidity (NTU)", side = 4, line = 2)
title("Chlorophyll a Concentrations \nbefore and after turnover, 2017")
abline(v = as.Date("2017-10-25"))

plot(seventeen$Date, seventeen$Chla_ugL, ylab = "Chlorophyll a (ug/L)", xlab = "Date")

