library(tidyverse)

nuts <- read.csv("./DataAnalysis/Data/water_chemistry/FCR_chemistry.csv")
nutsall <- read.csv("./DataAnalysis/Data/water_chemistry/chemistry.csv")

hist(nuts$Site)
unique(nuts$Site)
nuts$DateTime <- as.Date(nuts$DateTime)
nuts$Site <- as.factor(nuts$Site)
nuts <- nuts[nuts$DateTime>"2017-01-01",]
# ok we only have data at one upstream site, 20
ggplot(data = nuts[nuts$Depth_m<1.0& nuts$Site==c(20,50),], aes(x = Site, y = TP_ugL)) + geom_boxplot()
ggplot(data = nuts[nuts$Depth_m<1.0 & nuts$Site==c(20,50),], aes(x = Site, y = TN_ugL)) + geom_boxplot()
ggplot(data = nuts[nuts$Depth_m<1.0 & nuts$Site==c(20,50),], aes(x = Site, y = NH4_ugL)) + geom_boxplot()
ggplot(data = nuts[nuts$Depth_m<1.0& nuts$Site==c(20,50),], aes(x = Site, y = NO3NO2_ugL)) + geom_boxplot()
ggplot(data = nuts[nuts$Depth_m<1.0 & nuts$Site==c(20,50),], aes(x = Site, y = SRP_ugL)) + geom_boxplot()

ggplot(data = nuts[nuts$Depth_m<1.0 & nuts$Site==c(20,50),], aes(x = Site, y = TP_ugL)) + geom_line()

# what about on a given day?
ggplot(data = nuts[nuts$Depth_m<1.0 & nuts$Site==c(20,50),], aes(x = DateTime, y = TP_ugL, col = Site)) + geom_line()
ggplot(data = nuts[nuts$Depth_m<1.0 & nuts$Site==c(20,50),], aes(x = DateTime, y = TN_ugL, col = Site)) + geom_line()
ggplot(data = nuts[nuts$Depth_m<1.0 & nuts$Site==c(20,50),], aes(x = DateTime, y = NH4_ugL, col = Site)) + geom_line()
ggplot(data = nuts[nuts$Depth_m<1.0 & nuts$Site==c(20,50),], aes(x = DateTime, y = NO3NO2_ugL, col = Site)) + geom_line()
ggplot(data = nuts[nuts$Depth_m<1.0 & nuts$Site==c(20,50),], aes(x = DateTime, y = SRP_ugL, col = Site)) + geom_line()
# SRP is the only nutrient that is more variable at site 20 than site 50
ggplot(data = nuts[nuts$Depth_m<1.0 & nuts$Site==c(20,50),], aes(x = DateTime, y = DOC_mgL, col = Site)) + geom_line()


chl <- read.csv("./DataAnalysis/Data/Fluoroprobe/FluoroProbe.csv")
chl <- chl[chl$Reservoir=="FCR",]
chl <- chl[chl$Depth_m<0.3,]
chl$Site <- as.factor(chl$Site)
chl$DateTime <- as.Date(chl$DateTime)
chl <- chl[chl$DateTime>"2017-01-01",]
ggplot(data = chl[chl$Site==c(20,50),], aes(x = Site, y = TotalConc_ugL)) + geom_boxplot()
ggplot(data = chl, aes(x = DateTime, y = TotalConc_ugL, col = Site)) + geom_line()
