### processing summary statistics of met data for FCR over 2013-2017 time series
library(lubridate)
setwd("C:/Users/wwoel/Dropbox/FCR_TimeSeries/MET")
met <- read.csv("FCR_GLM_met_NLDAS2_010113_010118_GMTadjusted.csv")
met$time<-as.character(met$time)
met$time<-as.POSIXct(as.character(met$time),format="%Y-%m-%d %H:%M:%S")
met$time <- date(met$time)


#met$time <- as.Date(met$time, "%Y-%m-%d")

# create a vector of days for the duration of the time series data
days <- seq(as.Date("2013-03-07"), as.Date("2017-12-10"), "day")

#create a matrix with nrow = # of days and ncol = # of summary statistics wanted
met_stat <- matrix(nrow = length(days), ncol = 11)

#name the columns
colnames(met_stat) <- c("Date", "AirTemp_max", "AirTemp_median", "AirTemp_mean", "RelHum_max", "RelHum_median", 
                        "RelHum_mean", "WindSpeed_max", "WindSpeed_median", "WindSpeed_mean", "Rain_sum")



x=1
for(w in 1:length(days)){

    temp <- subset(met, met$time==days[w])
    
    met_stat[x, 1] <- as.Date(temp$time[1])
    met_stat[x, 2] <- max(temp$AirTemp)
    met_stat[x, 3] <- median(temp$AirTemp)
    met_stat[x, 4] <- mean(temp$AirTemp)
    met_stat[x, 5] <- max(temp$RelHum)
    met_stat[x, 6] <- median(temp$RelHum)
    met_stat[x, 7] <- mean(temp$RelHum)
    met_stat[x, 8] <- max(temp$WindSpeed)
    met_stat[x, 9] <- median(temp$WindSpeed)
    met_stat[x, 10] <- mean(temp$WindSpeed)
    met_stat[x, 11] <-sum(temp$Rain)
  

  x=x+1
}


