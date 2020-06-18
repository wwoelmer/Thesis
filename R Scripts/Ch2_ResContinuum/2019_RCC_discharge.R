library(tidyverse)

flow <- read.csv('./Data/continuum_discharge/2019_2020_Continuum_Discharge.csv')
flow$Date <- as.Date(flow$Date)

# continuum dates only
#dates <- c(as.Date('2019-04-29'), as.Date('2019-05-30'), as.Date('2019-06-27'), as.Date('2019-07-18'), as.Date('2019-08-22'), as.Date('2019-10-04'))

#rcc <- flow[flow$Date==as.Date('2019-04-29') | flow$Date==as.Date('2019-05-30') |  flow$Date==as.Date('2019-06-27') | flow$Date==as.Date('2019-07-18') | flow$Date==as.Date('2019-08-22') | flow$Date==as.Date('2019-10-04'),]
              
              
            

ggplot(data = flow, aes(x = Date, y = Flow_cms, col = Reservoir)) + geom_line() + facet_wrap(~Site)
ggplot(data = flow, aes(Flow_cms)) + geom_histogram() + facet_wrap(~Site + Reservoir)

weir <- read.csv('C:/Users/wwoel/Desktop/FLARE_AR_CHLA/ARIMA_working/FCRinflow_postQAQC.csv')
weir$time <- as.Date(weir$time)

hist(weir$FLOW, xlab = 'Discharge m3/s', main = 'Histogram of F100 Discharge 2013-present')

weir_2019 <- weir[weir$time>'2018-12-31',]
hist(weir_2019$FLOW, xlab = 'Discharge m3/s', main = 'Histogram of F100 Discharge 2019-present')

med_flow <- weir[weir$FLOW >0.025 & weir$FLOW < 0.15, ]
plot(med_flow$time, med_flow$FLOW)

par(mfrow = c(2,3))
plot(med_flow$time, med_flow$FLOW, xlim = c(as.Date('2019-01-01'), as.Date('2019-12-31')), main = '2019')
plot(med_flow$time, med_flow$FLOW, xlim = c(as.Date('2018-01-01'), as.Date('2018-12-31')), main = '2018')
plot(med_flow$time, med_flow$FLOW, xlim = c(as.Date('2016-01-01'), as.Date('2016-12-31')), main = '2016')
plot(med_flow$time, med_flow$FLOW, xlim = c(as.Date('2015-01-01'), as.Date('2015-12-31')), main = '2015')
plot(med_flow$time, med_flow$FLOW, xlim = c(as.Date('2014-01-01'), as.Date('2014-12-31')), main = '2014')
plot(weir$time, weir$FLOW)
