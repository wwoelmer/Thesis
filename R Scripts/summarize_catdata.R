
library(tidyverse)

download.file('https://github.com/CareyLabVT/SCCData/raw/mia-data/Catwalk.csv','Catwalk.csv')
catheader<-read.csv("Catwalk.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
catdata<-read.csv("Catwalk.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(catdata)<-names(catheader) #combine the names to deal with Campbell logger formatting

catdata$TIMESTAMP<-as.POSIXct(strptime(catdata$TIMESTAMP, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") #get dates aligned
catdata$TIMESTAMP[c(1:cat_timechange-1)]<-with_tz(force_tz(catdata$TIMESTAMP[c(1:cat_timechange-1)],"Etc/GMT+4"), "Etc/GMT+5") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set

for(j in 5:ncol(catdata)){
  catdata[,j]<-as.numeric(levels(catdata[,j]))[catdata[,j]]#need to set all columns to numeric values
}

plot(catdata$TIMESTAMP, catdata$Chla_1)

chl_exo <- catdata %>% select(TIMESTAMP, Chla_1)
chl_exo <- na.omit(chl_exo) # do this to get rid of na's so that when you take summary stats later, it won't drop days with na values


chl_summary <- chl_exo %>%                                                 # start with the raw data
  mutate(Date = as.Date(TIMESTAMP, format="%Y-%m-%d hh:mm:ss")) %>% # create a column of just the date
  select(-TIMESTAMP) %>%                                # drop the datetime column
  group_by(Date) %>%                                           # group by date (for daily statistics)
  summarise_all(c("mean", "median", "max","min"))            # get min, median, max, and sum for all variables each day
  

plot(chl_summary$Date, chl_summary$mean, xlim = c(as.Date('2019-07-15'), as.Date('2019-08-15')), ylim = c(0,80), type = 'l')
points(chl_summary$Date, chl_summary$median, xlim = c(as.Date('2019-07-15'), as.Date('2019-08-15')), col = 'red', type = 'l')
points(chl_summary$Date, chl_summary$max, xlim = c(as.Date('2019-07-15'), as.Date('2019-08-15')), col = 'orange', type = 'l')
points(chl_summary$Date, chl_summary$min, xlim = c(as.Date('2019-07-15'), as.Date('2019-08-15')), col = 'blue', type = 'l')
legend('topright', bty = 'n', lty = c(1,1), c('mean', 'median', 'max', 'min'), col = c('black', 'red', 'orange', 'blue'))
