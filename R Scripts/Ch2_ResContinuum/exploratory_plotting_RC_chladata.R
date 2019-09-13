# exploratory plotting of RC chl data

library(lubridate)
library(tidyverse)

chl <- read.csv("./Data/chla_extraction/chla_RCD_data.csv")
chl$Date <- as.Date(chl$Date)

ggplot(data = chl, aes(x = Date, y = Chla_ugL )) +facet_wrap(~Site) +geom_line()

ggplot(data = chl, aes(x = Date, y = Chla_ugL, col = Site ))  +geom_line() +facet_wrap(~Reservoir)
ggplot(data = chl[chl$Reservoir=='B',], aes(x = Date, y = Chla_ugL, col = Site ))  +geom_line() + facet_wrap(~Site_type)
ggplot(data = chl[chl$Reservoir=='F',], aes(x = Date, y = Chla_ugL, col = Site ))  +geom_line() + facet_wrap(~Site_type)
ggplot(data = chl, aes(x = Date, y = Chla_ugL, col = Site ))  +geom_line() +facet_wrap(~Site_type)

ggplot(data = chl[chl$Site_type=='S',], aes(x = Date, y = Chla_ugL, col = Site ))  +geom_line() 
# this plot shows F01 increasing over the season, likely a result of a decrease in surface water inputs as the reservoir gets lower and an increase
# in water inputs from the toe drain, so nutrient rich water coming from depth

ggplot(data = chl[chl$Site_type=='R',], aes(x = Date, y = Chla_ugL, col = Site ))  +geom_line() 

chl$Month <- month(chl$Date)
ggplot(data = chl[chl$Distance_from_stream>0,], aes(x = Distance_from_stream, y = Chla_ugL, col = Reservoir ))  +geom_point() +facet_wrap(~Month) + geom_smooth(method = 'lm')
# we start to see a decrease from riverine -> lacustrine only later in the season when discharge is much lower especially in FCR 

ggplot(data = chl[chl$Month=='4'& chl$Distance_from_stream>0,], aes(x = Distance_from_stream, y = Chla_ugL, col = Reservoir ))  + 
  geom_point() +
  facet_wrap(~Month) + 
  geom_smooth(method = 'lm') +
  ggtitle('April')
  
ggplot(data = chl[chl$Month=='5'& chl$Distance_from_stream>0,], aes(x = Distance_from_stream, y = Chla_ugL, col = Reservoir ))  + 
  geom_point() +
  facet_wrap(~Month) + 
  geom_smooth(method = 'lm') +
  ggtitle('May')

ggplot(data = chl[chl$Month=='6'& chl$Distance_from_stream>0,], aes(x = Distance_from_stream, y = Chla_ugL, col = Reservoir ))  + 
  geom_point() +
  facet_wrap(~Month) + 
  geom_smooth(method = 'lm') +
  ggtitle('June')

ggplot(data = chl[chl$Month=='7'& chl$Distance_from_stream>0,], aes(x = Distance_from_stream, y = Chla_ugL, col = Reservoir ))  + 
  geom_point() +
  facet_wrap(~Month) + 
  geom_smooth(method = 'lm') +
  ggtitle('July')

ggplot(data = chl[chl$Month=='8' & chl$Distance_from_stream>0,], aes(x = Distance_from_stream, y = Chla_ugL, col = Reservoir ))  + 
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle('August')

# do some statistical test to see if the different zones are actually different from each other? ...ANOVA??
