# preliminary exploration of RCC nutrient data
library(tidyverse)

nuts <- read.csv('./Data/analytical chemistry/Lachat 2019/chemistry_working_half.csv')
nuts <- nuts[nuts$Depth_m==0.1,]
nuts$Date <- as.Date(nuts$Date)

plot(nuts$Date, nuts$NH4_ppb)
ggplot(data = nuts, aes(x = Date, y = NH4_ppb )) +facet_wrap(~Site) +geom_line()

ggplot(data = nuts, aes(x = Date, y = NH4_ppb, col = Site ))  +geom_line() +facet_wrap(~Reservoir)
ggplot(data = nuts[nuts$Date==as.Date('2019-06-27'),], aes(x = Site, y = NH4_ppb) ) + geom_line()
ggplot(data = nuts[nuts$Date==as.Date('2019-07-18'),], aes(x = Site, y = NH4_ppb) ) + geom_line()
ggplot(data = nuts[nuts$Date==as.Date('2019-05-30'),], aes(x = Site, y = NH4_ppb) ) + geom_line()

ggplot(data = nuts[nuts$Site=='F200',], aes(x = Date, y = NH4_ppb)) + geom_line() 

may <- nuts[nuts$Date=='2019-05-30',]
plot(may$Site, may$NH4_ppb, type = 'p')
points(may$Site, may$NO3NO2_ppb, col = 'orange')
points(may$Site, may$PO4_ppb, col = 'purple')
