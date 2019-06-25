library(ggplot2)
library(tidyverse)

setwd("C:/Users/wwoel/Dropbox/FCR_TimeSeries")


ctd_surf <- read.csv("CTD/FCR_CTD_50surf.csv")
#convert date columns into date format
ctd_surf$Date <- as.Date(ctd_surf$Date, "%Y-%m-%d")


# format dataset into long format for ease of plotting
# gather(NewColumForNames, NewColforValues, ColumnsToCollapse)
long <- ctd_surf %>% gather(Variable, Value, Temp_C:SpCond_calc)

# create a theme to standardize some formatting elements
mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank())  
 

# plot 2013 data only 
ggplot(subset(long, Variable %in% c('Cond_uScm', 'SpCond_calc', "Temp_C")), 
       aes(x = Date, y = Value, col = Variable)) +
  geom_point() + mytheme + theme_bw() +
  scale_y_continuous(limits=c(0,60)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y", limits = as.Date(c('2013-01-01', '2014-04-28'))) +
  ggtitle("Conductivity and Specific Conductance \n at Falling Creek Reservoir, 2013")  + 
  theme(plot.title = element_text(hjust = 0.2)) + 
  ylab("uScm  or  °C") +
  scale_color_manual("", values=c('gray35', 'gray69', 'grey1'), 
                     labels=c('Conductivity', 'Specific Conductance','Temperature'), guide='legend')



# all 5 years
ggplot(subset(long, Variable %in% c('Cond_uScm', 'SpCond_calc', 'Temp_C')), 
       aes(x = Date, y = Value, col = Variable)) +
  geom_point() +  mytheme +
  scale_y_continuous(limits=c(0,60)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c('2013-01-01', '2017-12-31'))) +
  ggtitle("Conductivity and Specific Conductance \n at Falling Creek Reservoir, 2013-2017")  + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0,1.1), legend.justification = c(0,1)) + ylab("Conductance (uScm)") +
  scale_color_manual("", values=c('mediumvioletred', 'darkviolet', 'turquoise4'), labels=c('Sp. Cond.', 'Cond.', 'Temp_C'), guide='legend')


