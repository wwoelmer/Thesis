# facet plot of all variables
ggplot(long, aes(x = Date, y = Value, col = Variable)) +
  geom_point() +
  facet_wrap(. ~ Variable, scales = "free_y")

#subset for 2013-2015
ggplot(subset(long, Variable %in% c('Cond_uScm', 'SpCond_calc')), 
       aes(x = Date, y = Value, col = Variable)) +
  geom_point() +  mytheme +
  scale_y_continuous(limits=c(0,60)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c('2013-01-01', '2015-12-31'))) +
  ggtitle("Conductivity and Specific Conductance \n at Falling Creek Reservoir, 2013-2015")  + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0,1), legend.justification = c(0,1)) + ylab("Conductance (uScm)") +
  scale_color_manual("", values=c('mediumvioletred', 'darkviolet'), labels=c('Sp. Cond.', 'Cond.'), guide='legend')

# add temp
ggplot(subset(long, Variable %in% c('Cond_uScm', 'SpCond_calc', "Temp_C")), 
       aes(x = Date, y = Value, col = Variable)) +
  geom_point() +  mytheme +
  scale_y_continuous(limits=c(0,60)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c('2013-01-01', '2015-12-31'))) +
  ggtitle("Conductivity and Specific Conductance \n at Falling Creek Reservoir, 2013-2015")  + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0,1.1), legend.justification = c(0,1)) + ylab("Conductance (uScm)") +
  scale_color_manual("", values=c('mediumvioletred', 'darkviolet', 'turquoise4'), labels=c('Sp. Cond.', 'Cond.', 'Temp'), guide='legend')


# 2015-2017
ggplot(subset(long, Variable %in% c('Cond_uScm', 'SpCond_calc', 'Temp_C')), 
       aes(x = Date, y = Value, col = Variable)) +
  geom_point() +  
  mytheme +
  scale_y_continuous(limits=c(0,60)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c('2015-01-01', '2017-12-31'))) +
  ggtitle("Conductivity and Specific Conductance \n at Falling Creek Reservoir, 2015-2017")  + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0,1), legend.justification = c(0,1)) + 
  ylab("Conductance (uScm)") +
  scale_color_manual("", values=c('mediumvioletred', 'darkviolet', 'turquoise4'), labels=c('Sp. Cond.', 'Cond.', 'Temp_C'), guide='legend')





#ggplot(ctd_surf[which(ctd_surf$Date < "2015-12-31"),]) + mytheme +
# geom_point(aes(x =Date, y = SpCond_calc), color = "mediumvioletred") +
#geom_point(aes(x = Date, y = Cond_uScm), color = "darkviolet") +
#  ylim(c(0, 60))  + 
# ggtitle("Conductivity and Specific Conductance \n at Falling Creek Reservoir")  + 
#  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0,1), legend.box.just = c(0,1)) + ylab("Conductance (uScm)") +
#  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c('2013-01-01', '2015-12-31'))) +
#  scale_color_manual("", values=c('mediumvioletred', 'darkviolet'), labels=c('Sp. Cond.', 'Cond.'), guide='legend')

