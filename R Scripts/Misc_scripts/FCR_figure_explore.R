#data exploration of FCR CTD and water chemistry data 
##looking for patterns in chl a and other variables

library(ggplot2)
library(dplyr)

setwd("C:/Users/wwoel/Dropbox/FCR_TimeSeries")


#pull in datasets (CTD, water chem, and YSI data for surface (0.1m) and bottom (9.0m) layers)
ctd_surf <- read.csv("CTD/FCR_CTD_50surf.csv")
chem_surf <- read.csv("FCR_chem_50surf.csv")
ysi_surf <- read.csv("FCR_YSI_50surf.csv")
ctd_9 <- read.csv("FCR_CTD_50_9m.csv")
chem <- read.csv("FCR_chemistry.csv")

#convert date columns into date format
ctd_surf$Date <- as.Date(ctd_surf$Date, "%Y-%m-%d")
chem_surf$DateTime <- as.Date(chem_surf$DateTime, "%Y-%m-%d")
ysi_surf$DateTime <- as.Date(ysi_surf$DateTime, "%Y-%m-%d")
ctd_9$Date <- as.Date(ctd_9$Date, "%Y-%m-%d")
chem$DateTime <- as.Date(chem$DateTime, "%Y-%m-%d")


#change names of columns for ease of typing
colnames(chem_surf)[3] <- "Date"
colnames(ysi_surf)[3] <- "Date"
colnames(ysi_surf)[9] <- "PAR"
colnames(chem)[3] <- "Date"


######################################################################################################################
############################## CTD data #############################################################################
#try using ggplot2
##first look at important variables over time
##Chla over time
ggplot(ctd_surf) + geom_point(aes(x =Date, y = Chla_ugL), color = "steelblue") +
  ggtitle("Chla at FCR 50, 0.1m")

#add temp to chla
ggplot(ctd_surf) + geom_point(aes(x =Date, y = Temp_C), color = "green") + 
  geom_point(aes(x =Date, y = Chla_ugL), color = "steelblue") +
  labs(title = "Time Series at FCR", x = "Date", y = "Temp in C") +
  theme(legend.position = "top")

#turbidity over time
ggplot(ctd_surf) + geom_point(aes(x =Date, y = Turb_NTU), color = "pink")
#subsetted using ylim to look at data without crazy outliers and add chla
ggplot(ctd_surf) + geom_point(aes(x =Date, y = Turb_NTU), color = "pink")+ ylim(c(0,10)) + 
  geom_point(aes(x =Date, y = Chla_ugL), color = "steelblue")
# let's look at turbidty vs. chla
ggplot(ctd_surf) + geom_smooth(aes(x =Turb_NTU, y = Chla_ugL), color = "coral2", method = "lm") + 
  xlim(c(0,30)) 
turb <- lm(Chla_ugL ~ Turb_NTU, data = ctd_surf)
summary(turb)
plot(turb)

#DO from CTD and YSI over time 
ggplot(ctd_surf) + geom_point(aes(x =Date, y = DO_mgL), color = "purple") 
#DO over time and add chla
ggplot(ctd_surf) + geom_point(aes(x =Date, y = DO_mgL), color = "purple") +
  geom_point(aes(x =Date, y = Chla_ugL), color = "steelblue")
#look at DO from bottom waters (9.0m)
ggplot(ctd_9) + geom_point(aes(x =Date, y = DO_mgL), color = "deeppink4") + ggtitle("DO in mg/l at 9.0m")
#want to look at Do saturation but only have a small dataset from YSI data
ggplot(ysi_surf) + geom_point(aes(x =Date, y = DOSat), color = "green4") + ggtitle("% DO Saturation at 0.1m")



#Conductivity from CTD over time 
ggplot(ctd_surf) + geom_point(aes(x =Date, y = Cond_uScm), color = "red") 
#limit y axis to remove outliers
ggplot(ctd_surf) + geom_point(aes(x =Date, y = Cond_uScm), color = "red") + ylim(c(0,50))
#conductivity and add chla
ggplot(ctd_surf) + geom_point(aes(x =Date, y = Cond_uScm), color = "red") + ylim(c(0,50)) +
geom_point(aes(x =Date, y = Chla_ugL), color = "steelblue")
#Specific conductivity in 2017 only (bc there is no data before 2017)
ggplot(ctd_surf[ctd_surf$Date >= "2017-01-01",]) + 
  geom_point(aes(x =Date, y = Spec_Cond_uScm),color = "dodgerblue") + 
  geom_point(aes(x =Date, y = Cond_uScm), color = "red") +
  ggtitle("SpCond & Cond from CTD in 2017")

# calculate specific conductivity
ctd_surf <- mutate(ctd_surf, SpCond_calc = Cond_uScm/(1+(0.0191*(Temp_C - 25))))
ggplot(ctd_surf) + geom_point(aes(x =Date, y = SpCond_calc), color = "slateblue3") +
  ylim(c(0, 60)) + ggtitle("Caculated Sp Conductivity")
# check calculated sp cond with CTD provided sp cond
ggplot(ctd_surf) + geom_point(aes(x = Spec_Cond_uScm, y = SpCond_calc)) + ylim(c(0,50)) +
  geom_abline(slope = 1, intercept = 0)

#plot specific cond and actual cond
ggplot(ctd_surf)+ geom_point(aes(x =Date, y = SpCond_calc), color = "mediumvioletred") +
  ylim(c(0, 60)) + geom_point(aes(x = Date, y = Cond_uScm), color = "darkviolet") +
  ggtitle("Conductivity and Specific Conductance at Falling Creek Reservoir")  +
  ylab("Conductance (uScm)")

#subset for 3 year time frame


# conductance on left x and specific conductance on right x?
# legend/a different way to write code so that it is "tidy"









#actual cond and temp
ggplot(ctd_surf) + geom_point(aes(x =Date, y = Temp_C), color = "red") +
  ylim(c(0, 60)) + geom_point(aes(x = Date, y = Cond_uScm), color = "darkviolet") +
  ggtitle("Actual Cond and Temp over time")
#specific cond and temp
ggplot(ctd_surf) + geom_point(aes(x =Date, y = Temp_C), color = "red") +
  ylim(c(0, 60)) + geom_point(aes(x = Date, y = SpCond_calc), color = "slateblue3") +
  ggtitle("Specific Cond and Temp over time")

###################################################################################################################
####################### now look at chemistry data over time #####################################################
#total phosphorous
ggplot(chem_surf) + geom_point(aes(x =Date, y = TP_ugL), color = "turquoise4") +
  ggtitle("TP at FCR 50, 0.1m")
#total nitrogen
ggplot(chem_surf) + geom_point(aes(x =Date, y = TN_ugL), color = "indianred3") +
  ggtitle("TN at FCR 50, 0.1m")
#soluble reactive phosphorous 
ggplot(chem_surf) + geom_point(aes(x =Date, y = SRP_ugL), color = "blueviolet") 
#SRP and TP should be related?
ggplot(chem_surf) + geom_point(aes(x =TP_ugL, y = SRP_ugL), color = "slateblue") 
phos <- lm(SRP_ugL ~ TP_ugL, data = chem_surf)
summary(phos)
plot(SRP_ugL ~ TP_ugL, data = chem_surf)
abline(lm(SRP_ugL ~ TP_ugL, data = chem_surf))

#ammonium
ggplot(chem_surf) + geom_point(aes(x =Date, y = NH4_ugL), color = "seagreen3") 
#ammonium without some outliers
ggplot(chem_surf) + geom_point(aes(x =Date, y = NH4_ugL), color = "seagreen3")+ ylim(c(0,50))
#dissolved organic carbon
ggplot(chem_surf) + geom_point(aes(x =Date, y = DOC_mgL), color = "orangered4") +
  ggtitle("DOC at FCR 50, 0.1m")

# calculate TN:TP and plot
chem_surf <- mutate(chem_surf, TN_TP = TN_ugL/TP_ugL)
ggplot(chem_surf) + geom_point(aes(x =Date, y = TN_TP), color = "coral3") +
  ggtitle("TN:TP Ratio, FCR 50 0.1m")

# calculate ammonium + nitrate:SRP and plot
chem_surf <- mutate(chem_surf, NH4NO3_SRP = (NH4_ugL + NO3NO2_ugL)/SRP_ugL)
ggplot(chem_surf) + geom_point(aes(x =Date, y = NH4NO3_SRP), color = "mediumvioletred")+
  ggtitle("Ammonium + Nitrate/SRP")

################ inflow chemistry  ##############################################################################
ggplot(chem[which(chem$Site==100),]) + geom_point(aes(x = Date, y = TP_ugL), color = "sienna2") +
  ggtitle("TP at inflow")
ggplot(chem[which(chem$Site==100),]) + geom_point(aes(x = Date, y = TN_ugL), color = "darkseagreen4") +
  ggtitle("TN at inflow")
ggplot(chem[which(chem$Site==100),]) + geom_point(aes(x = Date, y = DOC_mgL), color = "tan4") +
  ggtitle("DOC at inflow")

#####################################################################################################################
################################## now look at YSI data over time ################################################
#PAR
ggplot(ysi_surf) + geom_point(aes(x =Date, y = PAR), color = "olivedrab3")
ggplot(ysi_surf) + geom_point(aes(x =Date, y = pH), color = "magenta")


##########################################################################################################################
### now I want to look at chla as a function of various variables

# Temp vs Chla
ggplot(ctd_surf) + geom_point(aes(x = Temp_C, y = Chla_ugL), color = "black")


#DO vs Chla
ggplot(ctd_surf) + geom_point(aes(x = DO_mgL, y = Chla_ugL), color = "black") + ylim(c(0,15))

