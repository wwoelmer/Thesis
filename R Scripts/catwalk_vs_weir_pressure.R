# script to look at pressure data from the catwalk vs from the weir at FCR inflow

library(dplyr)

# need to use the 'cleaned' catwalk data using the EDI catwalk script to get of maint days
catdata <- read.csv("Catwalk_cleanedEDI.csv")

# this dataframe is huge--first some data wrangling
catdata <- na.omit(catdata)
catdata$Date <- as.Date(catdata$DateTime, format = "%Y-%m-%d")
catdata <- catdata[catdata$Date>"2018-08-06",]
catdata$EXO_pressure <- as.numeric(catdata$EXO_pressure)
summary <- catdata %>% group_by(Date) %>% mutate(mean_psi = mean(EXO_pressure))
plot(summary$Date, summary$EXO_pressure, col = 'black')
# a few outliers from Jan maint but ok

# select just the pressure data on a daily time step to compare to the weir
summary <- summary %>% select(Date, EXO_pressure)
summary <- summary %>% distinct(Date, .keep_all = TRUE)

# now read in the newest inflow data that includes 2018
new <- read.csv("C:/Users/wwoel/Dropbox/Inflows/FCR_inflow_2018.csv")
new$Date <- as.Date(new$DateTime, format = "%Y-%m-%d")
new <- new %>% group_by(Date) %>% mutate(mean_psi_inf = mean(Pressure_psi))
new <- new %>% select(Date, mean_psi_inf)
new <- new %>% distinct(Date, .keep_all = TRUE)
inf_presh <- left_join(new, summary)
inf_presh <- inf_presh[inf_presh$Date>"2018-08-06",]
par(mfrow = c(2,2))
plot(inf_presh$EXO_pressure, inf_presh$mean_psi_inf, type = 'l')
plot(inf_presh$Date, inf_presh$EXO_pressure, type = 'l')
plot(inf_presh$Date, inf_presh$mean_psi_inf, type = 'l', col = 'brown')
