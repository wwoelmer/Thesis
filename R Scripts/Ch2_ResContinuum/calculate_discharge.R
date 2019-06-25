# script to calculate discharge from digitized stream width/depth/velocity measurements
library(tidyverse)

flow <- read.csv("./DataAnalysis/Data/Inflow/2019_Flowmate_Discharge_digitized.csv") # the location of the discharge_digitized.csv, should come from github
flow$Date <- as.Date(flow$Date)

# first convert the depth to m (it is always measured in cm in the field)
flow$Depth_m <- flow$Depth_cm/100

# now convert the velocity to m/s (the flowmeter measures in ft/s)
#flow$Velocity <- as.numeric(flow$Velocity)
flow$Velocity_m.s <- ifelse(flow$Velocity_unit=="ft_s", flow$Velocity*0.3048, flow$Velocity)


# lastly calculate discharge for each interval
flow$Discharge <- flow$Depth_m*flow$Velocity_m.s*flow$WidthInterval_m

# now sum by site and date to get the total discharge for that day/site
flow <-  flow %>% group_by(Site, Date) %>% mutate(Discharge_m3_s = sum(Discharge))


# now subset out only the unique discharge measurements
discharge <- flow %>% select(Date, Site, Discharge_m3_s)
discharge <- discharge[!duplicated(discharge[1:3]),]

wetland <- discharge[discharge$Site=='WET',]
plot(wetland$Date, wetland$Discharge_m3_s)

#write.csv(discharge, './DataAnalysis/Data/Inflow/discharge_calculations_flowmeter.csv', row.names = FALSE)
